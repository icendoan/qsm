use std::fmt::{self, Display};
use std::net;
use std::collections::VecDeque;
use std::io::{self, Write, Read};

use k;
pub use super::{Err, R};

#[derive(PartialEq, Eq, Copy, Clone, Debug)] pub enum SS { Hsk, Rdy, Pnd, Dc }
pub struct S { pub addr: String, pub port: u16, auth: String, stream: Option<net::TcpStream>, pub resp: VecDeque<k::K>, pub queue: VecDeque<String>, pub state: SS }

macro_rules! assert_state { ($x:expr, $y:expr) => ( if $x != $y.state { return Err(Err::State($x, $y.state)) }) }
macro_rules! stream { ($x:expr) => (
    if let Some(s) = $x.stream.as_mut() {
        s
    } else {
        let state = $x.state;
        $x.state = SS::Dc;
        return Err(Err::Err(format!("In state {:?} with no TcpStream", state)))})}

impl S {
    pub fn new(addr: &str, port: u16, user: &str, pass: &str) -> S {

        let mut auth = String::new();
        auth.push_str(user);
        auth.push(':');
        auth.push_str(pass);
        auth.push(k::KCAP as char);
        auth.push(0 as u8 as char);

        S { addr: addr.into(), port: port, auth: auth.into(), stream: None, resp: VecDeque::new(), queue: VecDeque::new(), state: SS::Dc }
    }

    pub fn connect(&mut self) -> R<()> {
        if self.state != SS::Dc {
            return Ok(())
        }

        let mut stream = net::TcpStream::connect((self.addr.as_ref(), self.port))?;
        stream.set_nonblocking(true)?;
        stream.write(self.auth.as_bytes())?;
        self.stream = Some(stream);
        self.state = SS::Hsk;
        Ok(())
    }

    pub fn disconnect(&mut self) -> R<()> {
        assert_state!(SS::Rdy, self);
        if let Some(stream) = self.stream.take() {
            stream.shutdown(net::Shutdown::Both).map_err(Err::from)?;
            self.state = SS::Dc;
            Ok(())
        } else {
            Err(Err::Err("TcpStream in incorrect state for status".into()))
        }
    }

    pub fn handshake(&mut self) -> R<bool> {
        assert_state!(SS::Hsk, self);
        let mut x = [0u8;8];
        let stream = stream!(self);
        match stream.read(&mut x[..]) {
            Ok(1) => {
                if x[0] == k::KCAP {
                    self.state = SS::Rdy;
                    Ok(true)
                } else {
                    self.state = SS::Dc;
                    stream.shutdown(net::Shutdown::Both)?;
                    self.stream = None;
                    Err(Err::Err(format!("Invalid handshake response: {}", x[0])))
                }
            },

            Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => Ok(false),

            x => {
                self.state = SS::Dc;
                stream.shutdown(net::Shutdown::Both)?;
                self.stream = None;
                Err(Err::Err(format!("Invalid handshake response: {:?}", x)))
            }
        }
    }

    fn send(&mut self) -> R<bool> {
        assert_state!(SS::Rdy, self);
        let stream = stream!(self);
        if let Some(q) = self.queue.pop_front() {
            let qry = unsafe { k::ktn(10, q.as_bytes().len() as i64) };
            k::mtk(qry).copy_from_slice(q.as_bytes());
            let b = unsafe { k::b9(2,qry) };
            k::mtk(b)[0] = 1u8; // force little-endian
            k::mtk(b)[1] = 1u8; // force sync messages
            stream.write(k::tk(b))?;
            self.state = SS::Pnd;
            unsafe {
                k::r0(qry);
                k::r0(b);
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn recv(&mut self) -> R<bool> {
        assert_state!(SS::Pnd, self);
        let stream = stream!(self);
        let mut x = [0u8; 8];

        match stream.read(&mut x) {
            Ok(8) => (),
            Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => return Ok(false),
            Err(e) => return Err(e.into()),
            Ok(0) => {
                self.state = SS::Dc;
                stream.shutdown(net::Shutdown::Both)?;
                self.stream = None;
                return Err(Err::Err(format!("Disconnected")))
            },
            Ok(n) => {
                self.state = SS::Dc;
                stream.shutdown(net::Shutdown::Both)?;
                self.stream = None;
                return Err(Err::Err(format!("Expected 8 bytes for recv header, received {}", n)))
            }
        };

        let mul: [i64;4] = [1,256,65536,16777216];
        let len: i64 = if x[0] == 1 {
            x[4..].iter().zip((mul).iter()).map(|(x,y)|(*x as i64)*(*y)).sum()
        } else {
            x[4..].iter().zip((mul).iter().rev()).map(|(x,y)|(*x as i64)*(*y)).sum()
        };

        let b = unsafe { k::ktn(4, len) };
        k::mtk(b)[0..8].copy_from_slice(&x[..]);
        let mut has_read = 8;
        loop {
            match stream.read(&mut k::mtk(b)[has_read..]) {
                Ok(n) if n + has_read == (len as usize) => break,
                Ok(n) => has_read += n,
                Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => (),
                Err(e) => return Err(e.into())
            }
        }

        let k = unsafe { k::ee(k::d9(b)) };
        unsafe { k::r0(b) };
        self.resp.push_back(k);
        self.state = SS::Rdy;

        Ok(true)
    }

    pub fn exec(&mut self) -> R<bool> {
        match self.state {
            SS::Hsk => self.handshake(),
            SS::Rdy => self.send(),
            SS::Pnd => self.recv(),
            SS::Dc => Ok(false)
        }
    }

    pub fn resp(&mut self) -> Option<k::K> {
        self.resp.pop_front()
    }

    pub fn queue(&mut self, s: String) {
        self.queue.push_back(s)
    }
}

impl Display for S {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{} (pnd: {}, rsp: {}) - {:?}", self.addr, self.port, self.queue.len(), self.resp.len(), self.state)
    }
}
