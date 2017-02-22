use super::*;
use std::{net, io, ffi, env};
use std::os::unix;
use krust;
use krust::kbindings::{self, KOwned, K};

use byteorder::{BigEndian, LittleEndian};

pub enum Connection
{
	Tcp(net::TcpStream),
	Unix(unix::net::UnixStream),
}

impl Connection
{
	pub fn tcp(addr: &str,
	           port: super::Port,
	           user: Option<&str>,
	           pass: Option<&str>)
	           -> R<Connection>
	{
		let t: net::TcpStream = net::TcpStream::connect((addr, port.0))?;
		t.set_nonblocking(true)?;
		let mut c = Connection::Tcp(t);
		c.handshake(user, pass)?;
		Ok(c)
	}

	pub fn unix(path: &str, user: Option<&str>, pass: Option<&str>) -> R<Connection>
	{
		let u: unix::net::UnixStream = unix::net::UnixStream::connect(path)?;
		u.set_nonblocking(true)?;
		let mut c = Connection::Unix(u);
		c.handshake(user, pass)?;
		Ok(c)
	}

	pub fn resp(&mut self) -> R<KOwned>
	{

		let mut header = [0u8; 8];
		match self.read(&mut header[..])
		{
			Ok(8) => (),
			Ok(n) => return Err(Error::NoHeader),
			Err(e) => return Err(Error::from(e)),
		}

		let big_endian = header[0] == 0;

		let mut cursor = io::Cursor::new(&header[4..]);
		let len = if big_endian
			{
				cursor.read_u32::<BigEndian>()
			}
			else
			{
				cursor.read_u32::<LittleEndian>()
			}.map_err(Error::from)?;

		unsafe {
			let kbuf: *const K = kbindings::ktn(4i32, len as i64);
			if kbuf.is_null()
			{
				return Err(Error::KFailure);
			}

			let slice: &mut [u8] = (*kbuf).fetch_slice::<u8>();
			match self.read(slice)
			{
				Ok(n) if n == len as usize => (),
				Ok(n) =>
				{
					kbindings::r0(kbuf); // do not use slice after this point
					return Err(Error::BadMsg);
				},
				Err(e) =>
				{
					kbindings::r0(kbuf);
					return Err(Error::from(e));
				},
			}

			if 0 != kbindings::okx(kbuf)
			{
				kbindings::r0(kbuf);
				return Err(Error::BadMsg);
			}

			let k: *const K = kbindings::b9(3i32, kbuf);
			kbindings::r0(kbuf);
			if k.is_null()
			{
				return Err(Error::KFailure);
			}

			Ok(KOwned(&*k))
		}
	}

	pub fn query(&mut self, query: String) -> R<()>
	{
		unsafe {
			let len = query.len() as i64;
			let cstr = ffi::CString::new(query).map_err(|_| Error::Internal("Bad utf8 in query"))?;
			let kq: *const K = kbindings::kpn(cstr.as_ptr(), len);
			let serialised = kbindings::b9(3, kq as *const K);
			let ser_slice = (*serialised).fetch_slice::<u8>();
			self.write_all(ser_slice).map_err(Error::IOError)
		}
	}

	fn handshake(&mut self, user: Option<&str>, pass: Option<&str>) -> R<()>
	{
		match (user, pass)
		{
			(Some(u), Some(p)) =>
			{
				let mut buf = u.to_owned().into_bytes();
				buf.push(':' as u8);
				buf.extend(p.as_bytes());
				buf.push(3); // don't need >2GiB msgs; can increase if req
				buf.push(0);
				self.write(&buf[..])?;
			},
			_ =>
			{
				let mut buf = Vec::new();
				match env::var("USER")
				{
					Ok(user) => buf.extend(user.as_bytes()),
					Err(_) => return Err(Error::Internal("No username!")),
				}

				buf.push(':' as u8);
				buf.push(3);
				buf.push(0);
				self.write(&buf[..])?;
			},
		}
		let mut cap = [0u8; 1];
		self.read(&mut cap)?;
		if cap[0] != 3
		{
			return Err(Error::KFailure);
		}

		Ok(())
	}
}

impl io::Read for Connection
{
	fn read(&mut self, buf: &mut [u8]) -> Result<usize, io::Error>
	{
		match *self
		{
			Connection::Tcp(ref mut t) => t.read(buf),
			Connection::Unix(ref mut u) => u.read(buf),
		}
	}
}

impl io::Write for Connection
{
	fn write(&mut self, buf: &[u8]) -> Result<usize, io::Error>
	{
		match *self
		{
			Connection::Tcp(ref mut t) => t.write(buf),
			Connection::Unix(ref mut u) => u.write(buf),
		}
	}

	fn flush(&mut self) -> Result<(), io::Error>
	{
		match *self
		{
			Connection::Tcp(ref mut t) => t.flush(),
			Connection::Unix(ref mut u) => u.flush(),
		}
	}
}
