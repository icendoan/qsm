#![feature(nll,untagged_unions)]
extern crate libc;
extern crate chrono;
use std::{ffi,sync,thread,fmt,time}; use std::collections::HashMap;
use std::ops::BitOrAssign;
#[allow(unused)] use std::io::{self,Write,Read,BufRead};

mod k;
mod s;
mod p;

use s::{S,SS};

#[derive(Debug)] pub enum Err { IO(io::Error), Fmt(fmt::Error), State(SS, SS), Err(String) }
impl From<io::Error> for Err { fn from(e:io::Error)->Err{ Err::IO(e) } }
impl From<fmt::Error> for Err { fn from(e:fmt::Error)->Err{ Err::Fmt(e) } }

pub type R<T> = Result<T, Err>;

struct C { s: HashMap<String, S>, c: Option<String>, l: sync::mpsc::Receiver<String>, fmt: (usize, usize) }
impl C {
    fn new() -> (C, sync::mpsc::Sender<String>) {
        let (tx, rx) = sync::mpsc::channel();
        (C { s: HashMap::new(), c: None, l: rx, fmt: (25, 80) }, tx)
    }

    fn exec(&mut self) -> CRes {

        let mut res = CRes::Sleep;

        for (k, v) in self.s.iter_mut() {
            match v.exec() {
                Err(Err::Err(e)) => { println!("{}: Error: {}", k, e); res = CRes::Prompt; },
                Err(Err::State(e,r)) => { println!("{}: Expected state of {:?}, found {:?}", k, e, r); res = CRes::Prompt; },
                Err(Err::IO(e)) => { println!("{}: IO Error {}", k, e); res = CRes::Prompt; },
                Err(Err::Fmt(e)) => { println!("{}: Fmt Error {}", k, e); res = CRes::Prompt; },
                Ok(b) if b => res |= CRes::Upd,
                _ => ()
            }
        }

        if let Ok(x) = self.l.try_recv() {
            res = CRes::Prompt;

            match parse(x) {
                Ok(a) => match self.accept(a) {
                    Err(Err::Err(e)) => println!("Error: {}", e),
                    Err(Err::IO(e)) => println!("IO Error: {}", e),
                    Err(Err::Fmt(e)) => println!("Fmt Error: {}", e),
                    Err(Err::State(e, r)) => println!("Expected state of {:?}, found {:?}", e, r),
                    Ok(r) => res |= r,
                },
                Err(Err::Err(e)) => { println!("Parse error: {}", e); return CRes::Prompt; }
                _ => { println!("Unexpected error type"); return CRes::Prompt; }
            }
        }

        if let Some(srv) = self.s.get_mut(self.c.as_ref().map(AsRef::as_ref).unwrap_or("none")) {
            if let Some(k) = srv.resp() {
                let (l, c) = self.fmt;
                p::pr(l,c,k);
                res |= CRes::Prompt;
            }
        }

        res
    }

    fn accept(&mut self, a: Action) -> R<CRes> {
        match a {
            Action::New { name, addr, port, user, pass } => { self.s.insert(name, S::new(&addr, port, &user, &pass)); Ok(CRes::Upd) },
            Action::Del { name: Some(name) } => {
                self.s.remove(&name)
                    .ok_or(Err::Err(format!("No such server {}", name)))
                    .and_then(|mut x| x.disconnect())
                    .map(|_| CRes::Upd)
            }
            Action::Del { name: None } => { if let Some(c) = self.c.take() {
                self.s.remove(&c)
                    .ok_or(Err::Err(format!("No such server {}", c)))
                    .and_then(|mut x| x.disconnect())
                    .map(|_| CRes::Upd)
            } else { Ok(CRes::Upd) } }
            Action::Cnn { name: Some(name) } => {
                if let Some(s) = self.s.get_mut(&name) {
                    s.connect()?; self.c = Some(name); Ok(CRes::Upd) }
                else {
                    Err(Err::Err(format!("No such server exists: {}", name))) } }
            Action::Cnn { name: None } => {
                let c = self.c.as_ref();
                let s = &mut self.s;
                if let Some(s) = c.and_then(|x| s.get_mut(&*x)) {
                    s.connect()
                        .map(|_|CRes::Upd)
                } else {
                    Ok(CRes::Upd)
                }
            },
            Action::Hlp => { Ok(CRes::Upd) },
            Action::Lst => { for (k, v) in &self.s { println!("{}: {}", k, v) } Ok(CRes::Upd) }
            Action::Qry { query } => {
                let c = self.c.as_ref();
                let s = &mut self.s;
                c.and_then(|x| s.get_mut(&*x)).and_then(|s| {s.queue(query); Some(()) }).ok_or(Err::Err("No such server".into()))
                    .map(|_| CRes::Upd)
            },
            Action::Fmt { fmt } => { self.fmt = fmt; Ok(CRes::Upd) },
            Action::Exit => Ok(CRes::Exit),
            Action::NOP => Ok(CRes::Upd)
        }
    }
}

#[derive(Copy, Clone, Debug)] enum CRes { Prompt, Sleep, Upd, Exit }
impl BitOrAssign for CRes {
    fn bitor_assign(&mut self, other: CRes) {
        use CRes::*;
        *self = match (*self, other) {
            (Exit, _) => Exit,
            (_, Exit) => Exit,
            (Prompt, _) => Prompt,
            (_, Prompt) => Prompt,
            (Upd, Sleep) => Upd,
            (Sleep, Upd) => Upd,
            (x, _) => x
        }
    }
}

enum Action {
    New { name: String, addr: String, port: u16, user: String, pass: String },
    Del { name: Option<String> },
    Cnn { name: Option<String> },
    Hlp,
    Lst,
    Qry { query: String },
    Fmt { fmt: (usize, usize) },
    Exit,
    NOP
}

fn parse(x: String) -> R<Action> {

    fn new(x: &str) -> R<Action> {
        let mut iter = x.split_whitespace();
        let name = iter.next().ok_or(Err::Err(format!("Missing parameter: name")))?.into();
        let addr = iter.next().ok_or(Err::Err(format!("Missing parameter: addr")))?.into();
        let port: u16 = iter.next().ok_or(Err::Err(format!("Missing parameter: port")))
            .and_then(|s| s.parse().map_err(|e| Err::Err(format!("u16 parse error: {:?}", e))))?;
        let user = iter.next().map(Into::into).unwrap_or(String::new());
        let pass = iter.next().map(Into::into).unwrap_or(String::new());
        Ok(Action::New { name, addr, port, user, pass })
    }
    fn del(x: &str) -> R<Action> { Ok(Action::Del { name: if x.is_empty() { None } else { Some(x.into()) }}) }
    fn cnn(x: &str) -> R<Action> { Ok(Action::Cnn { name: if x.is_empty() { None } else { Some(x.into()) }}) }
    fn hlp(_: &str) -> R<Action> { Ok(Action::Hlp) }
    fn lst(_: &str) -> R<Action> { Ok(Action::Lst) }
    fn fmt(x: &str) -> R<Action> {
        let mut iter = x.split_whitespace();
        let l = iter.next().ok_or(Err::Err(format!("Missing parameter: lines")))?.parse().map_err(|e| Err::Err(format!("num parse err: {:?}", e)))?;
        let c = iter.next().ok_or(Err::Err(format!("Missing parameter: cols")))?.parse().map_err(|e| Err::Err(format!("num parse err: {:?}", e)))?;;
        Ok(Action::Fmt { fmt: (l, c) })
    }
    fn qry(x: &str) -> R<Action> { Ok(Action::Qry { query: x.into() }) }

    match (&x).trim() {
        x if x.starts_with(":n") => new(&x[2..].trim()),
        x if x.starts_with(":d") => del(&x[2..].trim()),
        x if x.starts_with(":c") => cnn(&x[2..].trim()),
        x if x.starts_with(":h") => hlp(&x[2..].trim()),
        x if x.starts_with(":l") => lst(&x[2..].trim()),
        x if x.starts_with(":fmt") => fmt(&x[4..].trim()),
        x if x == "\\\\" => Ok(Action::Exit),
        x if x.is_empty() => Ok(Action::NOP),
        x => qry(x)
    }
}

fn read(tx: sync::mpsc::Sender<String>) {
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let mut s = String::new();

    loop {
        stdin.read_to_string(&mut s).unwrap();
        //stdin.read_line(&mut s).unwrap();
        tx.send(s).unwrap();
        s = String::new();
    }
}

fn main() {
    let (mut c, tx) = C::new();
    thread::spawn(move || read(tx));
    let o = io::stdout();
    let mut o = o.lock();

    unsafe { k::khp(ffi::CString::default().as_ptr(),-1); }

    write!(o, "{})", c.c.as_ref().map(AsRef::as_ref).unwrap_or("none"));
    o.flush().expect("Cannot flush stdout!");
    loop {
        match c.exec() {
            CRes::Exit => break,
            CRes::Prompt => {
                write!(o, "{})", c.c.as_ref().map(AsRef::as_ref).unwrap_or("none"));
                o.flush().expect("Cannot flush stdout!");
            },
            CRes::Sleep => thread::sleep(time::Duration::from_millis(100)),
            CRes::Upd => ()
        }
    }
}
