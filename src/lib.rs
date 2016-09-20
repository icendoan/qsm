#![feature(slice_patterns)]
#![feature(advanced_slice_patterns)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(stmt_expr_attributes)]
#![feature(question_mark)]
#![feature(plugin)]
// #![plugin(clippy)]

// #![deny(clippy)]

extern crate krust;
use krust::kbindings;
use krust::kbindings::{KData, KOwned, KVal};

extern crate regex;
use regex::Regex;

use std::{ffi, ptr, thread};
use std::str::FromStr;
use std::collections::HashMap;
use std::sync::mpsc;

pub enum ConnectionMessage
{
    Query(Query),
    GetResult,
    Reconnect,
    Close,
}

pub enum ConnectionResponse
{
    StrResult(String),
    KResult(KOwned),
    Error(String),
}

pub struct Connection
{
    pub id: u64,
    pub handle: Option<i32>,
    pub result: Option<KOwned>,
    pub addr: String,
    pub port: i32,
    pub user: Option<String>,
    pub pass: Option<String>,
    pub rx: mpsc::Receiver<ConnectionMessage>,
    pub tx: mpsc::Sender<(u64, ConnectionResponse)>,
}

pub struct ConnectionInfo
{
    pub name: String,
    pub id: u64,
    pub addr: String,
    pub port: i32,
    pub tx: mpsc::Sender<ConnectionMessage>,
    pub last_msg: Option<ConnectionResponse>,
    pub user: Option<String>,
    pub pass: Option<String>,
}

pub struct Query
{
    qtype: QueryType,
    text: String,
    params: Vec<KOwned>,
}

pub enum QueryType
{
    Pretty,
    Raw,
}

pub struct ConnectionBuilder
{
    addr: Option<String>,
    port: Option<i32>,
    user: Option<String>,
    pass: Option<String>,
}

impl ConnectionBuilder
{
    pub fn new() -> ConnectionBuilder
    {
        ConnectionBuilder {
            addr: None,
            port: None,
            user: None,
            pass: None,
        }
    }

    pub fn addr(&mut self, addr: &str) -> &mut Self
    {
        self.addr = Some(addr.to_owned());
        self
    }

    pub fn user(&mut self, user: &str) -> &mut Self
    {
        self.user = Some(user.to_owned());
        self
    }

    pub fn pass(&mut self, pass: &str) -> &mut Self
    {
        self.pass = Some(pass.to_owned());
        self
    }

    pub fn port(&mut self, port: i32) -> &mut Self
    {
        self.port = Some(port);
        self
    }

    /// Returns the itself if there is an error, or missing data
    pub fn finalise(self,
                    tx: mpsc::Sender<(u64, ConnectionResponse)>,
                    rx: mpsc::Receiver<ConnectionMessage>,
                    id: u64)
                    -> Result<Connection, (&'static str, ConnectionBuilder)>
    {
        if self.addr.is_some() && self.port.is_some()
        {
            Ok(Connection {
                addr: self.addr.unwrap(),
                port: self.port.unwrap(),
                user: self.user,
                pass: self.pass,
                tx: tx,
                rx: rx,
                id: id,
                result: None,
                handle: None,
            })
        }
        else
        {
            Err(("Missing information", self))
        }
    }
}

impl Connection
{
    pub fn run(mut self)
    {
        thread::spawn(move || self.listen());
    }

    fn listen(mut self)
    {
        loop
        {
            match self.rx.recv()
            {
                Ok(ConnectionMessage::Query(q)) =>
                {
                    self.send(q);
                },

                Ok(ConnectionMessage::GetResult) => if let Some(kowned) = self.result.clone()
                {
                    if let KVal::String(s) = KVal::new(kowned.0)
                    {
                        self.tx.send((self.id, ConnectionResponse::StrResult(s.to_owned())));
                    }
                    else
                    {
                        self.tx.send((self.id, ConnectionResponse::KResult(kowned)));
                    }
                },
                Ok(ConnectionMessage::Reconnect) =>
                {
                    self.connect();
                },

                Ok(ConnectionMessage::Close) |
                Err(_) =>
                {
                    // also shutdown in case of an error/crash

                    if let Some(h) = self.handle
                    {
                        unsafe {
                            kbindings::kclose(h);
                        }
                    }

                    return;
                },
            }
        }
    }

    fn connect(&mut self) -> Option<i32>
    {
        if let Some(h) = self.handle
        {
            return Some(h);
        }

        let caddr = ffi::CString::new(self.addr.as_bytes()).unwrap();

        let handle = match (&self.user, &self.pass)
        {
            (&Some(ref u), &Some(ref p)) =>
            {
                let mut auth = String::with_capacity(1 + u.len() + p.len());
                auth.push_str(u);
                auth.push(':');
                auth.push_str(p);

                let cauth = ffi::CString::new(auth).unwrap();

                unsafe {
                    kbindings::khpu(caddr.as_ptr(),
                                    self.port,
                                    cauth.as_ptr())
                }
            },

            _ =>
            unsafe { kbindings::khp(caddr.as_ptr(), self.port) },
        };

        if handle < 0
        {
            self.tx.send((self.id, ConnectionResponse::Error("Connection error".to_owned())));
            None
        }
        else
        {
            self.handle = Some(handle);
            Some(handle)
        }
    }

    fn send(&mut self, query: Query)
    {
        let handle = match self.connect()
        {
            Some(h) => h,
            None => return,
        };

        let text = match query.qtype
        {
            QueryType::Pretty =>
            {
                let mut s = ".Q.s ".to_owned();
                s.push_str(&query.text);
                ffi::CString::new(s).unwrap()
            },

            QueryType::Raw =>
            {
                ffi::CString::new(query.text).unwrap()
            },
        };

        let kptr = unsafe {
            kbindings::k(handle,
                         text.as_ptr(),
                         ptr::null() as *const kbindings::K)
        };

        if kptr.is_null()
        {
            self.tx.send((self.id, ConnectionResponse::Error("null response".to_owned())));
        }
        else
        {
            let kowned = unsafe { KOwned(&*kptr) };

            self.result = Some(kowned.clone());

            if let KVal::String(s) = KVal::new(kptr)
            {
                self.tx.send((self.id, ConnectionResponse::StrResult(s.to_owned())));
            }
            else
            {
                self.tx.send((self.id, ConnectionResponse::KResult(kowned)));
            }
        }
    }
}

pub enum Action<'a>
{
    Help,
    Query(Query),
    Copy(&'a str, &'a str, &'a str),
    Rename(&'a str, &'a str),
    New(&'a str, ConnectionBuilder),
    Delete(&'a str),
    List,
    Open(&'a str),
    Save(&'a str),
    Reconnect,
    Switch(&'a str),
    Result,
    Ignore,
    Error(&'a str),
}

pub fn parse(s: &str) -> Action
{
    match s
    {
        s if s.trim().starts_with(":h") => Action::Help,
        s if s.trim().starts_with(":k ") => parse_kquery(&s.trim()[3..]),
        s if s.trim().starts_with(":cp ") => parse_copy(&s.trim()[3..]),
        s if s.trim().starts_with(":mv ") => parse_rename(&s.trim()[3..]),
        s if s.trim().starts_with(":n ") => parse_new(&s.trim()[3..]),
        s if s.trim().starts_with(":d ") => Action::Delete(&s.trim()[3..]),
        s if s.trim().starts_with(":l ") || s.trim().starts_with(":l") => Action::List,
        s if s.trim().starts_with(":o ") => Action::Save(&s.trim()[3..]),
        s if s.trim().starts_with(":o") => Action::Save(".qsm"),
        s if s.trim().starts_with(":w ") => Action::Save(&s.trim()[3..]),
        s if s.trim().starts_with(":w") => Action::Save(".qsm"),
        s if s.trim().starts_with(":r ") => Action::Reconnect,
        s if s.trim().starts_with(":s ") => Action::Switch(&s.trim()[3..]),
        s if s.trim().starts_with(":g") => Action::Result,
        s if s.trim().is_empty() || s.trim().starts_with('/') || s.trim().starts_with(';') => Action::Ignore,
        s => Action::Query(Query {
            qtype: QueryType::Pretty,
            text: s.trim().to_owned(),
            params: Vec::new(),
        }),
    }
}

// returns a connection builder to allow
// the central server to provide the return channel
fn parse_new(s: &str) -> Action
{
    let mut cb = ConnectionBuilder::new();
    let mut name: &str = "";

    for (i, chunk) in s.split(';').enumerate()
    {
        match i
        {
            0 => name = chunk,
            1 =>
            {
                cb.addr(chunk);
            },
            2 =>
            {
                match str::parse(chunk)
                {
                    Ok(p) =>
                    {
                        cb.port(p);
                    },
                    _ => return Action::Error("Malformed connection info"),
                }
            },
            3 =>
            {
                cb.user(chunk);
            },
            4 =>
            {
                cb.pass(chunk);
            },
            _ => return Action::Error("Malformed connection info"),
        }
    }

    Action::New(name, cb)
}

fn parse_copy(s: &str) -> Action
{
    let mut expr: &str = "";
    let mut to: &str = "";
    let mut name: &str = "";

    for (i, chunk) in s.split(';').enumerate()
    {
        match i
        {
            0 => expr = chunk,
            1 => to = chunk.trim(),
            2 => name = chunk.trim(),
            _ => return Action::Error("Malformed copy info"),
        }
    }

    if expr.is_empty() || to.is_empty() || name.is_empty()
    {
        return Action::Error("Missing information");
    }

    Action::Copy(expr, to, name)
}

fn parse_rename(s: &str) -> Action
{
    let mut from: &str = "";
    let mut to: &str = "";

    for (i, chunk) in s.split(';').enumerate()
    {
        match i
        {
            0 => from = chunk,
            1 => to = chunk,
            _ => return Action::Error("Malformed renaming information"),
        }
    }

    if from.is_empty() || to.is_empty()
    {
        return Action::Error("Missing information");
    }

    Action::Rename(from, to)
}

fn parse_kquery(s: &str) -> Action
{
    // todo: extract raw k object specs
    Action::Query(Query {
        qtype: QueryType::Raw,
        text: s.to_owned(),
        params: Vec::new(),
    })
}
