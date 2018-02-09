use std::{fmt, ffi, str};
use std::collections::VecDeque;
use std::sync::mpsc::{Sender, Receiver};

use k::*;
use u::*;

#[derive(Debug, PartialEq, Eq)]
pub enum ServerAction
{
    Connect,
    Disconnect,
    Pop,
    Flush,
    ToggleActive,
    Async(String),
    Sync(String),
    Exit
}

pub fn listen(mut server: Server, rx: Receiver<ServerAction>)
{
    let mut actions = Vec::new();
    loop
    {
        let action = actions.pop()
            .unwrap_or(rx.recv().unwrap());

        match action
        {
            ServerAction::Connect => server.connect(),
            ServerAction::Disconnect => server.disconnect(),
            ServerAction::Exit => return server.disconnect(),
            ServerAction::Flush => server.flush(),
            ServerAction::Pop =>
            {
                
            },
            ServerAction::ToggleActive => server.active = !server.active,
            ServerAction::Async(query) =>
            {
                
            },
            ServerAction::Sync(query) =>
            {
                
            }
        }
    }
}

pub struct Server
{
    pub addr: ffi::CString,
    pub port: i32,
    pub auth: Option<ffi::CString>,
    pub handle: Option<i32>,
    pub active: bool,
    pub buffer: VecDeque<K>
}

impl Server
{
    fn connect(&mut self)
    {
        if let Some(h) = self.handle.take()
        {
            unsafe {
                kclose(h);
            }
        }

        let h = unsafe {
            match self.auth
            {
                Some(ref a) => khpu(self.addr.as_ptr(),
                                    self.port,
                                    a.as_ptr()),
                None => khp(self.addr.as_ptr(), self.port),
            }
        };

        match h
        {
            0 => println!("Bad authentication."),
            -1 => println!("K Failure"),
            -2 => println!("Timeout"),
            h =>
            {
                self.handle = Some(h);
                println!("Connected.");
            },
        }

    }

    fn disconnect(&mut self)
    {
        match self.handle.take()
        {
            Some(h) =>
            {
                unsafe {kclose(h);}
                println!("Disconnected.");
            },
            None => println!("Not connected.")
        }
    }

    fn sync(&mut self, query: String) -> R<K>
    {

        let h = match self.handle
        {
            Some(h) => h,
            None => return Err(Error::WithMsg("Not connected"))
        };

        let q = match ffi::CString::new(query)
        {
            Ok(q) => q,
            _ => return Err(Error::WithMsg("Bad query (String failure)"))
        };
        
        let ptr = unsafe {
            let qptr = q.into_raw() as *const i8;
            let ptr = k(h, qptr, K_VOID);

            if ptr.is_null() {
                return Err(Error::WithMsg("Null response (disconnection)"))
            }

            let _ = ffi::CString::from_raw(qptr as *mut i8);
            ptr
        };

        Ok(K(ptr))
    }

    fn async(&mut self, query: String) -> R<()>
    {
        let h = match self.handle
        {
            Some(h) => h,
            None => return Err(Error::WithMsg("Not connected"))
        };

        let q = match ffi::CString::new(query)
        {
            Ok(q) => q,
            _ => return Err(Error::WithMsg("Bad query (string failure)"))
        };

        unsafe {
            let qptr = q.into_raw();
            let _ = k(-1 * h, qptr as *const i8, K_VOID);
            let _ = ffi::CString::from_raw(qptr);
        }

        Ok(())
    }

    fn flush(&mut self)
    {
        self.buffer.clear();
    }

    fn pop(&mut self) -> R<K>
    {
        self.buffer.pop_front()
            .ok_or(Error::WithMsg("Nothing in queue."))
    }
}

impl fmt::Display for Server
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        let status = if self.handle.is_some()
        {
            "Connected"
        }
        else
        {
            "Disconnected"
        };

        write!(f,
               "{}:{} ({})",
               unsafe { str::from_utf8_unchecked(self.addr.as_bytes()) },
               self.port,
               status)
    }
}

pub struct ServerHandle
{
    pub connected: bool,
    pub addr: String,
    pub port: i32,
    pub auth: Option<String>,
    pub tx: Sender<ServerAction>,
}

impl fmt::Display for ServerHandle
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f,
               "{}:{} ({})",
               unsafe { str::from_utf8_unchecked(self.addr.as_bytes()) },
               self.port,
               if self.connected {
                   "Connected"
               }
               else {
                   "Disconnected"
               })
    }
}

impl ServerHandle
{
    pub fn connect(&mut self) -> R<()>
    {
        self.tx.send(ServerAction::Connect)
            .map_err(|_| Error::WithMsg("Could not send."))
    }

    pub fn disconnect(&mut self) -> R<()>
    {
        self.tx.send(ServerAction::Disconnect)
            .map_err(|_| Error::WithMsg("Could not send."))
    }

    pub fn sync(&mut self, query: String) -> R<()>
    {
        self.tx.send(ServerAction::Sync(query))
            .map_err(|_| Error::WithMsg("Could not send."))
    }

    pub fn async(&mut self, query: String) -> R<()>
    {
        self.tx.send(ServerAction::Async(query))
            .map_err(|_| Error::WithMsg("Could not send."))
    }

    pub fn addr(&self) -> &str
    {
        &self.addr
    }

    pub fn port(&self) -> i32
    {
        self.port
    }

    pub fn auth(&self) -> Option<&str>
    {
        self.auth.as_ref().map(|x| x.as_ref())
    }
}
