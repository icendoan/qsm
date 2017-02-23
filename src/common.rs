use std::{ffi, fmt, io};

#[derive(Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct ServerId(u64);

#[derive(Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct Port(pub u16);

impl fmt::Display for Port
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", self.0)
    }
}

pub type R<T> = Result<T, Error>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Task
{
    K,
    Q,
    Plot,
}

impl fmt::Display for Task
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f,
               "{}",
               match *self
               {
                   Task::K => "Raw",
                   Task::Q => "PrettyPrinted",
                   Task::Plot => "GnuPlot",
               })
    }
}

pub struct Setting
{
    pub msgtype: Task,
    pub server: Option<String>,
}

impl Setting
{
    pub fn new() -> Self
    {
        Setting {
            msgtype: Task::Q,
            server: None,
        }
    }

    pub fn patch(&mut self, other: SettingsBuilder)
    {
        if let Some(m) = other.msgtype
        {
            self.msgtype = m;
        }

        if other.server.is_some()
        {
            self.server = other.server;
        }
    }
}

impl fmt::Display for Setting
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f,
               "Settings:
Message type: {}
Server: {:?}",
               self.msgtype,
               self.server)
    }
}

pub struct SettingsBuilder
{
    msgtype: Option<Task>,
    server: Option<String>,
}

impl SettingsBuilder
{
    pub fn new() -> Self
    {
        SettingsBuilder {
            msgtype: None,
            server: None,
        }
    }

    pub fn msgtype(&mut self, task: Task) -> &mut Self
    {
        self.msgtype = Some(task);
        self
    }

    pub fn server(&mut self, server: String) -> &mut Self
    {
        self.server = Some(server);
        self
    }

    pub fn build(self) -> R<Setting>
    {
        if let SettingsBuilder { msgtype: Some(m), server: s } = self
        {
            Ok(Setting {
                msgtype: m,
                server: s,
            })
        }
        else
        {
            Err(Error::Internal("Incomplete SettingsBuilder"))
        }
    }
    pub fn has_update(&self) -> bool
    {
        self.msgtype.is_some() || self.server.is_some()
    }
}

pub enum Error
{
    IOError(io::Error),
    Timeout,
    NoHeader,
    BadMsg,
    KFailure, // used for failures in kdb code
    Internal(&'static str),
    ParseError(Option<&'static str>),
    WithMsg(&'static str),
    NotConnected,
    Warning(&'static str),
}

impl From<io::Error> for Error
{
    fn from(e: io::Error) -> Error
    {
        Error::IOError(e)
    }
}

impl fmt::Display for Error
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match *self
        {
            Error::IOError(ref e) => write!(f, "{}", e),
            Error::Timeout => write!(f, "Error: Timeout"),
            Error::NoHeader => write!(f, "Error: No Header in Message!"),
            Error::BadMsg => write!(f, "Error: Bad Message!"),
            Error::KFailure => write!(f, "Error: KError"),
            Error::Internal(ref e) => write!(f, "Internal Error: {}", e),
            Error::WithMsg(ref m) => write!(f, "Error: {}", m),
            Error::ParseError(Some(ref msg)) => write!(f, "Parse Error: {}", msg),
            Error::ParseError(None) => write!(f, "Parse Error"),
            Error::NotConnected => write!(f, "Error: Not Connected"),
            Error::Warning(ref s) => write!(f, "Warning: {}", s),
        }
    }
}

pub fn sym_str(x: &*const i8) -> &str
{
    unsafe { ffi::CStr::from_ptr(*x).to_str().unwrap() }
}
