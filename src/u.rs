use std::{io, fmt};
pub type R<T> = Result<T, Error>;

pub enum Error
{
    IOError(io::Error),
    Timeout,
    KFailure, // used for failures in kdb code
    Parse(Option<&'static str>),
    WithMsg(&'static str),
    NotConnected,
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
            Error::KFailure => write!(f, "Error: KError"),
            Error::WithMsg(ref m) => write!(f, "Error: {}", m),
            Error::Parse(Some(ref msg)) => write!(f, "Parse Error: {}", msg),
            Error::Parse(None) => write!(f, "Parse Error"),
            Error::NotConnected => write!(f, "Error: Not Connected"),
        }
    }
}

pub fn err(e: Error)
{
    println!("{}", e);
}

pub trait IterE: Iterator
{
    fn fold1<F: Fn(Self::Item, Self::Item) -> Self::Item>
        (&mut self,
         f: F)
         -> Option<Self::Item>
    {
        let init = if let Some(init) = self.next()
        {
            init
        }
        else
        {
            return None;
        };

        Some(self.fold(init, f))
    }
}

impl<T: Iterator> IterE for T {}
pub struct RoundRobin<T>(Vec<Box<Iterator<Item=T>>>, usize);
pub fn round_robin<T>(v: Vec<Box<Iterator<Item=T>>>) -> RoundRobin<T> {
    RoundRobin(v, 0)
}
impl <T> Iterator for RoundRobin<T> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        let r = unsafe { self.0.get_unchecked_mut(self.1).next() };

        if r.is_some() {
            self.1 = (self.1 + 1) % self.0.len();
        }

        r
    }
}

pub fn unify_result<T>(r: Result<T, T>) -> T
{
    match r
    {
        Ok(t) => t,
        Err(t) => t
    }
}
