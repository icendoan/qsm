#![feature(untagged_unions)]
extern crate libc;
use std::{str, env, fs, ffi, slice};
use std::collections::HashMap;
use std::fmt;
use std::fmt::Write as FmtWrite;
use std::io::{self, Write, BufRead, BufReader};

const K_VOID: *const K = 0 as *const K;
type CharStar = *const libc::c_char;
// this shouldn't be sized
// but right now rust untagged unions cannot have unsized members
#[repr(C)]
#[derive(Debug)]
struct KA
{
    n: libc::c_long,
    g0: [u8;1]
}

#[repr(C)]
union KU
{
    g: [u8; 16],
    h: libc::c_short,
    i: libc::c_int,
    j: libc::c_long,
    e: libc::c_float,
    f: libc::c_double,
    s: CharStar,
    k: *const K,
    v: KA,
}

// k0 {signed char m,a,t;C u;I r;union{G g;H h;I i;J j;E e;F f;S s;struct k0*k;struct{J n;G G0[1];};};}*K;

#[repr(C)]
struct K
{
    m: libc::c_schar,
    a: libc::c_schar,
    t: libc::c_schar,
    u: libc::c_char,
    r: libc::c_int,
    data: KU
}

impl fmt::Debug for K
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "K@{:p} m: {:?}, a: {:?}, t: {:?}, u: {:?}, r: {:?}", &self, self.m, self.a, self.t, self.u, self.r)
    }
}

#[link(name="kdb")]
extern "C" {
    fn khp(addr: CharStar, port: i32) -> i32;
    fn khpu(addr: CharStar, port: i32, auth: CharStar) -> i32;
    fn k(hande: i32, query: CharStar, ...) -> *const K;
    fn r0(x: *const K);
    fn kclose(handle: i32);
}


fn main()
{
    let mut state = State::new();
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut lock = stdin.lock();
    let mut input = String::new();

    if let Err(e) = init(&mut input, &mut state)
    {
        err(e);
    }

    input.clear();
    print!("{})", &state.current);
    stdout.flush().expect("Couldn't flush stdout");

    loop
    {
        match lock.read_line(&mut input)
        {
            Ok(_) =>
            {
                if eval(&mut state, parse(&input)).is_none()
                {
                    break;
                }

                input.clear();
                print!("{})", &state.current);
                stdout.flush().expect("Couldn't flush stdout");
            },
            _ => break,
        }
    }

    exit(state);
}

fn init(input: &mut String, state: &mut State) -> R<()>
{
    let home = env::var("HOME").map_err(|_| Error::WithMsg("no HOME"))?;
    let path = format!("{}/.qsm", home);
    let file = fs::File::open(&path).map_err(|_| Error::WithMsg("Cannot find file ~/.qsm"))?;
    let mut reader = BufReader::new(file);

    loop
    {
        input.clear();
        match reader.read_line(input)
        {
            Ok(0) => break,
            Ok(_) => eval(state, parse(&*input)).ok_or(Error::WithMsg("Attempted early exit in init!"))?,
            Err(_) =>
            {
                println!("Error reading startup script ~/.qsm");
                break;
            },
        }
    }

    Ok(())
}

fn exit(mut state: State)
{
    for (_, server) in state.servers.iter_mut()
    {
        let _ = server.disconnect();
    }
}

struct State
{
    servers: HashMap<String, Server>,
    snippets: HashMap<String, String>,
    settings: Settings,
    current: String,
}

impl State
{
    fn new() -> State
    {
        State {
            servers: HashMap::new(),
            snippets: HashMap::new(),
            current: "NONE".to_owned(),
            settings: Settings::unset(),
        }
    }
}

struct Server
{
    addr: ffi::CString,
    port: i32,
    auth: Option<ffi::CString>,
    handle: Option<i32>,
}

impl Server
{
    fn connect(&mut self) -> R<()>
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
            0 => Err(Error::WithMsg("Bad authentication.")),
            -1 => Err(Error::KFailure),
            -2 => Err(Error::Timeout),
            h =>
            {
                self.handle = Some(h);
                Ok(())
            },
        }

    }

    fn disconnect(&mut self) -> R<()>
    {
        match self.handle.take()
        {
            Some(h) =>
            unsafe {
                kclose(h);
            },
            None => println!("Not connected."),
        };

        Ok(())
    }

    fn sync(&mut self, query: String) -> R<*const K>
    {
        let h = self.handle.ok_or(Error::NotConnected)?;
        let q = ffi::CString::new(query).map_err(|_| Error::WithMsg("Bad query (string failure)"))?;

        let ptr = unsafe {
            let qptr = q.into_raw() as *const i8;
            let ptr = k(h, qptr, K_VOID);
            let _ = ffi::CString::from_raw(qptr as *mut i8);
            ptr
        };

        Ok(ptr)
    }

    fn async(&mut self, query: String) -> R<()>
    {
        let h = self.handle.ok_or(Error::NotConnected)?;
        let q = ffi::CString::new(query).map_err(|_| Error::WithMsg("Bad query (string failure)"))?;

        unsafe {
            let qptr = q.into_raw();
            let _ = k(-1 * h,
                                 qptr as *const i8,
                                 K_VOID);
            let _ = ffi::CString::from_raw(qptr);
        }

        Ok(())
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
               unsafe { std::str::from_utf8_unchecked(self.addr.as_bytes()) },
               self.port,
               status)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Mode
{
    Q,
    K,
    Backtrace,
    Raw,
}

impl fmt::Display for Mode
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match *self
        {
            Mode::Q => write!(f, "Q"),
            Mode::K => write!(f, "K"),
            Mode::Backtrace => write!(f, "Backtrace"),
            Mode::Raw => write!(f, "Raw"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Settings
{
    mode: Option<Mode>,
    async: Option<bool>,
}

impl Settings
{
    fn unset() -> Settings
    {
        Settings {
            mode: None,
            async: None,
        }
    }

    fn patch(&mut self, other: Settings)
    {
        self.mode = other.mode.or(self.mode);
        self.async = other.async.or(self.async);
    }
}

impl fmt::Display for Settings
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f,
               "mode: {}\n",
               self.mode.unwrap_or(Mode::Q))?;
        write!(f,
               "async: {}\n",
               self.async.unwrap_or(false))?;

        Ok(())
    }
}

type R<T> = Result<T, Error>;

enum Error
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

enum Action<'a>
{
    New
    {
        name: String,
        addr: String,
        port: i32,
        auth: Option<String>,
    },

    Connect
    { name: Option<&'a str> },

    Query
    { query: &'a str, overrides: Settings },

    Snippet
    { name: String, src: String },

    List,
    Delete
    { name: Option<&'a str> },
    Set
    { setting: Settings },
    Help,
    Info
    { name: Option<&'a str> },
    Ignore,
    Load
    { path: Option<&'a str> },
    Save
    { path: Option<&'a str> },
    Exit,
}

impl<'a> Action<'a>
{
    fn parse(txt: &'a str) -> R<Action<'a>>
    {
        fn optional_str(txt: &str) -> Option<&str>
        {
            if !txt.is_empty() { Some(txt) } else { None }
        }

        fn split_pair<'a>(txt: &'a str, splitter: &str)
                          -> R<(&'a str, &'a str)>
        {
            let eq_ix = txt.find(splitter)
                .ok_or(Error::Parse(Some("Not a key-value pair")))?;
            Ok((&txt[..eq_ix].trim(), &txt[eq_ix + 1..].trim()))
        }


        fn new(txt: &str) -> R<Action>
        {
            struct NewBuilder
            {
                name: Option<String>,
                addr: Option<String>,
                port: Option<i32>,
                user: Option<String>,
                pass: Option<String>,
            }

            impl NewBuilder
            {
                fn name(&mut self, name: &str) -> &mut Self
                {
                    self.name = Some(name.to_owned());
                    self
                }

                fn addr(&mut self, addr: &str) -> &mut Self
                {
                    self.addr = Some(addr.to_owned());
                    self
                }

                fn port(&mut self, port: &str) -> &mut Self
                {
                    if let Ok(x) = i32::from_str_radix(port, 10)
                    {
                        self.port = Some(x);
                    }
                    self
                }

                fn user(&mut self, user: Option<&str>) -> &mut Self
                {
                    self.user = user.map(String::from);
                    self
                }

                fn pass(&mut self, pass: Option<&str>) -> &mut Self
                {
                    self.pass = pass.map(String::from);
                    self
                }

                fn build<'a>(self) -> R<Action<'a>>
                {
                    if let (Some(n), Some(a)) = (self.name, self.addr)
                    {
                        let auth = if let (Some(u), Some(p)) =
                            (self.user, self.pass)
                        {
                            let mut auth = u.clone();
                            auth.push(':');
                            auth.push_str(&p);
                            Some(auth)
                        }
                        else
                        {
                            None
                        };

                        Ok(Action::New {
                               name: n,
                               addr: a,
                               port: self.port.unwrap_or(0),
                               auth: auth,
                           })
                    }
                    else
                    {
                        Err(Error::Parse(Some("Not enough fields for New")))
                    }
                }
            }

            let mut builder: NewBuilder = NewBuilder {
                name: None,
                addr: None,
                port: None,
                user: None,
                pass: None,
            };

            let mut iter =
                txt.split(' ').map(str::trim).filter(|x| !x.is_empty());

            builder
                .name(iter.next().ok_or(Error::Parse(Some("Missing name")))?)
                .addr(iter.next().ok_or(Error::Parse(Some("Missing addr")))?)
                .port(iter.next().ok_or(Error::Parse(Some("Missing port")))?)
                .user(iter.next())
                .pass(iter.next());

            builder.build()

        }

        fn settings(txt: &str) -> R<Action>
        {
            if txt.trim().is_empty()
            {
                return Ok(Action::Set { setting: Settings::unset() });
            }

            // syntax is "key = value (; key = value )+"

            let mut s = Settings::unset();
            for kv in txt.split(' ')
            {
                match split_pair(kv, "=")?
                {
                    ("mode", "q") => s.mode = Some(Mode::Q),
                    ("mode", "k") => s.mode = Some(Mode::K),
                    ("mode", "Q") => s.mode = Some(Mode::Q),
                    ("mode", "K") => s.mode = Some(Mode::K),
                    ("mode", "bt") |
                    ("mode", "backtrace") => s.mode = Some(Mode::Backtrace),
                    ("mode", "Backtrace") => s.mode = Some(Mode::Backtrace),
                    ("mode", "raw") => s.mode = Some(Mode::Raw),
                    ("async", "true") => s.async = Some(true),
                    ("async", "false") => s.async = Some(false),
                    ("sync", "true") => s.async = Some(false),
                    ("sync", "false") => s.async = Some(true),
                    (_, _) =>
                    {
                        return Err(Error::Parse(Some("Unrecognised setting.")));
                    },
                }
            }

            Ok(Action::Set { setting: s })
        }

        fn delete(txt: &str) -> R<Action>
        {
            Ok(Action::Delete { name: optional_str(txt) })
        }

        fn connect(txt: &str) -> R<Action>
        {
            Ok(Action::Connect { name: optional_str(txt) })
        }

        fn info(txt: &str) -> R<Action>
        {
            Ok(Action::Info { name: optional_str(txt) })
        }

        fn query(query: &str, overrides: Settings) -> R<Action>
        {
            Ok(Action::Query { query, overrides })
        }

        fn snippet(txt: &str) -> R<Action>
        {
            let (name, body) = split_pair(txt, " ")?;
            Ok(Action::Snippet {
                   name: name.to_owned(),
                   src: body.to_owned(),
               })
        }

        fn save(txt: &str) -> R<Action>
        {
            Ok(Action::Save { path: optional_str(txt) })
        }

        fn load(txt: &str) -> R<Action>
        {
            Ok(Action::Load { path: optional_str(txt) })
        }

        let trimmed: &'a str = txt.trim();
        match trimmed
        {
            t if t.starts_with(":q") || t.starts_with(r"\\") =>
            
                Ok(Action::Exit),
            t if t.starts_with(":s") => settings(&t[2..].trim()),
            t if t.starts_with(":n") => new(&t[2..].trim()),
            t if t.starts_with(":l") => Ok(Action::List),
            t if t.starts_with(":h") => Ok(Action::Help),
            t if t.starts_with(":d") => delete(&t[2..].trim()),
            t if t.starts_with(":c") => connect(&t[2..].trim()),
            t if t.starts_with(":i") => info(&t[2..].trim()),
            t if t.starts_with(":S") => snippet(&t[2..].trim()),
            t if t.starts_with("/") || t.is_empty() => Ok(Action::Ignore),
            t if t.starts_with(":r") =>
            {
                let mut s = Settings::unset();
                s.mode = Some(Mode::Raw);
                query(&t[2..].trim(), s)
            },
            t if t.starts_with(":w") => save(&t[2..].trim()),
            t if t.starts_with(":x") => load(&t[2..].trim()),

            t => query(t, Settings::unset()),
        }
    }
}

fn help()
{
    println!(":n name addr port user pass - Define a new server to connect to.
:c <name> - Connect to the specified server, or if not specified, the current one.
:S name body - Create a new snippet with name, and value body. Invoke via inserting «name» into text.
:s (name=value;)* - Set setting with name `name` to `value`. If invoked without any key-value pairs, prints current settings.
:l - List known servers and snippets.
:d <name> - Deletes the server or snippet with name `name`. If no name is supplied, deletes the current server.
:i <name> - Prints information about the specified server or snippet.
:r <query> - Sends this query without any preprocessing. :pretty <query> - Enforces pretty printing for this query.
:h - Prints this message.
:w <path> - Saves servers, snippets and settings to the path ($HOME/.qsm if omitted).
:x <path> - Runs a file as a qsm script ($HOME/.qsm if omitted - run on startup).
\\\\ - exit the prompt

All other strings are preprocessed and sent as queries to the current server.
")
}

fn parse(text: &str) -> R<Action>
{
    Action::parse(text)
}

fn eval(state: &mut State, a: R<Action>) -> Option<()>
{
    match a
    {
        Ok(a) => action(state, a),
        Err(e) => err(e),
    }
}

fn action(s: &mut State, a: Action) -> Option<()>
{
    fn connect(s: &mut State, n: Option<&str>)
    {
        let n = if let Some(n) = n
        {
            if n == &s.current
            {
                println!("Reconnecting...");
                &s.current
            }
            else if s.servers.contains_key(n)
            {
                println!("Switching to: {}.", n);
                s.current.clear();
                if let Err(e) = s.current.write_str(n)
                {
                    return println!("Error: {}", e);
                }

                &s.current
            }
            else
            {
                return println!("No such server: {}.", n);
            }
        }
        else
        {
            &s.current
        };

        if let Some(server) = s.servers.get_mut(n)
        {
            match server.connect()
            {
                Ok(_) => println!("Connected to {}.", n),
                Err(_) => println!("Could not connect to {}.", n),
            }
        }
    }

    fn delete(s: &mut State, n: Option<&str>)
    {
        if let Some(n) = n
        {
            println!("{}",
                     s.servers
                         .remove(n)
                         .map(|_| format!("Removed {}.", n))
                         .or(s.snippets.remove(n))
                         .map(|_| format!("Removed {}.", n))
                         .unwrap_or(format!("No such server or snippet {}.",
                                            n)))
        }
        else
        {
            s.servers.remove(&s.current);
            println!("Removed {}.", &s.current);
            s.current = "NONE".to_owned();
        }
    }

    fn info(s: &mut State, n: Option<&str>)
    {
        let n = n.unwrap_or(&s.current);
        if let Some(server) = s.servers.get(n)
        {
            println!("{}", server);
        }
        else
        {
            println!("No such server: {}.", n);
        }
    }

    fn list(s: &State)
    {
        let name_len = s.servers.keys().map(String::len).max().unwrap_or(0);

        println!("Known servers:");
        for (name, server) in s.servers.iter()
        {
            println!("{0:width$}: {1}",
                     name,
                     server,
                     width = name_len + 1)
        }

        println!("");
        println!("Known snippets:");

        let snippet_len = s.snippets.keys().map(String::len).max().unwrap_or(0);
        let max_width = 80 - snippet_len;
        for (name, src) in s.snippets.iter()
        {
            let truncated_src = if src.len() > max_width
            {
                &src[..max_width]
            }
            else
            {
                src
            };
            let ellipses = if src.len() > max_width { "..." } else { "" };
            println!("{0:width$}: {1}{2}",
                     name,
                     truncated_src,
                     ellipses,
                     width = snippet_len + 1);
        }
    }

    fn new(s: &mut State,
           n: String,
           addr: String,
           port: i32,
           auth: Option<String>)
    {
        let addr = match ffi::CString::new(addr)
        {
            Ok(a) => a,
            _ => return println!("Invalid server address"),
        };

        let auth = match auth.map(ffi::CString::new)
        {
            Some(Ok(a)) => Some(a),
            None => None,
            _ => return println!("Invalid authentication (string failure)"),
        };

        let server = Server {
            addr: addr,
            port: port,
            auth: auth,
            handle: None,
        };

        if let Some(mut old) = s.servers.insert(n, server)
        {
            println!("Disconnecting old connection...");
            if let Err(e) = old.disconnect()
            {
                err(e);
            }
        }
        println!("Added new connection.");
    }

    fn set(s: &mut State, settings: Settings)
    {
        s.settings.patch(settings);
        println!("{}", &s.settings);
    }

    fn q(s: &mut State, query: &str, overrides: Settings)
    {
        let mut settings = s.settings.clone();
        settings.patch(overrides);

        let mut query = match settings.mode
        {
            None | Some(Mode::Q) => format!(".Q.s[({})]", query),
            Some(Mode::K) => format!("k) {}", query),
            Some(Mode::Backtrace) => format!(".Q.trp[{{.Q.s[({})]}};();{{raze \"'\",(string x),\"\\n\",.Q.sbt y}}]",
                                             query),
            Some(Mode::Raw) => query.to_owned(),
        };

        let mut fmtstr = String::new();
        for (name, src) in s.snippets.iter()
        {
            fmtstr.clear();
            let _ = write!(fmtstr, "«{}»", name);
            query = query.replace(&fmtstr, src);
        }

        let server = if let Some(srv) = s.servers.get_mut(&s.current)
        {
            srv
        }
        else
        {
            return println!("No such server: {}", &s.current);
        };

        if settings.async.unwrap_or(false)
        {
            if let Err(e) = server.async(query)
            {
                err(e);
            }
        }
        else
        {
            match server.sync(query)
            {
                Ok(kptr) =>
                {
                    pprint(kptr);
                    unsafe { r0(kptr); }
                },
                Err(e) =>
                {
                    err(e);
                },
            };
        }

    }

    fn snippet(s: &mut State, n: String, src: String)
    {
        if let Some(_) = s.snippets.insert(n, src)
        {
            println!("Replacing old snippet...");
        }
        println!("Added snippet.");
    }

    fn load(s: &mut State, path: Option<&str>)
    {
        let path = if let Some(path) = path
        {
            path.to_owned()
        }
        else
        {
            format!("{}/.qsm", env::var("HOME").unwrap())
        };

        let file = match fs::File::open(&path)
        {
            Ok(f) => f,
            Err(_) => return println!("Could not find file: {}.", &path),
        };

        let mut reader = BufReader::new(file);
        let mut input = String::new();

        loop
        {
            input.clear();
            match reader.read_line(&mut input)
            {
                Ok(0) => break,
                Ok(_) => eval(s, parse(&*input)).unwrap(),
                Err(_) =>
                {
                    println!("Error executing script: {}.", &path);
                    break;
                },
            }
        }


    }

    fn save(s: &State, path: Option<&str>)
    {
        let path = if let Some(path) = path
        {
            path.to_owned()
        }
        else
        {
            format!("{}/.qsm", env::var("HOME").unwrap())
        };

        let mut file = match fs::File::create(&path)
        {
            Ok(f) => f,
            Err(_) => return println!("Could not find file: {}.", &path),
        };

        write!(file,
               ":s mode={} async={}\n",
               s.settings.mode.unwrap_or(Mode::Q),
               s.settings.async.unwrap_or(false))
                .unwrap();

        for (name, server) in &s.servers
        {
            write!(file,
                   ":n {} {} {}",
                   name,
                   unsafe { str::from_utf8_unchecked(server.addr.as_bytes()) },
                   server.port)
                    .unwrap();
            if let Some(ref auth) = server.auth
            {
                let string =
                    unsafe { str::from_utf8_unchecked(auth.as_bytes()) };
                for chunk in string.split(':')
                {
                    write!(file, " {}", chunk).unwrap();
                }
            }

            write!(file, "\n").unwrap();
        }

        write!(file, ":c {}\n", &s.current).unwrap();

        for (name, src) in &s.snippets
        {
            write!(file, ":S {} {}\n", name, src).unwrap();
        }

    }

    match a
    {
        Action::Connect { name } => Some(connect(s, name)),
        Action::Delete { name } => Some(delete(s, name)),
        Action::Exit => return None,
        Action::Help => Some(help()),
        Action::Ignore => Some(()),
        Action::Info { name } => Some(info(s, name)),
        Action::List => Some(list(s)),
        Action::Load { path } => Some(load(s, path)),
        Action::New {
            name,
            addr,
            port,
            auth,
        } => Some(new(s, name, addr, port, auth)),
        Action::Query { query, overrides } => Some(q(s, query, overrides)),
        Action::Set { setting } => Some(set(s, setting)),
        Action::Snippet { name, src } => Some(snippet(s, name, src)),
        Action::Save { path } => Some(save(s, path)),
    }
}

fn pprint(k: *const K)
{
    unsafe fn cast_list<'a, T:'a>(x: *const K) -> &'a [T]
    {
        let len = (*x).data.v.n as usize;
        let ptr = (*x).data.v.g0.as_ptr() as *const T;

        slice::from_raw_parts(ptr, len)
    }

    fn sym(prefix: char, ptr: CharStar)
    {
        let cstr: &ffi::CStr = unsafe { ffi::CStr::from_ptr(ptr) };
        match cstr.to_str()
        {
            Ok(s) => print!("{}{}", prefix, s),
            Err(_) => print!("'utf8"),
        }

    }

    fn syms<'a>(prefix: char, d: &[CharStar])
    {
        print!("[");
        for ptr in d
        {
            sym(prefix, *ptr);
        }
        print!("]");

        println!();
    }

    fn string(x: *const K) -> &'static str
    {
        unsafe
        {
            let bytes = cast_list::<u8>(x);
            str::from_utf8_unchecked(bytes)
        }
    }

    if k.is_null()
    {
        println!("Null kptr");
        return;
    }

    unsafe
    {
        match (*k).t
        {
            10 =>
            {
                let s = string(&*k);
                if s.chars().rev().next().map(|c| c == '\n').unwrap_or(false)
                {
                    print!("{}", s);
                }
                else
                {
                    println!("{}", s);
                }
            },
            
            -11 => sym('`',(*k).data.s),
            11 => syms('`', cast_list::<CharStar>(k)),

            0 => cast_list::<*const K>(k).iter().map(|x|pprint(*x)).fold((), |_, _| ()),

            -128 => {sym('\'', (*k).data.s); println!()},

            101 => println!(), // untyped null

            100 => println!("{}", string(&*k)), // lambda

            t => println!("Cannot print type {}", t),
        }
    }
}

fn err(e: Error) -> Option<()>
{
    println!("{}", e);
    Some(())
}
