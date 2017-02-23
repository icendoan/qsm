#![feature(box_syntax, box_patterns, conservative_impl_trait)]
extern crate krust;
extern crate libc;
extern crate byteorder;
extern crate gnuplot;

use krust::kbindings::KOwned;

use std::{fmt, io};
use std::collections::{VecDeque, HashMap};
use std::sync::mpsc;

mod plotter;
mod action;
mod conn;
mod common;
use action::Action;
use common::*;
use conn::*;

enum Status
{
    Connected(Connection),
    Disconnected,
}

impl fmt::Display for Status
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match *self
        {
            Status::Connected(_) => write!(f, "Connected"),
            Status::Disconnected => write!(f, "Disconnected"),
        }
    }
}

pub struct Control
{
    msg_seq: u64,
    settings: Setting,
    servers: HashMap<String, Server>,
    snippets: HashMap<String, String>,
}

impl Control
{
		let mut fig: gnuplot::Figure = gnuplot::Figure::new();
    pub fn new() -> (Control, mpsc::Receiver<String>, mpsc::Sender<String>)
    {
        let (tx_t, rx_t) = mpsc::channel();

        let c = Control {
            msg_seq: 0,
            settings: Setting::new(),
            servers: HashMap::new(),
            snippets: HashMap::new(),
        };

        (c, rx_t, tx_t)
    }

    pub fn run(&mut self, rx_term: &mut mpsc::Receiver<String>)
    {
        let mut should_shutdown = false;
        while !should_shutdown
        {
            for s in self.servers.values_mut()
            {
                if s.task.is_some()
                {
                    if let Status::Connected(ref mut conn) = s.status
                    {
                        match conn.resp()
                        {
                            Ok(msg) =>
                            {
                                let task = s.task.take().unwrap();
                                s.queue.push_back((task, msg));
                            },

                            // WouldBlock is okay
                            Err(Error::IOError(ref e)) if e.kind() == io::ErrorKind::WouldBlock => (),
                            Err(e) => err(&s.name, e),
                        }
                    }
                }
            }

            match rx_term.try_recv()
            {
                Ok(msg) =>
                {
                    match Action::parse(&msg)
                    {
                        Ok(act) =>
                        {
                            match action(act,
                                         &mut self.servers,
                                         &mut self.snippets,
                                         &mut self.settings)
                            {
                                Ok(r) => should_shutdown = r,
                                Err(e) => println!("{}", e),
                            }
                        },
                        Err(e) => println!("{}", e),
                    }
                },
                Err(mpsc::TryRecvError::Empty) => (),
                Err(mpsc::TryRecvError::Disconnected) => return, // if the terminal channel goes down, abort
            }

            {

                if let &Some(ref name) = &self.settings.server
                {
                    if let Some(ref mut server) = self.servers.get_mut(name)
                    {
                        for (task, result) in server.queue.drain(..)
                        {

                            let eval = match task
                            {
                                Task::K | Task::Q => print(result),
                                Task::Plot => plotter::plot(&mut fig, result),
                            };

                            if let Err(e) = eval
                            {
                                println!("{}", e);
                            }

                        }
                    }
                }
            }
        }
    }
}



fn print(k: KOwned) -> R<()>
{
    Ok(())
}

fn action(a: Action, servers: &mut HashMap<String, Server>, snippets: &mut HashMap<String, String>, settings: &mut Setting) -> R<bool>
{
    fn server_from_name<'a>(name: Option<&str>, servers: &'a mut HashMap<String, Server>, settings: &Setting) -> R<&'a Server>
    {
        if let Some(n) = name
        {
            servers.get(n).ok_or(Error::WithMsg("No such server."))
        }
        else if let &Some(ref n) = &settings.server
        {
            servers.get(n).ok_or(Error::WithMsg("No such server."))
        }
        else
        {
            Err(Error::WithMsg("No server specified."))
        }
    }

    fn server_from_name_mut<'a>(name: Option<&str>, servers: &'a mut HashMap<String, Server>, settings: &Setting) -> R<&'a mut Server>
    {
        if let Some(n) = name
        {
            servers.get_mut(n).ok_or(Error::WithMsg("No such server."))
        }
        else if let &Some(ref n) = &settings.server
        {
            servers.get_mut(n).ok_or(Error::WithMsg("No such server."))
        }
        else
        {
            Err(Error::WithMsg("No server specified."))
        }

    }

    fn snippet_from_name<'a>(name: Option<&str>, snippets: &'a HashMap<String, String>) -> R<&'a str>
    {
        name.and_then(|n| snippets.get(n))
            .map(|x| x.as_ref())
            .ok_or(Error::WithMsg("No such snippet."))
    }


    fn preprocess(snippets: &HashMap<String, String>, query: &str) -> R<String>
    {
        let mut s = String::with_capacity(query.len());

        let mut slice = query;
        while !slice.is_empty()
        {
            let next_index = slice.find('£');
            if let Some(index) = next_index
            {
                let (head, tail) = slice.split_at(index);
                s.push_str(head);

                if let Some(end) = tail.find(|c: char| !c.is_alphabetic())
                {
                    let (name, rest) = tail.split_at(end);
                    s.push_str(snippet_from_name(Some(name), snippets)?);
                    slice = rest;
                }
                else
                {
                    s.push_str(tail);
                    return Ok(s);
                }
            }
            else
            {
                s.push_str(slice);
                return Ok(s);
            }
        }

        Ok(s)
    }

    match a
    {
        Action::Connect { name } =>
        {
            let name_ref: Option<&str> = name.map(|x| &*x);
            let server = server_from_name_mut(name_ref, servers, settings)?;
            server.connect().map(|_| {
                println!("Connected");
                false
            })
        },

        Action::Disconnect { name } =>
        {
            let name_ref: Option<&str> = name.map(|x| &*x);
            let server = server_from_name_mut(name_ref, servers, settings)?;
            server.disconnect().map(|_| {
                println!("Disconnected");
                false
            })
        },

        Action::Delete { name } =>
        {
            match name
            {
                Some(n) => servers.remove(n),
                None =>
                {
                    match settings.server
                    {
                        Some(ref n) => servers.remove(n),
                        None => return Err(Error::WithMsg("No such server.")),
                    }
                },
            };

            Ok(false)
        },

        Action::Exit => return Ok(true),
        Action::Help =>
        {
            help();
            Ok(false)
        },
        Action::Ignore => Ok(false),
        Action::Info { name } =>
        {
            fn server_info(server: &Server) -> String
            {
                format!("{}: {}:{}, Status:{}, Pending Messages: {}, Current Task: {:?}",
                        server.name,
                        server.addr,
                        server.port,
                        server.status,
                        server.queue.len(),
                        server.task)
            }

            fn snippet_info(snippet: &str) -> String
            {
                snippet.to_owned()
            }


            let name: Option<&str> = name.map(|x| x.as_ref());

            let info: R<String> = server_from_name(name, servers, settings)
                .map(server_info)
                .or(snippet_from_name(name, snippets).map(snippet_info));
            println!("{}", info?);

            Ok(false)
        },

        Action::List =>
        {
            for server in servers.values()
            {
                println!("{} ({})", server.name, server.status);
            }

            for snippet in snippets.keys()
            {
                println!("{}", snippet);
            }

            Ok(false)
        },

        Action::New { name, addr, port, user, pass } =>
        {
            let server = Server::new(name.clone(), addr, port, user, pass);
            println!("Added {}.", &name);
            servers.insert(name, server);
            Ok(false)
        },

        Action::Query { task, query } =>
        {
            let task = task.unwrap_or(settings.msgtype);

            if let Some(ref name) = settings.server
            {
                let server = servers.get_mut(name).ok_or(Error::WithMsg("No such server."))?;

                let query = preprocess(snippets, query)?;

                match &mut server.status
                {
                    &mut Status::Connected(ref mut conn) => conn.query(query).map(|_| false),
                    &mut Status::Disconnected => Err(Error::NotConnected),
                }
            }
            else
            {
                Err(Error::WithMsg("No server specified."))
            }
        },

        Action::Set { setting } =>
        {

            if setting.has_update()
            {
                settings.patch(setting);
                println!("Settings updated.");
            }
            else
            {
                println!("{}", settings);
            }
            Ok(false)
        },

        Action::Snippet { name, src } =>
        {
            snippets.insert(name, src);
            println!("Snippet added.");
            Ok(false)
        },
    }
}

fn err(server: &str, err: Error)
{
    println!("{}: {}", server, err)
}

fn help()
{
    println!(":n name;addr;port;user;pass - Define a new server to connect to.
:c <name> - Connect \
	          to the specified server, or if not specified, the current one.
:x <name> - Disconnect from the specified server, or if not specified, the current one.
:S name £ body - \
	          Create a new snippet with name, and value body. Invoke via inserting £name into \
	          text.
:s (name=value;)* - Set setting with name `name` to `value`. If invoked \
	          without any key-value pairs, prints current settings.
:l - List known servers and \
	          snippets.
:d <name> - Deletes the server or snippet with name `name`. If no name is \
	          supplied, deletes the current server.
:i <name> - Prints information about the \
	          specified server or snippet.
:r <query> - Sends this query without any \
	          preprocessing.
:plot <query> - Plots the value of this query with gnuplot.
:pretty \
	          <query> - Enforces pretty printing for this query.
:h - Prints this message.

All \
	          other strings are preprocessed and sent as queries to the current server.
")
}

struct Server
{
    addr: String,
    port: Port,
    name: String,
    user: Option<String>,
    pass: Option<String>,
    queue: VecDeque<(Task, KOwned)>,
    task: Option<Task>,
    status: Status,
}

impl Server
{
    fn new(name: String, addr: String, port: Port, user: Option<String>, pass: Option<String>) -> Server
    {
        Server {
            addr: addr,
            port: port,
            name: name,
            user: user,
            pass: pass,
            queue: VecDeque::new(),
            task: None,
            status: Status::Disconnected,
        }
    }

    fn connect(&mut self) -> R<()>
    {
        fn opt_str(x: &Option<String>) -> Option<&str>
        {
            if let &Some(ref s) = x { Some(s) } else { None }
        }

        self.status = Status::Disconnected; // close old connection

        let addr = &self.addr;
        let user = opt_str(&self.user);
        let pass = opt_str(&self.pass);

        let conn: Connection;

        if self.addr == "localhost"
        {
            conn = Connection::unix(addr, user, pass)?;
        }
        else
        {
            conn = Connection::tcp(addr, self.port, user, pass)?;
        }


        self.status = Status::Connected(conn);

        Ok(())
    }

    fn disconnect(&mut self) -> R<()>
    {
        self.status = Status::Disconnected;
        Ok(())
    }
}
