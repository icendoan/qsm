#![feature(box_syntax, box_patterns, conservative_impl_trait)]
extern crate krust;
extern crate libc;
extern crate byteorder;
extern crate gnuplot;

use krust::kbindings::{KOwned, KVal, KData};

use std::{ffi, ptr, fmt, net, env};
use std::collections::{VecDeque, HashMap};
use std::convert::From;
use std::io::{self, Read, Write};
use std::os::unix;
use std::sync::mpsc;

mod plotter;
mod conn;
use conn::*;

#[derive(Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
struct ServerId(u64);


pub type R<T> = Result<T, Error>;


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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Task
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

enum Action<'a>
{
	New
	{
		name: String,
		addr: String,
		port: Port,
		user: Option<String>,
		pass: Option<String>,
	},

	Connect
	{ name: Option<&'a str> },

	Query
	{ task: Option<Task>, query: String },

	Snippet
	{ name: String, src: String },

	List,
	Delete
	{ name: Option<&'a str> },
	Set
	{ setting: SettingsBuilder },
	Help,
	Info
	{ name: Option<&'a str> },
	Ignore,
	Exit,
}

impl<'a> Action<'a>
{
	fn parse(txt: &'a str) -> R<Action<'a>>
	{

		fn optional_string(txt: &str) -> Option<String>
		{
			if txt.is_empty()
			{
				None
			}
			else
			{
				Some(txt.to_owned())
			}
		}

		fn optional_str(txt: &str) -> Option<&str>
		{
			if !txt.is_empty() { Some(txt) } else { None }
		}

		fn split_key_value_pair<'a>(txt: &'a str, splitter: &str) -> R<(&'a str, &'a str)>
		{
			let eq_ix = txt.find(splitter).ok_or(Error::ParseError(Some("Not a key-value pair")))?;
			Ok((&txt[..eq_ix].trim(), &txt[eq_ix + 1..].trim()))
		}


		fn new(txt: &str) -> R<Action>
		{
			struct NewBuilder
			{
				name: Option<String>,
				addr: Option<String>,
				port: Option<Port>,
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
					if let Ok(x) = u16::from_str_radix(port, 10)
					{
						self.port = Some(Port(x));
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
						Ok(Action::New {
							name: n,
							addr: a,
							port: self.port.unwrap_or(Port(0)),
							user: self.user,
							pass: self.pass,
						})
					}
					else
					{
						Err(Error::ParseError(Some("Not enough fields for New")))
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
			let mut iter = txt.split(';');

			builder.name(iter.next().ok_or(Error::ParseError(Some("Incomplete input for New")))?);
			builder.addr(iter.next().ok_or(Error::ParseError(Some("Incomplete input for New")))?);
			builder.port(iter.next().ok_or(Error::ParseError(Some("Incomplete input for New")))?);
			builder.user(iter.next());
			builder.pass(iter.next());
			builder.build()
		}

		fn settings(txt: &str) -> R<Action>
		{
			// syntax is "key = value (; key = value )+"
			let mut b = SettingsBuilder::new();

			for kv in txt.split(';')
			{
				match split_key_value_pair(kv, "=")
				{
					Ok(("server", name)) => b.server(name.to_owned()),
					Ok(("msgtype", m)) =>
					{
						b.msgtype(match m
						{
							"pretty" | "q" => Task::Q,
							"plot" => Task::Plot,
							"raw" | "k" => Task::K,
							_ => return Err(Error::ParseError(Some("Unrecognised msgtype"))),
						})
					},
					Ok((x, _)) => return Err(Error::ParseError(Some("Unrecognised setting"))),
					Err(e) => return Err(e),
				};
			}

			Ok(Action::Set { setting: b })
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

		fn query(txt: &str, task: Option<Task>) -> R<Action>
		{
			Ok(Action::Query {
				task: task,
				query: txt.to_owned(),
			})
		}

		fn snippet(txt: &str) -> R<Action>
		{
			// syntax: name £ body - £ is not an allowed char in q
			let (name, body) = split_key_value_pair(txt, "£")?;
			Ok(Action::Snippet {
				name: name.to_owned(),
				src: body.to_owned(),
			})
		}


		let trimmed: &'a str = txt.trim();
		match trimmed
		{
			t if t.starts_with(":q") || t.starts_with("//") => Ok(Action::Exit),
			t if t.starts_with(":s") => settings(&t[2..]),
			t if t.starts_with(":n") => new(&t[2..]),
			t if t.starts_with(":l") => Ok(Action::List),
			t if t.starts_with(":h") => Ok(Action::Help),
			t if t.starts_with(":d") => delete(&t[2..]),
			t if t.starts_with(":c") => connect(&t[2..]),
			t if t.starts_with(":i") => info(&t[2..]),
			t if t.starts_with(":S") => snippet(&t[2..]),
			t if t.starts_with("/") => Ok(Action::Ignore),
			t if t.starts_with(":plot") => query(t, Some(Task::Plot)),
			t if t.starts_with(":r") => query(t, Some(Task::K)),
			t if t.starts_with(":pretty") => query(t, Some(Task::Q)),
			t => query(t, None),
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
		let mut fig: gnuplot::Figure = gnuplot::Figure::new();
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

fn action(a: Action,
          servers: &mut HashMap<String, Server>,
          snippets: &mut HashMap<String, String>,
          settings: &mut Setting)
          -> R<bool>
{
	fn server_from_name<'a>(name: Option<&str>,
	                        servers: &'a mut HashMap<String, Server>,
	                        settings: &Setting)
	                        -> R<&'a Server>
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

	fn server_from_name_mut<'a>(name: Option<&str>,
	                            servers: &'a mut HashMap<String, Server>,
	                            settings: &Setting)
	                            -> R<&'a mut Server>
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

	fn snippet_from_name<'a>(name: Option<&str>, snippets: &'a HashMap<String, String>)
	                         -> R<&'a str>
	{
		name.and_then(|n| snippets.get(n))
			.map(|x| x.as_ref())
			.ok_or(Error::WithMsg("No such snippet."))
	}


	fn preprocess(snippets: &HashMap<String, String>, mut query: String) -> String
	{
		query
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

				let query = preprocess(snippets, query);

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
	port: conn::Port,
	name: String,
	user: Option<String>,
	pass: Option<String>,
	queue: VecDeque<(Task, KOwned)>,
	task: Option<Task>,
	status: Status,
}

impl Server
{
	fn new(name: String,
	       addr: String,
	       port: conn::Port,
	       user: Option<String>,
	       pass: Option<String>)
	       -> Server
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

struct Setting
{
	msgtype: Task,
	server: Option<String>,
}

impl Setting
{
	fn new() -> Self
	{
		Setting {
			msgtype: Task::Q,
			server: None,
		}
	}

	fn patch(&mut self, other: SettingsBuilder)
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

struct SettingsBuilder
{
	msgtype: Option<Task>,
	server: Option<String>,
}

impl SettingsBuilder
{
	fn new() -> Self
	{
		SettingsBuilder {
			msgtype: None,
			server: None,
		}
	}

	fn msgtype(&mut self, task: Task) -> &mut Self
	{
		self.msgtype = Some(task);
		self
	}

	fn server(&mut self, server: String) -> &mut Self
	{
		self.server = Some(server);
		self
	}

	fn build(self) -> R<Setting>
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
	fn has_update(&self) -> bool
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

fn sym_str(x: &*const i8) -> &str
{
	unsafe { ffi::CStr::from_ptr(*x).to_str().unwrap() }
}

fn kbool(k: KVal) -> Option<&[bool]>
{
	if let KVal::Bool(KData::List(b)) = k
	{
		Some(b)
	}
	else
	{
		None
	}
}

fn kguid(k: KVal) -> Option<&[[u8; 16]]>
{
	if let KVal::Guid(KData::List(b)) = k
	{
		Some(b)
	}
	else
	{
		None
	}
}

fn ku8(k: KVal) -> Option<&[u8]>
{
	if let KVal::Byte(KData::List(x)) = k
	{
		Some(x)
	}
	else
	{
		None
	}
}

fn ki16(k: KVal) -> Option<&[i16]>
{
	if let KVal::Short(KData::List(x)) = k
	{
		Some(x)
	}
	else
	{
		None
	}
}

fn ki32(k: KVal) -> Option<&[i32]>
{
	match k
	{
		KVal::Int(KData::List(x)) |
		KVal::Month(KData::List(x)) |
		KVal::Date(KData::List(x)) |
		KVal::Minute(KData::List(x)) |
		KVal::Second(KData::List(x)) |
		KVal::Time(KData::List(x)) => Some(x),
		_ => None,
	}
}

fn ki64(k: KVal) -> Option<&[i64]>
{
	match k
	{
		KVal::Long(KData::List(x)) |
		KVal::Timestamp(KData::List(x)) |
		KVal::Timespan(KData::List(x)) => Some(x),
		_ => None,
	}
}

fn kf32(k: KVal) -> Option<&[f32]>
{
	if let KVal::Real(KData::List(x)) = k
	{
		Some(x)
	}
	else
	{
		None
	}
}

fn kf64(k: KVal) -> Option<&[f64]>
{
	match k
	{
		KVal::Float(KData::List(x)) |
		KVal::Datetime(KData::List(x)) => Some(x),
		_ => None,
	}
}
