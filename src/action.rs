use common::*;

pub enum Action<'a>
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
    Disconnect
    { name: Option<&'a str> },
    Query
    { task: Option<Task>, query: &'a str },
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
    pub fn parse(txt: &'a str) -> R<Action<'a>>
    {
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
                query: txt,
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

        fn disconnect(txt: &str) -> R<Action>
        {
            Ok(Action::Disconnect { name: optional_str(txt) })
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
            t if t.starts_with(":x") => disconnect(&t[2..]),
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
