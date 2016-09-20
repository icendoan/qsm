#![feature(slice_patterns)]
#![feature(advanced_slice_patterns)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(stmt_expr_attributes)]
#![feature(plugin)]
#![feature(mpsc_select)]
// #![plugin(clippy)]

extern crate qsm;
extern crate krust;

use std::io::{self, BufRead, Read, Write};
use std::collections::{BTreeMap, HashMap};
use std::{fs, thread};
use std::sync::mpsc;

use qsm::*;

use krust::kbindings::KVal;

struct State
{
    store: BTreeMap<u64, ConnectionInfo>,
    ids: HashMap<String, u64>,
    curr: u64,
    id_seq: u64,
    tx: mpsc::Sender<(u64, ConnectionResponse)>, // for cloning
}

enum Control
{
    Init,
    Ready(mpsc::Receiver<String>)
}

impl Control
{
    fn new() -> Control
    {
        Control::Init
    }

    fn run(mut self, mut st: State, mut rx: mpsc::Receiver<(u64, ConnectionResponse)>) -> mpsc::Sender<String>
    {
        let (tx_in, rx_in) = mpsc::channel();

        self = Control::Ready(rx_in);

        thread::spawn(move ||
                      {
                          let lines = if let Control::Ready(x) = self
                          {
                              x
                          }
                          else
                          {
                              return
                          };

                          loop
                          {
                              select! {
                                  l = lines.recv() => {
                                      if let Ok(l) = l
                                      {
                                          line(&mut st, l);
                                      }
                                      else
                                      {
                                          break  
                                              
                                      }
                                  },
                                  
                                  m = rx.recv() =>
                                  {
                                      if let Ok(m) = m
                                      {
                                          msg(&mut st, m);
                                      }
                                      else
                                      {
                                          break
                                      }
                                  }
                              }
                          }
                      });
        tx_in
    }
}

impl State
{
    fn new() -> (State, mpsc::Receiver<(u64, ConnectionResponse)>)
    {
        let (tx, rx) = mpsc::channel();

        let s = State {
            store: BTreeMap::new(),
            ids: HashMap::new(),
            curr: 0,
            id_seq: 0,
            tx: tx,
        };

        (s, rx)
    }
}

fn main()
{
    let stdin = io::stdin();
    let (mut st, mut rx) = State::new();

    if let Ok(mut f) = fs::File::open(".qsm")
    {
        let mut conf: String = if let Ok(meta) = f.metadata()
        {
            String::with_capacity(meta.len() as usize)
        }
        else
        {
            String::new()
        };

        let _ = f.read_to_string(&mut conf);

        for line in conf.lines()
        {
            eval(&mut st, parse(line))
        }
    }

    println!("If lost, try :h");
    print!("{})", st.curr);
    io::stdout().flush().unwrap();
    let mut lines = stdin.lock().lines();

    let mut c = Control::new();

    let tx = c.run(st, rx);

    while let Some(Ok(line)) = lines.next()
    {
        tx.send(line);
    }
}

fn eval(st: &mut State, ac: Action)
{
    match ac
    {
        Action::Query(q) =>
        {
            let conn = if let Some(c) = st.store.get(&st.curr)
            {
                c
            }
            else
            {
                return println!("Error: invalid connection");
            };

            conn.tx.send(ConnectionMessage::Query(q));
        },

        Action::Copy(expr, to, name) =>
        {
            // todo: properly handle return messages
            return println!("Unimplemented");
            /*
            let conn = if let Some(c) = st.store.get(&st.curr)
            {
                c
            }
            else
            {
                return println!("Error: invalid connection");
            };

            let q = Query {
                qtype: QueryType::Raw,
                text: expr.to_owned(),
                params: Vec::new(),
            };

            conn.tx.send(ConnectionMessage::Query(q));

            // don't see a way to avoid blocking here
            match conn.rx.recv()
            {
                ConnectionResponse::KResult(k) =>
                {
                    // find new server
                    let sink = match st.store.values().filter(|x| x.name == to).next()
                    {
                        Some(s) => s,
                        Err(_) => return println!("Error: destination does not exist"),
                    };

                    let mov = Query {
                        qtype: QueryType::Raw,
                        text: format!("{}:", name),
                        params: vec![k],
                    };

                    sink.tx.send(ConnectionMessage::Query(mov));

                    println!("{}: {} -> {}", name, conn.name, to);

                },

                _ => println!("Error: Did not receive K object"),
            }
            */
        },

        Action::Rename(from, to) =>
        {
            if let Some(id) = st.ids.remove(from)
            {
                st.ids.insert(to.to_owned(), id);
                println!("Renamed {} -> {}", from, to);
            }
        },

        Action::Help =>
        {},

        Action::New(name, cb) =>
        {
            let tx = st.tx.clone();
            let (tx_msg, rx) = mpsc::channel();

            let conn = match cb.finalise(tx, rx, st.id_seq)
            {
                Ok(c) => c,
                Err(e) => return println!("Error: {}", e.0),
            };

            let info = ConnectionInfo {
                name: name.to_owned(),
                id: st.id_seq,
                addr: conn.addr.clone(),
                port: conn.port,
                user: conn.user.clone(),
                pass: conn.pass.clone(),
                tx: tx_msg,
                last_msg: None,
            };

            match st.store.insert(st.id_seq, info)
            {
                Some(_) => println!("Replaced connection #{} with {}",
                                    st.id_seq,
                                    name),
                None => println!("Added new connection #{}: {}",
                                 st.id_seq,
                                 name),
            };

            st.ids.insert(name.to_owned(), st.id_seq);

            st.id_seq += 1;

            conn.run();
        },

        Action::Delete(name) =>
        {
            if let Some(id) = st.ids.remove(name)
            {
                if let Some(conn) = st.store.remove(&id)
                {
                    conn.tx.send(ConnectionMessage::Close);
                    println!("Deleted connection: {}", name);
                }
            }
        },

        Action::List =>
        {
            for info in st.store.values()
            {
                println!("Connections:");
                println!("{}", info.name);
            }
        },

        Action::Open(path) =>
        {
            if let Ok(mut f) = fs::File::open(path)
            {
                let mut out = if let Ok(meta) = f.metadata()
                {
                    String::with_capacity(meta.len() as usize)
                }
                else
                {
                    String::new()
                };

                let _ = f.read_to_string(&mut out);

                for line in out.lines()
                {
                    eval(st, parse(line));
                }
            }
        },

        Action::Save(path) =>
        {
            if let Ok(mut f) = fs::File::create(path)
            {
                let mut buffer = String::new();

                for (name, ref info) in st.ids.keys().zip(st.store.values())
                {
                    buffer.push_str(":n ");
                    buffer.push_str(name);
                    buffer.push(';');
                    buffer.push_str(&info.addr);
                    buffer.push(';');
                    buffer.push_str(&format!("{}", info.port));
                    buffer.push(';');

                    if let Some(ref u) = info.user
                    {
                        buffer.push_str(u);
                    }

                    buffer.push(';');

                    if let Some(ref p) = info.pass
                    {
                        buffer.push_str(p);
                    }

                    buffer.push(';');

                    f.write_all(buffer.as_bytes());

                    buffer.clear();
                }

                println!("Saved data at: {}", path);
            }
            else
            {
                println!("Error: could not create file: {}", path)
            }
        },

        Action::Reconnect =>
        {
            if let Some(conn) = st.store.get(&st.curr)
            {
                conn.tx.send(ConnectionMessage::Reconnect);
                println!("Reconnected {}", conn.name);
            }
        },

        Action::Switch(to) =>
        {
            if let Some(id) = st.ids.get(to)
            {
                st.curr = *id;

                println!("Switched to {}", to);

                let last_msg = if let Some(ref c) = st.store.get(id)
                {
                    match c.last_msg
                    {
                        Some(ref m) => m,
                        _ => return
                    }
                }

                else
                {
                    return
                };

                match *last_msg
                {
                    ConnectionResponse::StrResult(ref s) => println!("{}", s),
                    ConnectionResponse::KResult(ref k) => println!("{:?}", KVal::new(k.0)),
                    ConnectionResponse::Error(ref e) => println!("Error: {}", e),
                }
            }
        },

        Action::Result =>
        {
            let last_msg = if let Some(ref c) = st.store.get(&st.curr)
            {
                match c.last_msg
                {
                    Some(ref m) => m,
                    _ => return
                }
            }

            else
            {
                return
            };

            match *last_msg
            {
                ConnectionResponse::StrResult(ref s) => println!("{}", s),
                ConnectionResponse::KResult(ref k) => println!("{:?}", KVal::new(k.0)),
                ConnectionResponse::Error(ref e) => println!("Error: {}", e),
            }
        },

        Action::Ignore =>
        {},

        Action::Error(e) =>
        {
            println!("Error: {}", e)
        },

    }
}

fn line(st: &mut State, line: String)
{
    let curr = if let Some(c) = st.store.get(&st.curr)
    {
        c.name.clone()
    }
    else
    {
        "<NONE>".to_owned()
    };

    if line.is_empty()
    {
        print!("{})", curr);
        io::stdout().flush().unwrap();
    }

    print!("{})", curr);
    eval(st, parse(&line));
    io::stdout().flush().unwrap();
}

fn msg(st: &mut State, (id, msg): (u64, ConnectionResponse))
{
    let curr = match st.store.get(&st.curr)
    {
        Some(c) => c.name.clone(),
        _ => "<NONE>".to_owned()
    };

    if id == st.curr
    {
        println!("");
        match msg
        {
            ConnectionResponse::StrResult(s) =>
            {
                println!("{}", s);
            },

            ConnectionResponse::KResult(k) =>
            {
                println!("{:?}", KVal::new(k.0));
            },

            ConnectionResponse::Error(e) =>
            {
                println!("{}", e)
            },
        }
        print!("{})", curr);
        io::stdout().flush().unwrap();
    }
    else
    {
        let info = match st.store.get_mut(&id)
        {
            Some(c) => c,
            None =>
            {
                return println!("Error: message received from server id: {}, \
                          but no such server exists!",
                         id);
            },
        };

        info.last_msg = Some(msg);
    }
}
