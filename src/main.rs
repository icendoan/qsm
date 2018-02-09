#![feature(untagged_unions, box_syntax, box_patterns, mpsc_select)]
extern crate libc;
extern crate chrono;
extern crate clap;
use std::{thread, fs, env};
use std::io::{self, Read, BufReader, Write, BufRead};
use std::sync::mpsc::Sender;

// unsafe impl std::marker::Send for K {} 

mod k;
mod c; // control
mod s; // server
mod u; // util

use c::C;
use u::*;

fn main()
{
    use clap::{App,Arg};

    let args = App::new("qsm")
        .arg(Arg::with_name("eof")
             .short("e")
             .long("eof")
             .takes_value(false)
             .help("Requires EOF before sending strings (for use in emacs)."))
        .arg(Arg::with_name("init")
             .short("x")
             .long("init")
             .takes_value(true)
             .help("Path to init qsm file (default ~/.qsm)"))
        .get_matches();

    let (c, mut tx_io) = C::new();

    let handle = thread::spawn(move || c::run(c));

    let wait_for_eof = args.occurrences_of("eof") != 0;
    let init_path = args.value_of("init")
        .map(str::to_owned)
        .unwrap_or({
            let home = env::var("HOME").expect("No $HOME");
            format!("{}/.qsm", home)
    });

    let mut input = String::new();

    unify_result(init(&mut tx_io, &init_path, &mut input)
        .map_err(err));
        

    let stdin = io::stdin();
    let mut lock = stdin.lock();
    let mut stdout = io::stdout();

    loop
    {
        input.clear();
        stdout.flush().expect("Couldn't flush stdout");
        match if wait_for_eof
        {
            lock.read_to_string(&mut input)
        }
        else
        {
            lock.read_line(&mut input)
        }
        {
            Ok(_) => tx_io.send(input.clone())
                .expect("Failed to send input to controller. Exiting."),
            _ => break
        }
    }

    tx_io.send("\\".into())
        .expect("IO channel closed!");

    let c = handle.join()
        .expect("Controller thread crashed before returning.");

    exit(c);
}

fn init(tx_io: &mut Sender<String>, path: &str, input: &mut String) -> R<()>
{
    let file = fs::File::open(path)
        .map_err(|_| Error::WithMsg("Cannot find init file"))?;
    let mut reader = BufReader::new(file);

    loop
    {
        input.clear();
        match reader.read_line(input)
        {
            Ok(0) => break,
            Ok(_) => { tx_io.send(input.clone())
                       .expect("Cannot send IO."); },
            Err(_) =>
            {
                return Err(Error::WithMsg("Error reading init file."));
            },
        }
    }
 
    Ok(())
}

fn exit(mut c: C)
{
    for h in c.servers.values_mut()
    {
        h.disconnect();
    }
}
