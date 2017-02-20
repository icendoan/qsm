#![feature(mpsc_select)]
#![allow(unused_must_use)]
#![allow(unused_variables)]
extern crate qsm;
extern crate krust;


use krust::kbindings::KVal;

use qsm::*;
use std::{fs, thread};
use std::collections::{BTreeMap, HashMap};
use std::io::{self, BufRead, Read, Write};
use std::sync::mpsc;

struct State
{
    tx_control: mpsc::Sender<TMsg>,
    rx_control: mpsc::Receiver<TResp>,
    control: thread::JoinHandle<()>,
}

impl State
{
    fn new() -> Self
    {
        let (mut c, tx_c, rx_c) = Control::new();

        let thread = c.run();

        State {
            tx_control: tx_c,
            rx_control: rx_c,
            control: thread,
        }
    }
}

fn main()
{
    let mut state = State::new();
    let mut stdin = stdin().lock();
    let lines = stdin.lines();
    loop
    {
        if let Ok(line) = lines.next()
        {
            match parse(line)
            {
                Ok(TMsg::Exit) =>
                {
                    state.tx_control.send(msg);
                    break;
                },
                Ok(msg) => state.tx_control.send(msg),
                Err(e) => println!("Error: {}", e),
            }
        }

        match state.rx_control.try_recv()
        {
            Ok(TResp::Str(s)) => println!("{}", s),
            Ok(TResp::Err(e)) => println!("Error: {}", e),
            Err(mpsc::TryRecvError::Empty) =>
            {},
            Err(mpsc::TryRecvError::Disconnected) =>
            {
                println!("Error: disconnected from controller thread. Exiting!");
                break;
            },
        }

        thread::sleep_ms(100);
    }
}
