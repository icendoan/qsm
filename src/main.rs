#![feature(nll,drain_filter,untagged_unions,box_syntax,box_patterns,str_escape)]#![allow(non_snake_case)]
extern crate libc; extern crate chrono;
use std::{str,env,net,sync,thread,time,fmt,fs,path,ffi}; use std::collections::{HashMap,VecDeque};
use std::io::{self,Write,Read,BufRead};use str::FromStr;use fmt::Write as FmtWrite;

mod k {
    use libc;
    use std::slice;
    pub type C=libc::c_char;
    pub type J=libc::c_long;
    pub type G=libc::c_uchar;
    pub type S=*const C;
    pub type SC=libc::c_schar;
    pub type H=libc::c_short;
    pub type I=libc::c_int;
    pub type E=libc::c_float;
    pub type F=libc::c_double;
    pub type K=*const K0;
    pub const KCAP:u8=3;
    #[repr(C)]#[derive(Debug,Copy,Clone)]pub struct KA{n:J,g0:[G;1]}
    #[repr(C)]pub union KU{g:G,h:H,i:I,j:J,e:E,f:F,s:S,k:K,v:KA}
    #[repr(C)]pub struct K0{m:SC,a:SC,t:SC,u:C,r:I,k:KU}
    #[link(name="kdb")]extern"C"{
        pub fn okx(x:K)->I;
        pub fn b9(x:I,y:K)->K;
        pub fn d9(x:K)->K;
        pub fn ktn(t:I,n:J)->K;
        pub fn r0(x:K);}
    pub fn tk<'a,T>(k:K)->&'a [T]{
        unsafe{slice::from_raw_parts(
            (&(*k).k.v.g0)as *const G as *const T,
            ((*k).k.v.n) as usize)} }
    pub fn mtk<'a,T>(k:K)->&'a mut [T]{
        unsafe{slice::from_raw_parts_mut(
            (&(*k).k.v.g0)as *const G as *const T as *mut T,
            ((*k).k.v.n) as usize)}}
    pub fn gK(k:&K0)->G{unsafe{k.k.g}}
    pub fn hK(k:&K0)->H{unsafe{k.k.h}}
    pub fn iK(k:&K0)->I{unsafe{k.k.i}}
    pub fn jK(k:&K0)->J{unsafe{k.k.j}}
    pub fn eK(k:&K0)->E{unsafe{k.k.e}}
    pub fn fK(k:&K0)->F{unsafe{k.k.f}}
    pub fn sK(k:&K0)->S{unsafe{k.k.s}}
    pub fn kK(k:&K0)->K{unsafe{k.k.k}}
    pub fn vK(k:&K0)->KA{unsafe{k.k.v}}
    pub fn ty(k:&K0)->SC{k.t}
}

use k::*;

#[derive(Debug)] enum Err {
    IO(io::Error),
    KFail,
    Parse(Option<String>),
    WithMsg(String),
    Pnd,
    DC,
    Wait,
    Fmt(fmt::Error) }
impl From<io::Error> for Err {
    fn from(e: io::Error) -> Err { Err::IO(e) }}
impl From<fmt::Error> for Err {
    fn from(e: fmt::Error)->Err { Err::Fmt(e) }}
macro_rules! E (($x:expr)=>(Err::WithMsg($x.into())));
type R<T>=Result<T,Err>;

struct Flags { multiline:bool }
impl Flags {
    fn flags() -> Flags {
        let mut f = Flags { multiline:false };
        for a in env::args()
        { match a.as_ref() {"-e"=>f.multiline=true,_=>()}} f}}

#[derive(PartialEq,Eq,Debug,Copy,Clone)] enum SState { DC, Pnd, Cnn, Hndshk }
impl Default for SState { fn default() -> SState { SState::DC }}
impl fmt::Display for SState {
    fn fmt(&self,f: &mut fmt::Formatter)->fmt::Result {
        write!(f,"{}",match*self{
            SState::DC=>"Disconnected",
            SState::Pnd=>"Pending response",
            SState::Hndshk=>"Handshaking",
            SState::Cnn=>"Connected"})}}

#[derive(Default)]
struct Server { addr: String,
                port: u16,
                auth: String,
                stream: Option<net::TcpStream>,
                queue: VecDeque<(String,Box<Fn(k::K)->R<String>>)>,
                state: SState,
                post: Option<Box<Fn(k::K)->R<String>>> }
impl Server {
    fn new(addr:String,port:u16,auth:String)->Server{
        Server{ addr, port, auth, ..Default::default() }}

    fn conn(&mut self)->R<()> {
        if self.state==SState::DC {
            let mut s=net::TcpStream::connect((self.addr.as_ref(),self.port))?;
            s.write(&self.auth.as_bytes())?;
            self.stream=Some(s);
            self.state=SState::Hndshk;
        }
        Ok(())}

 fn hndshk(&mut self)->R<()>{
     let mut s=self.stream.take()
         .ok_or(Err::WithMsg(
             "Bad state: in state HNDSHK with no connection"
                 .to_owned()))?;
     let mut buf=[0;8];
     match s.read(&mut buf[..]){
         Ok(1)=>{ if buf[0]!=k::KCAP {
             self.state=SState::DC;
             Err(Err::DC)
         } else {
             self.stream=Some(s);
             self.state=SState::Cnn;
             Ok(())}},
         _=>{ self.state=SState::DC;
              Err(Err::DC) }}}

 fn recv(&mut self)->R<String> {
     if self.state != SState::Pnd {
         return Err(E!("No pending response"))
     }
        let mut hdr=[0;9]; let mut s = self.stream.take().ok_or(E!("No stream but pending response"))?;
  match s.read(&mut hdr) {Ok(9) => (), Err(ref e)if e.kind()==io::ErrorKind::WouldBlock=>return Err(Err::Wait), Err(e) => return Err(Err::IO(e)), Ok(n) => return Err(Err::WithMsg(format!("Expected 9 bytes, received {}", n)))};
  let mut len: J = 0; for i in 0..4 { len += (hdr[ if 1 == hdr[0] { i } else { 4 - i }] as J) << i; }
  let b = unsafe { ktn(hdr[8] as I, len) }; s.read(mtk(b))?; let k = if 1 == unsafe { okx(b) } { unsafe { d9(b) } } else { return Err(Err::KFail) };
  if let Some(f) = self.post.take() { f(k) } else { Fmt::default().pprint(k) }}
 fn send(&mut self,q:&str,post:Box<Fn(K)->R<String>>)->R<()>{
     match self.state { SState::DC => {self.conn()?;self.hndshk()?}, SState::Hndshk => self.hndshk()?, SState::Pnd => return Err(Err::Pnd), SState::Cnn => () }
     let mut stream=self.stream.take().ok_or(Err::DC)?;let k = unsafe {ktn(10,q.len()as J)};mtk(k).copy_from_slice(q.as_bytes());let b = unsafe {b9(KCAP as I,k)};stream.write(tk(b))?; self.stream=Some(stream); self.post = Some(post); unsafe {r0(b);r0(k);} Ok(())}}
struct State { servers:HashMap<String,Server>, resp:Vec<(String, String)>, settings:Settings, curr:Option<String>, input: sync::mpsc::Receiver<String> }
#[derive(Debug,Copy,Clone,Eq,PartialEq)] enum Fmt { Q, PPr(usize,usize), CSV } impl Fmt
{ fn parse(x:&str)->R<Fmt> {match x.trim(){
    "q"=>Ok(Fmt::Q),"Q"=>Ok(Fmt::Q),"csv"=>Ok(Fmt::CSV), s if s.starts_with("ppr")=>
    {let mut i=s[4..s.len()-1].split(","); match(i.next().and_then(|x| usize::from_str(x).ok()),i.next().and_then(|x| usize::from_str(x).ok()))
     {(Some(l),Some(c))=>Ok(Fmt::PPr(l,c)),_=>Err(Err::Parse(Some("Bad ppr(x,y) format".into())))}},_=>Err(Err::Parse(Some("Unrecognised format".into())))}}
  fn pprint(&self,k:K)->R<String>
  {enum P {A(String),V(Vec<P>)} impl P { fn unwrap(self) -> String { match self { P::A(s) => s, _ => panic!() }}
                                         fn sv<'a>(self,x:&'a str,y:&'a str)->String{match self {P::A(s)=>s,P::V(v)=>{let mut s=v.into_iter().map(|p| p.sv(y,y)).fold(String::new(),|mut s,p|{s.push_str(&p);s.push_str(x);s});s.truncate(s.len()-x.len());s}}}}
   fn nsj(x:J)->u32{(x%(1e9 as J)) as u32}fn sj(x:J)->u32{((x%(60*(1e9 as J)))/(1e9 as J))as u32}fn mj(x:J)->u32{((x%(60*60*(1e9 as J)))/(60*(1e9 as J)))as u32}fn hj(x:J)->u32{((x%(24*60*60*(1e9 as J)))/(60*60*(1e9 as J)))as u32}fn dj(x:J)->u32{(x/(24*60*60*(1e9 as J)))as u32}
   fn b(k:G)->R<P>{Ok(P::A(format!("{}",if 0==k{"0"}else{"1"})))}
   fn c(k:C)->R<P>{let mut s=String::new();s.push(k as u8 as char); Ok(P::A(s))}
   fn d(k:I)->R<P>{Ok(P::A(chrono::NaiveDate::from_num_days_from_ce_opt(10957+k).ok_or(E!(format!("bad d: {}",k)))?.to_string()))}
   fn e(k:E)->R<P>{Ok(P::A(format!("{}",k)))}fn f(k:F)->R<P>{Ok(P::A(format!("{}",k)))}fn g(k:G)->R<P>{Ok(P::A(format!("{}",k)))}fn h(k:H)->R<P>{Ok(P::A(format!("{}",k)))}fn i(k:I)->R<P>{Ok(P::A(format!("{}",k)))}fn j(k:J)->R<P>{Ok(P::A(format!("{}",k)))}
   fn m(k:I)->R<P>{let x=k;Ok(P::A(chrono::NaiveDate::from_ymd(2000+x/12,(1+x%12)as u32,1).to_string()))}
   fn n(k:J)->R<P>{let x=k;Ok(P::A(chrono::NaiveTime::from_hms_nano_opt(hj(x),mj(x),sj(x),nsj(x)).ok_or(E!(format!("bad n: {}",x)))?.to_string()))}
   fn p(k:J)->R<P>{let x=k;Ok(P::A(chrono::NaiveDateTime::from_timestamp_opt(60*60*24*(10957+dj(x)as i64)+sj(x)as i64,nsj(x)).ok_or(E!(format!("bad p: {}",x)))?.to_string()))}
   fn v(k:I)->R<P>{let x_=k;let x=x_.abs()as u32;Ok(P::A(format!("{}{}",if x_<0{"-"}else{""},chrono::NaiveTime::from_hms_opt(x/60*60,(x%60*60)/60,x%60).ok_or(E!(format!("bad v: {}{}",if x_<0{"-"}else{""},x)))?)))}
   fn s(k:S)->R<P>{let cs=unsafe{ffi::CStr::from_ptr(k)}; Ok(P::A(format!("`{}",cs.to_str().map_err(|_|E!("utf8 conversion failed"))?)))}
   fn t(k:I)->R<P>{let x_=k;let x=x_.abs()as J*1_000_000;Ok(P::A(format!("{}{}",if x_<0{"-"}else{""},chrono::NaiveTime::from_hms_milli_opt(hj(x),mj(x),sj(x),nsj(x)/1000).ok_or(E!(format!("bad t: {}",x)))?)))}
   fn u(k:I)->R<P>{Ok(P::A(format!("{}",chrono::NaiveTime::from_hms_opt(hj((k as J)*(60e9 as J)),mj((k as J)*(60e9 as J)),0).ok_or(E!(format!("bad u: {}",k)))?)))}
   fn uu(u:[u8;16])->R<P>{let mut s=String::new();for x in u.chunks(4){write!(&mut s,"{}{}{}{}-",x[0],x[1],x[2],x[3])?}s.pop();Ok(P::A(s))}
   fn x(k:G)->R<P>{Ok(P::A(format!("{:x}",k)))}
   fn z(k:F)->R<P>{let x=(k*24.0*60.0*60.0*1e9)as J;Ok(P::A(chrono::NaiveDateTime::from_timestamp_opt(60*60*24*(10957+dj(x)as i64)+sj(x)as i64,nsj(x)).ok_or(E!(format!("bad p: {}",x)))?.to_string()))}
   fn xt(c:&K0,r:&K0)->R<P>{if let P::V(mut v) = xd(c,r)? { let mut v_=vec![P::A("+".into())];v_.append(&mut v);Ok(P::V(v))} else{Err(E!("bad flip"))}}
   fn xd(k:&K0,v:&K0)->R<P>{Ok(P::V(vec![desc(k as K)?,P::A("!".into()),desc(v as K)?]))}
   fn ss<T:Copy>(k:&K0,f:fn(T)->R<P>)->R<P>{tk::<T>(k as K).into_iter().map(|x|f(*x)).collect::<R<_>>().map(|x|P::V(x))}
   fn desc(k_:K)->R<P>
   {if k_.is_null(){return Err(E!("Null ptr"));} let k = unsafe{&*k_}; match ty(k)
    {1=>ss(k,b),2=>ss(k,uu),4=>ss(k,x),5=>ss(k,h),6=>ss(k,i),7=>ss(k,j),8=>ss(k,e),9=>ss(k,f),10=>ss(k,c),11=>ss(k,s),12=>ss(k,p),13=>ss(k,m),14=>ss(k,d),15=>ss(k,z),16=>ss(k,n),17=>ss(k,u),18=>ss(k,v),19=>ss(k,t),
     -1=>b(gK(k)),-2=>uu(tk(k as K)[0]),-4=>x(gK(k)),-5=>h(hK(k)),-6=>i(iK(k)),-7=>j(jK(k)),-8=>e(eK(k)),-9=>f(fK(k)),-10=>c(gK(k) as C),-11=>s(sK(k)),-12=>p(jK(k)),-13=>m(iK(k)),-14=>d(iK(k)),-15=>z(fK(k)),-16=>n(jK(k)),-17=>u(iK(k)),-18=>v(iK(k)),-19=>t(iK(k)),
     0=>ss(k,desc),98=>xt(tk(kK(unsafe{&*kK(k)}))[0],tk(kK(unsafe{&*kK(k)}))[0]),99=>xd(tk(kK(k))[0],tk(kK(k))[0]),_=>Err(E!(format!("{} nyi",ty(k))))}}
   fn flip<T>(v:Vec<Vec<T>>)->Vec<Vec<T>>{let mut w=Vec::new();let mut iters:Vec<_>=v.into_iter().map(|x|x.into_iter()).collect();while let Some(x)=iters.iter_mut().map(|x| x.next()).collect(){w.push(x)}w}
   fn tt(c:&[K0],r:&[K0])->R<P>{let c0=c.into_iter().map(|x|desc(x as K)).collect::<R<Vec<_>>>()?;let r0:Vec<Vec<P>>=r.into_iter().map(|x|desc(x as K).and_then(|x| match x { P::V(v) => Ok(v), _ => Err(E!(format!("Wrong shape table")))})).collect::<R<Vec<_>>>()?; let mut w=vec![P::V(c0)];for r in flip(r0) {w.push(P::V(r))}; Ok(P::V(w))}
   fn dd(k:&[K0],v:&[K0])->R<P>{let k0=k.into_iter().map(|x|desc(x as K)).collect::<R<Vec<_>>>()?;let v0=v.into_iter().map(|x|desc(x as K)).collect::<R<Vec<_>>>()?; Ok(P::V(k0.into_iter().zip(v0.into_iter()).map(|(x,y)| P::V(vec![x,y])).collect()))}
   fn pp(k:&K0,l:usize,c:usize)->R<String>{Err(E!("nyi"))}
   fn csv(k:&K0)->R<String>{panic!()}
   if k.is_null() { return Err(E!("Null ptr")) } let ty = ty(unsafe{&*k});
   match *self { Fmt::Q => { if ty == 10 { Ok(ss(unsafe{&*k},c)?.sv("","")) } else { Err(Err::WithMsg(format!("type: expected string, found {}", ty)))}},
                 Fmt::CSV => csv(unsafe{&*k}),
                 Fmt::PPr(l,c) => pp(unsafe{&*k},l,c) }}}
impl Default for Fmt { fn default() -> Fmt { Fmt::Q }}
enum PP { None, BT, S(String) } impl Default for PP { fn default() -> Self { PP::None }}
impl PP { fn parse(x:&str)->R<PP>{match x.trim(){"bt"=>Ok(PP::BT),"backtrace"=>Ok(PP::BT),"q"=>Ok(PP::None),""=>Ok(PP::None),_=>Ok(PP::S(x.to_owned()))}}
          fn prepare(&self,x:&str)->String{match*self{PP::None=>x.to_owned(),PP::BT=>format!(".Q.trp[eval;parse raze \"{}\";{{raze\"'\",(string x),\"\\n\",.Q.sbt y}}]",x.escape_default()),PP::S(ref fmt)=>fmt.replace('ω', x)}}}
#[derive(Default)]struct Settings { fmt:Fmt, pp:PP }
impl fmt::Display for Err { fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "Err: {}", match *self { Err::DC => "Disconnected".into(), Err::IO(ref io) => format!("IO Error: {}", io), Err::KFail => "K error".into(), Err::Parse(ref x) => if let Some(ref y) = x { format!("Parse error: {:#?}", y) } else { "Parse failure".into() }, Err::Pnd => "Pending response".into(), Err::Wait => "Not yet ready".into(), Err::WithMsg(ref m) => m.clone(), Err::Fmt(ref e) => format!("Formatting error: {}", e) })}}
enum Action
{New { name:String, addr:String, port:u16, auth:String }, Connect { name:Option<String> }, Query { query:String, pp: Option<PP>, fmt: Option<Fmt>},
 Info { name:Option<String> }, Delete { name:Option<String> }, Set { delta:HashMap<String, String> }, Load { path:Option<String> }, Save { path:Option<String> }, List, Help, Ignore, Exit}
fn read(m:bool,tx: sync::mpsc::Sender<String>)->!
{let stdin=io::stdin();let mut l=stdin.lock();
 loop { let mut s=String::new();if m {l.read_to_string(&mut s).unwrap()}else{l.read_line(&mut s).unwrap()};tx.send(s).unwrap(); }}
fn parse(s:String)->R<Action>
{enum P<'a>{A(Action),S(&'a str),E(Err)} impl <'a, F:FnOnce(&'a str)->P<'a>>std::ops::BitOr<F> for P<'a>
 {type Output=Self; fn bitor(self, f:F)->Self::Output { match self { P::A(a)=>P::A(a), P::S(s)=>f(s), P::E(e)=>P::E(e)}}}
 fn P(s:&str,x:&str)->bool{s.starts_with(x)} fn E(x:&str)->bool{x.is_empty()}
 fn kv(s:&str)->Vec<(&str,&str)>{s.split("  ").map(|x| x.trim().split('=').collect()).filter(|x:&Vec<&str>|x.len()==2).map(|x|(x[0],x[1])).collect()}
 macro_rules! X {($x:expr,$y:expr)=>(if let Some(x)=$x{x}else{return P::E(Err::Parse(Some(format!("{}",$y))))})};
 macro_rules! P {($i:ident,$p:expr,$c:expr)=> (fn $i<'a>(s:&'a str)->P<'a>{let x=s.trim();if P(x,$p){let y=x[$p.len()..].trim();P::A($c(if E(y){Some(y.into())}else{None}))}else{P::S(s)}})}
 fn new<'a>(s:&'a str)->P<'a>{
  let x=s.trim();if x.starts_with(":n")
  {let mut i=(&x[2..]).split_whitespace();let name=X!(i.next(),"Missing name").trim().into();let addr=X!(i.next(),"Missing address").into();
   let port=X!(u16::from_str_radix(X!(i.next(),"Missing port"),10).ok(),"Malformed port");
   let mut auth:String=i.next().map(str::to_owned).or(std::env::var("USER").ok()).unwrap_or("kdb".to_string());
   auth.push(':');auth.push_str(i.next().unwrap_or(""));auth.push(3 as char);auth.push(0 as char);
   P::A(Action::New { name, addr, port, auth })}else{P::S(s)}}
 P!(connect,":c",|name|Action::Connect{name});P!(info,":i",|name|Action::Info{name});P!(del,":d",|name|Action::Delete{name});
 P!(load,":ex",|path|Action::Load{path});P!(save,":w",|path|Action::Save{path});
 fn set<'a>(s:&'a str)->P<'a>
 {let x=s.trim();if P(x,":S")
  {let mut delta=HashMap::new();for(k,v)in kv(&x[2..])
   {match k {"fmt"=>delta.insert("fmt".into(),v.into()),"pp"=>delta.insert("pp".into(),v.into()),_=>return P::E(Err::Parse(Some("Unrecognised key".into())))};};
   P::A(Action::Set{delta})} else { P::S(s) }}
 P!(list,":ls",|_:Option<String>|Action::List);P!(help,":h",|_:Option<String>|Action::Help);P!(exit,"\\\\",|_:Option<String>|Action::Exit);
 fn query<'a>(s:&'a str)->P<'a>{if !E(s){P::A(Action::Query{query:s.to_owned(), fmt:None, pp:None})} else {P::S(s)}}
 fn ignore<'a>(s:&'a str)->P<'a>{if s.is_empty(){P::A(Action::Ignore)}else{P::S(s)}}
 fn err<'a>(p:P<'a>)->R<Action>{match p{P::A(a)=>Ok(a),P::E(e)=>Err(e),P::S(s)=>Err(Err::Parse(Some(format!("Unrecognised string: {}",s))))}}
 err(new(&s) | connect | info | del | set | load | save | list | help | ignore | exit | query) }
fn eval(state:&mut State,a:Action)->R<()>
{macro_rules! S (($s:expr)=>($s.servers.get_mut($s.curr.as_ref().ok_or(E!("No server selected"))?).ok_or(E!("No such server"))?);
                 ($s:expr,$n:expr)=>(if let Some(n)=$n{$s.servers.get_mut(n).ok_or(E!("No such server"))?}else{S!($s)}));
 fn home(name:Option<String>)->R<path::PathBuf>{name.map(|x|x.into()).or(env::home_dir().map(|mut x| {x.push(".qsmrc"); x})).ok_or(E!("Could not find home directory"))}
 fn new(state:&mut State,name:String,addr:String,port:u16,auth:String)->R<()>{let s=Server::new(addr,port,auth); state.servers.insert(name,s);Ok(())}
 fn query(state:&mut State, query:String, pp:Option<PP>, fmt: Option<Fmt>)->R<()>{let s=S!(state);let x=pp.as_ref().unwrap_or(&state.settings.pp).prepare(&query); let f=fmt.unwrap_or(state.settings.fmt); s.queue.push_back((x, box move |k| f.pprint(k))); Ok(())}
 fn connect(state:&mut State, name:Option<String>)->R<()>{S!(state,name.as_ref()).conn().map(|_| println!("Connected"))?;if name.is_some(){state.curr=name;}Ok(())}
 fn info(state:&mut State, name:Option<String>)->R<()>{let s=S!(state,name.as_ref());let n=name.as_ref().or(state.curr.as_ref()).unwrap();Ok(println!("{}: {}:{} [{}]",n,s.addr,s.port,s.state))}
 fn del(state:&mut State, name:Option<String>)->R<()>{let n=(if name.is_none() || name == state.curr { state.curr.take() } else { name }).ok_or(E!("No server selected"))?; state.servers.remove(&n).ok_or(E!("No such server."))?; Ok(())}
 fn save(state:&State, name:Option<String>)->R<()>{let p=home(name)?;let mut f=fs::File::create(&p).map_err(|_|E!("Could not open file"))?; for (k,v) in &state.servers { let auth:Vec<&str> = v.auth[0..v.auth.len()-2].split(':').collect(); writeln!(f, ":n {} {} {} {} {}", k, v.addr, v.port, auth[0], auth[1])?;} writeln!(f, ":S fmt={}  pp={}", match state.settings.fmt {Fmt::Q=>"q".into(),Fmt::CSV=>"csv".into(),Fmt::PPr(x,y)=>format!("ppr({},{})",x,y)}, match state.settings.pp { PP::None => "q".into(), PP::BT => "backtrace".to_owned(), PP::S(ref s) => s.clone() })?; if let Some(n) = state.curr.as_ref() { writeln!(f, ":s {}", n)?; } Ok(())}
 fn load(state:&mut State, name:Option<String>)->R<()>{let p=home(name)?;let mut f=fs::File::open(&p).map_err(|_|E!("Could not open file"))?;let mut t=String::new();f.read_to_string(&mut t)?;for l in t.lines() { eval(state, parse(l.to_owned())?)? } Ok(())}
 fn set(state:&mut State, delta:HashMap<String,String>)->R<()>{macro_rules! mask (($s:expr,$map:expr,$k:expr,$p:expr)=>(if let Some(x)=$map.get($k){$s=$p(x)?})); mask!(state.settings.fmt,delta,"fmt",Fmt::parse); mask!(state.settings.pp, delta, "pp", PP::parse); Ok(()) }
 fn list(state:&State)->R<()>{use std::cmp::max;let mut w=[0;3];for(k,v)in&state.servers{w[0]=max(w[0],k.len());w[1]=max(w[1],v.addr.len());w[2]=max(w[2],format!("{}",v.port).len());}for(k,v)in&state.servers{println!("{:w0$} {:w1$}:{:w2$} - {}",k,v.addr,v.port,v.state,w0=w[0],w1=w[1],w2=w[2])}Ok(())}
 fn help()->R<()>{Ok(())}
 fn exit(state:&mut State)->!{ if let Err(e) = eval(state, Action::Save { path: None }) { println!("Error saving qsmrc: {:#?}", e) } std::process::exit(0); }
 match a { Action::New{name,addr,port,auth}=>new(state,name,addr,port,auth),
           Action::Query{query:q,fmt,pp}=>query(state,q,pp,fmt),
           Action::Connect{name}=>connect(state,name),
           Action::Info{name}=>info(state,name),
           Action::Delete{name}=>del(state,name),
           Action::Save{path}=>save(state,path),
           Action::Load{path}=>load(state,path),
           Action::Set{delta}=>set(state,delta),
           Action::List => list(state),
           Action::Help => help(),
           Action::Ignore => Ok(()),
           Action::Exit => exit(state)}}
fn prompt(state:&State)->R<()>{ let mut stdout = io::stdout(); write!(stdout,"{})",if let Some(ref n)=state.curr{n}else{"none"})?; Ok(stdout.flush()?) }
fn init(state:&mut State)->R<()>{ if let Err(e) = eval(state, Action::Load { path: None }) { println!("Error loading init file .qsmrc: {}", e) }; prompt(state) }
fn repl(state:&mut State)->R<()>{ let s = state.input.try_recv().map_err(|_| Err::Wait)?; if let Err(e) = eval(state, parse(s)?) {println!("{}", e);} prompt(state) }
fn resp(state:&mut State)->R<()>{ for (k,v) in &mut state.servers { if v.state == SState::Pnd { match v.recv() { Ok(msg) => { println!("[Received message from {}]",k); state.resp.push((k.clone(),msg)) }, Err(Err::Wait) => (), Err(e) => println!("{}", e) }}} Ok(()) }
fn send(state:&mut State)->R<()>{ for (_,v) in &mut state.servers { match v.state { SState::Cnn => if let Some((msg, post)) = v.queue.pop_front() { v.send(&msg, post)? }, SState::Hndshk => v.hndshk()?, _ => () }} Ok(()) }
fn main()
{let flags = Flags::flags(); let (tx, rx) = sync::mpsc::channel(); let _t=thread::spawn(move || read(flags.multiline,tx));
 let mut state = State { servers: HashMap::new(), resp: Vec::new(), settings: Settings::default(), curr: None, input: rx };
 init(&mut state).unwrap(); loop { repl(&mut state).or_else(|_| resp(&mut state)).or_else(|_| send(&mut state)).unwrap_or_else(|_| thread::sleep(time::Duration::from_millis(100))) }}
