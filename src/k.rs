use libc;
use chrono;
use u::*;
use std::{fmt, marker, slice, ffi, str};
use std::ops::Deref;

pub const K_VOID: *const K = 0 as *const K;
pub type S=*const libc::c_char;

#[derive(PartialEq, Eq)]
pub struct K(pub *const K0);

unsafe impl marker::Send for K {}

impl Deref for K
{
    type Target = K0;
    fn deref(&self) -> &K0
    {
        unsafe { &*(self.0) }
    }
}

impl fmt::Debug for K
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        self.0.fmt(f)
    }
}

// this shouldn't be sized
// but right now rust untagged unions cannot have unsized members
#[repr(C)]
#[derive(Debug)]
pub struct KA
{
    n: libc::c_long,
    g0: [u8; 1],
}

#[repr(C)]
pub union KU
{
    g: libc::c_schar,
    h: libc::c_short,
    i: libc::c_int,
    j: libc::c_long,
    e: libc::c_float,
    f: libc::c_double,
    s: S,
    k: *const K0,
    v: KA,
}

#[repr(C)]
pub struct K0
{
    pub m: libc::c_schar,
    pub a: libc::c_schar,
    pub t: libc::c_schar,
    pub u: libc::c_char,
    pub r: libc::c_int,
    pub data: KU,
}

impl fmt::Debug for K0
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f,
               "K@{:p} m: {:?}, a: {:?}, t: {:?}, u: {:?}, r: {:?}",
               &self,
               self.m,
               self.a,
               self.t,
               self.u,
               self.r)
    }
}

#[link(name="kdb")]
extern "C" {
    pub fn khp(addr: S, port: i32) -> i32;
    pub fn khpu(addr: S, port: i32, auth: S) -> i32;
    pub fn k(hande: i32, query: S, ...) ->*const K0;
    pub fn jv(x:*const *const K0,y:*const K0)->*const K0;
    pub fn r0(x:*const K0);
    pub fn kclose(handle: i32);
}

unsafe fn as_vector<'a, T: 'a>(x: *const K0) -> &'a [T]
{
    let len = (*x).data.v.n as usize;
    let ptr = (*x).data.v.g0.as_ptr() as *const T;

    slice::from_raw_parts(ptr, len)
}

unsafe fn get<'a, T: 'a>(x: *const K0, n: usize) -> &'a T
{
    &as_vector(x)[n]
}

pub fn pprint(x:K,lines:usize,cols:usize) -> String
{
    use chrono::{TimeZone,Datelike,Timelike};
    fn js<'a, T:'a+ToString>(s:&mut String, x: &'a [T], j: &str) {
        if x.len()==1{s.push(',');s.push_str(x[0].to_string().as_ref())}
        else{for y in x{s.push_str(y.to_string().as_ref());s.push_str(j);}
             let n=s.len()-j.len();s.truncate(n);}}
    fn r1(s: &mut String, x: &[i8]) {
        for b in x {if *b == 0 {s.push('0');} else { s.push('1');}}
        s.push('b');}
    fn r4(s: &mut String, x: &[i8]) {
        s.push_str("0x"); for b in x {
            s.push(b"0123456789abcdef"[(((*b as u8)>>4)) as usize] as char);
            s.push(b"0123456789abcdef"[((*b as u8)&15) as usize] as char);}}
    fn r12(s:&mut String,x:&[i32]){
        for d in x{s.push_str(dfmt(*d).as_ref());s.push_str(" ");}
        let n=s.len()-" ".len();s.truncate(n);}
    fn r14(s:&mut String,x:&[i64]){
        for p in x{s.push_str(pfmt(*p).as_ref());s.push_str(" ");}
        let n=s.len()-" ".len();s.truncate(n);}
    fn pfmt(x:i64)->String {
        use std::iter::repeat;use std::cmp::max;
        let d=(x/8.64e13f64 as i64+10957)*(8.64e4f64 as i64);
        let s=(x%8.64e13 as i64)/1e9 as i64;
        let n=(x%1e9f32 as i64)as u32;
        let t=chrono::Utc.timestamp(d+s,n);
        let np:String=repeat("0")
            .take(8-((max(t.nanosecond(),1) as f64)
                  .log(10f64).floor() as usize))
            .collect();
        format!("{}.{}{}.{}{}D{}{}:{}{}:{}{}.{}{}",t.year(),
                if t.month() < 10 { "0" } else { "" }, t.month(),
                if t.day() < 10 { "0" } else { "" }, t.day(),
                if t.hour() < 10 { "0" } else { "" }, t.hour(),
                if t.minute() < 10 { "0" } else { "" }, t.minute(),
                if t.second() < 10 { "0" } else { "" }, t.second(),
                np,t.nanosecond())}
    fn dfmt(x:i32)->String{
        use std::ops::Add;
        let d=chrono::NaiveDate::from_ymd(2000,1,1)
            .add(chrono::Duration::days(x as i64));
        format!("{}.{}{}.{}{}",d.year(),
                if d.month() < 10 { "0"}else{""}, d.month(),
                if d.day() < 10 { "0"}else{""}, d.day())}
    fn sym(x: S) -> &'static str {
        let cstr = unsafe { ffi::CStr::from_ptr(x) };
        cstr.to_str().unwrap()}
    fn string(x:*const K0) -> &'static str {
        unsafe {let bytes = as_vector::<u8>(x);
            str::from_utf8_unchecked(bytes)}}
    fn p0(s:&mut String,x:*const K0){
        unsafe{match (*x).t{
            -128=>s.push_str(&format!("'{}",sym((*x).data.s))),
            -12=>s.push_str(&pfmt((*x).data.j)),
            -14=>s.push_str(&dfmt((*x).data.i)),
            -11=>s.push_str(&format!("`{}",sym((*x).data.s))),
            -10=>s.push_str(&format!("{}",(*x).data.g as u8 as char)),
            -9=>s.push_str(&format!("{}",(*x).data.f)),
            -8=>s.push_str(&format!("{}",(*x).data.e)),
            -7=>s.push_str(&format!("{}",(*x).data.j)),
            -6=>s.push_str(&format!("{}",(*x).data.i)),
            -5=>s.push_str(&format!("{}",(*x).data.h)),
            -4=>s.push_str(&format!("0x{:x}",(*x).data.g)),
            -1=>s.push_str(&format!("{}b",if 0==(*x).data.g{0}else{1})),
            0=>{s.push('(');for k in as_vector::<*const K0>(x){
                    p0(s,*k);s.push(';');}s.pop();s.push(')');},
            1=>r1(s,as_vector::<i8>(x)),
            4=>r4(s,as_vector::<i8>(x)),
            5=>js(s,as_vector::<i16>(x)," "),
            6=>js(s,as_vector::<i32>(x)," "),
            7=>js(s,as_vector::<i64>(x)," "),
            8=>js(s,as_vector::<f32>(x)," "),
            9=>js(s,as_vector::<f64>(x)," "),
            10=>{s.push('"');
                 let v=string(x).replace('\n', r"\n");
                 s.push_str(&v);s.push('"');},
            11=>for p in as_vector::<S>(x){s.push('`');s.push_str(sym(*p));},
            14=>{s.push('(');for d in as_vector::<i32>(x)
                 {s.push_str(dfmt(*d).as_ref());s.push(';');}
                 s.pop();s.push(')');},
            12=>{s.push('(');for p in as_vector::<i64>(x)
                 {s.push_str(pfmt(*p).as_ref());s.push(';');}
                 s.pop();s.push(')');},
            98=>t0(s,*get::<*const K0>((*x).data.k,0),
                   *get::<*const K0>((*x).data.k,1)),
            99=>d0(s,*get::<*const K0>(x,0),*get::<*const K0>(x,1)),
            100=>s.push_str(string(x)),
            101=>s.push_str("::"),
            t=>{s.clear();s.push_str(&format!("Cannot pprint type {}",t));}
        }};}
    fn t0(s:&mut String,n:*const K0,c:*const K0)
    {unsafe{s.push('+');for p in as_vector::<S>(n)
            {s.push('`');s.push_str(sym(*p));}
        s.push('!');p0(s,c);}}
    fn d0(s:&mut String,k:*const K0,v:*const K0){p0(s,k);s.push('!');p0(s,v);}
    fn t1(s:&mut String,n:*const K0,c:*const K0,
          k:usize,lines:usize,cols:usize){unsafe{
        use std::cmp::{min,max};let mut v=Vec::new();let mut a=Vec::new();
              for (c,d) in as_vector::<S>(n).iter()
                  .zip(as_vector::<*const K0>(c).iter()){
            let mut cl=Vec::new();let cn=sym(*c).to_owned();
            a.push(cn.len());cl.push(cn);match(**d).t{
                1=>for b in as_vector::<i8>(*d)
                {cl.push(format!("{}b",if *b==0{0}else{1}));},
                4=>for b in as_vector::<i8>(*d){cl.push(format!("{:x}",*b));},
                5=>for b in as_vector::<i16>(*d){cl.push(format!("{}",*b));},
                6=>for b in as_vector::<i32>(*d){cl.push(format!("{}",*b));},
                7=>for b in as_vector::<i64>(*d){cl.push(format!("{}",*b));},
                8=>for b in as_vector::<f32>(*d){cl.push(format!("{}",*b));},
                9=>for b in as_vector::<f64>(*d){cl.push(format!("{}",*b));},
                11=>for b in as_vector::<S>(*d){cl.push(sym(*b).to_owned());},
                10=>for b in as_vector::<u8>(*d)
                    {cl.push(format!("{}",*b as char));},
                12=>for b in as_vector::<i64>(*d){cl.push(pfmt(*b));},
                14=>for b in as_vector::<i32>(*d){cl.push(dfmt(*b));},
                0=>for b in as_vector::<*const K0>(*d)
                   {let mut t=String::new();p0(&mut t,*b);cl.push(t);},
                t=>{s.clear();s.push_str(&format!("Cannot print type {}", t));
                    return;}}
                v.push(cl);}
        for(l,c)in a.iter_mut().zip(v.iter()){for s in c{*l=max(*l,1+s.len());}}
        let num_lines=v.first().map(Vec::len).unwrap_or(0);

        let mut sz=a.iter().sum(); if k != 0 { sz += 2; }

        let iter:Vec<_>=v.into_iter()
            .map(|x| box x.into_iter() as Box<Iterator<Item=String>>)
            .collect();

        let num_cols = a.len();
        let key_col = a.iter().take(k).sum();
        let mut current_line = String::new();
        for (line, column, value, alignment) in round_robin(iter)
            .zip(a.iter().cycle())
            .enumerate()
            .take(num_cols * min(2 + num_lines, lines))
            .map(|(x,(y,a))| (x / num_cols, x % num_cols, y, a)) {

                if column == 0 && line > 0 {
                    if current_line.len() > cols {
                        current_line.truncate(cols - 1);
                        current_line.push_str("..");
                    }
                    current_line.push('\n');
                    s.push_str(&current_line);
                    current_line.clear();
                }

                if line == 1 && column == 0 {

                    for i in 0..min(sz, cols - 2) {
                        if k != 0 {
                            if i == key_col {
                                s.push('|');
                            } else if i == key_col + 1 {
                                s.push(' ');
                            } else {
                                s.push('-');
                            }
                        } else {
                            s.push('-');
                        }
                    }
                    if sz > cols {
                        s.push_str("..");
                    }
                    s.push('\n');

                }

                if k != 0 && column == k {
                    current_line.push_str("| ");
                }

                let padding = alignment - value.len();
                current_line.push_str(&value);
                for _ in 0..padding {
                    current_line.push(' ');
                }
            }
        if current_line.len() > cols {
            current_line.truncate(cols - 1);
            current_line.push_str("..");
        }
        current_line.push('\n');
        s.push_str(&current_line);
        current_line.clear();
        if num_lines > lines { s.push_str("..\n"); }
        }}
    fn d1(s:&mut String,k:*const K0,v:*const K0,lines:usize,cols:usize){unsafe{
        let mut ks=Vec::new();let mut vs=Vec::new();
        match(*k).t{
            1=>for b in as_vector::<i8>(k)
            {ks.push(format!("{}",if*b==0{0}else{1}))},
            4=>for b in as_vector::<i8>(k){ks.push(format!("{:x}",*b))},
            5=>for b in as_vector::<i16>(k){ks.push(format!("{}",*b));},
            6=>for b in as_vector::<i32>(k){ks.push(format!("{}",*b));},
            7=>for b in as_vector::<i64>(k){ks.push(format!("{}",*b));},
            8=>for b in as_vector::<f32>(k){ks.push(format!("{}",*b));},
            9=>for b in as_vector::<f64>(k){ks.push(format!("{}",*b));},
            11=>for b in as_vector::<S>(k){ks.push(sym(*b).to_owned());},
            10=>for b in as_vector::<u8>(k)
            {ks.push(format!("{}",*b as char));},
            12=>for b in as_vector::<i64>(k){ks.push(pfmt(*b));},
            14=>for b in as_vector::<i32>(k){ks.push(dfmt(*b));},
            0=>for b in as_vector::<*const K0>(k)
            {let mut t=String::new();p0(&mut t,*b);ks.push(t);},
            t=>{s.clear();s.push_str(&format!("Cannot print type {}", t));
                return;}}
        match(*v).t{
            1=>for b in as_vector::<i8>(v)
            {vs.push(format!("{}",if*b==0{0}else{1}))},
            4=>for b in as_vector::<i8>(v){vs.push(format!("{:x}",*b))},
            5=>for b in as_vector::<i16>(v){vs.push(format!("{}",*b));},
            6=>for b in as_vector::<i32>(v){vs.push(format!("{}",*b));},
            7=>for b in as_vector::<i64>(v){vs.push(format!("{}",*b));},
            8=>for b in as_vector::<f32>(v){vs.push(format!("{}",*b));},
            9=>for b in as_vector::<f64>(v){vs.push(format!("{}",*b));},
            11=>for b in as_vector::<S>(v){vs.push(sym(*b).to_owned());},
            10=>for b in as_vector::<u8>(v)
            {vs.push(format!("{}",*b as char));},
            12=>for b in as_vector::<i64>(v){vs.push(pfmt(*b));},
            14=>for b in as_vector::<i32>(v){vs.push(dfmt(*b));},
            0=>for b in as_vector::<*const K0>(v)
            {let mut t=String::new();p0(&mut t,*b);vs.push(t);},
            t=>{s.clear();s.push_str(&format!("Cannot print type {}", t));
                return;}}
        for (i, (k,v)) in ks.iter().zip(vs.iter()).enumerate().take(lines) {
            s.push_str(k);
            s.push_str("| ");
            s.push_str(v);
            if s.len() > (i+1)*cols {
                s.truncate((i+1)*cols-2);
                s.push_str("..")
            }
            s.push('\n');
        }
    }}
    fn t2(s:&mut String,l:*const K0,r:*const K0,lines:usize,cols:usize){unsafe{
        let k=as_vector::<S>(*get::<*const K0>((*l).data.k,0)).len();
        let c=jv(get::<*const K0>((*l).data.k,0),
                 *get::<*const K0>((*r).data.k,0));
        let v=jv(get::<*const K0>((*l).data.k,1),
                 *get::<*const K0>((*r).data.k,1));
        t1(s,c,v,k,lines,cols);r0(c);r0(v);}}
    let x = x.0; if x.is_null() { return "nullptr".to_owned() }
    let mut s=String::new();
    unsafe{match (*x).t{
        0=>{let mut t=String::new();for k in as_vector::<*const K0>(x)
            .iter().take(lines){p0(&mut t,*k);t.truncate(cols);
            s.push_str(&t);t.clear();s.push('\n');}
            if as_vector::<K>(x).len()>lines{s.push_str("...\n");}r0(x);},
        10=>{s.push_str(string(x));r0(x);},
        14=>{r14(&mut s,as_vector::<i64>(x));r0(x);},
        12=>{r12(&mut s,as_vector::<i32>(x));r0(x);},
        98=>{t1(&mut s,*get::<*const K0>((*x).data.k,0),
                *get::<*const K0>((*x).data.k,1),0,lines,cols);r0(x);},
        99=>{let k=*get::<*const K0>(x,0);let v=*get::<*const K0>(x,1);
             if((*k).t==98)&&(*v).t==98{t2(&mut s,k,v,lines,cols)}
             else{d1(&mut s,k,v,lines,cols);r0(x);}},
        _=>{p0(&mut s,x);s.truncate(cols);r0(x);}}}
    s
}
