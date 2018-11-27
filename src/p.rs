#![allow(non_snake_case)]
use std::fmt::{Write,Display};
use chrono::{NaiveDate,NaiveTime,Duration};
use std::{ffi,str,cmp,iter};
use k;

const N:usize = 1_000;

pub fn pr(l:usize,c:usize,x: k::K) {

    print!("\r{:width$}",' ',width=c);

    if x.is_null() { println!("\rNullptr"); return }
    let x = unsafe{ &*x };

    match k::t(x) {
        0 => {
            match prim(N,x) {
                P::M(m) => println!("\r{}",join_with("\n",m)),
                _ => panic!("prim on 0 list returned simple vec or atom")
            }
        },
        99 => print!("\r{}",dict(N,l,c,x)),
        98 => print!("\r{}",tab(N,l,c,x)),
        _ => {
            let s = nest(N,x);
            if (s.len() < c) || (s.starts_with('{') && s.ends_with('}')) {
                println!("\r{}", s);
            } else {
                println!("\r{}...", &s[..c-3]);
            }
        }
    };

    unsafe { k::r0(x) }
}

enum P { A(String), V(Option<&'static str>, Option<&'static str>,Option<&'static str>, Vec<P>), M(Vec<P>) }
fn b(x:u8)->P{P::A(format!("{}",if x!=0{1}else{0}))}
fn guid(x: &[u8])->P{
    let mut s = String::new();
    for (i,b) in x.iter().enumerate()
    { write!(&mut s, "{:x}", b); if 3 == (i % 4) { s.push('-'); } }
    s.pop();
    P::A(s)
}
fn x0(x: k::G)->P{P::A(format!("{:x}",x))}
fn auto<T: Display>(x:T)->P{P::A(format!("{}",x))}
fn s(x: k::S) -> P { P::A(unsafe { ffi::CStr::from_ptr(x).to_str().unwrap_or("Invalid utf8").into() }) }
fn p(x: k::J) -> P { P::A(format!("{}", NaiveDate::from_ymd(2000,1,1).and_hms(0,0,0) + Duration::nanoseconds(x))) }
fn m(x: k::I) -> P { P::A(format!("{}.{}",2000+(x/12),1+(x%12))) }
fn d(x: k::I) -> P {P::A(format!("{}",NaiveDate::from_ymd(2000,1,1)+Duration::days(x as i64)))}
fn z(x: k::F) -> P {
    let d = NaiveDate::from_ymd(2000,1,1).and_hms(0,0,0)+Duration::days(x.trunc() as i64);
    let t = Duration::nanoseconds((x.fract() * (Duration::days(1).num_nanoseconds().unwrap() as f64)) as i64);
    P::A(format!("{}", d + t))
}
fn n(x: k::J) -> P { P::A(format!("{}", Duration::nanoseconds(x))) }
fn u(x: k::I) -> P { P::A(format!("{}", NaiveTime::from_hms(0,0,0)+Duration::minutes(x as i64))) }
fn v(x: k::I) -> P { P::A(format!("{}", NaiveTime::from_hms(0,0,0)+Duration::seconds(x as i64))) }
fn t(x: k::I) -> P {
    let mm=(x%1000)as u32; let s=(x/1000)as u32; let m=(s/60)as u32; let h=(m/60)as u32;
    if let Some(t) = NaiveTime::from_hms_milli_opt(h%24,m%60,s%60,mm) {P::A(format!("{}",t))} else {P::A(format!("bad t {}",x))} }

macro_rules! KA {
    ($k:expr,$ac:expr) => (auto($ac($k))) ;
    ($k:expr,$ac:expr,$at:expr) => ($at($ac($k)))
}

macro_rules! KV {
    ($k:expr,$num:expr,$tt:ty) => (P::V(None,None,None,k::tk::<$tt>($k).iter().take($num).map(|x|auto(*x)).collect())) ;
    ($k:expr,$num:expr,$tt:ty,$at:expr) => (P::V(None,None,None,k::tk::<$tt>($k).iter().take($num).map(|x|$at(*x)).collect()));
    ($k:expr,$num:expr,$tt:ty,$at:expr,$sep:expr,$pre:expr,$suf:expr) =>
        (P::V(Some($sep),Some($pre),Some($suf),k::tk::<$tt>($k).iter().take($num).map(|x|$at(*x)).collect()))
}

fn prim(num_items:usize, x:&k::K0)->P{
    match k::t(x) {
        0 =>P::M(k::tk::<k::K>(x).iter().take(num_items).map(|x|unsafe{prim(num_items, &**x)}).collect()),
        -1=>KA!(x,k::gK,b),1=>KV!(x,num_items,u8,b,"","","b"),
        -2=>guid(k::tk::<u8>(x)),2=>P::V(None,None,None,k::tk::<u8>(x).chunks(16).take(num_items).map(guid).collect()),
        -4=>KA!(x,k::gK,x0),4=>KV!(x,num_items,u8,x0,"","0x",""),
        -5=>KA!(x,k::hK),5=>KV!(x,num_items,k::H),
        -6=>KA!(x,k::iK),6=>KV!(x,num_items,k::I),
        -7=>KA!(x,k::jK),7=>KV!(x,num_items,k::J),
        -8=>KA!(x,k::eK),8=>KV!(x,num_items,k::E),
        -9=>KA!(x,k::fK),9=>KV!(x,num_items,k::F),
        -10=>KA!(x,|x|k::gK(x)as char),10=>KV!(x,num_items,u8,|x:u8|auto(x as char),"","",""),
        -11=>KA!(x,k::sK,s),11=>KV!(x,num_items,k::S,s,"`","`",""),
        -12=>KA!(x,k::jK,p),12=>KV!(x,num_items,k::J,p),
        -13=>KA!(x,k::iK,m),13=>KV!(x,num_items,k::I,m),
        -14=>KA!(x,k::iK,d),14=>KV!(x,num_items,k::I,d),
        -15=>KA!(x,k::fK,z),15=>KV!(x,num_items,k::F,z),
        -16=>KA!(x,k::jK,n),16=>KV!(x,num_items,k::J,n),
        -17=>KA!(x,k::iK,v),17=>KV!(x,num_items,k::I,v),
        -18=>KA!(x,k::iK,u),18=>KV!(x,num_items,k::I,u),
        -19=>KA!(x,k::iK,t),19=>KV!(x,num_items,k::I,t),
        98=>P::A(nest(num_items,x)), 99 => P::A(nest(num_items,x)),
         x if ((x > -80) && (x < -19)) || ((x > 19) && (x < 80)) => P::A(format!("err: recv type {}, enums unexpected", x)),
        -128 => KA!(x,k::sK,|x| match s(x){P::A(s)=>P::A(format!("'{}",s)),_=>panic!()}),
        100=>P::A(str::from_utf8(k::tk::<u8>(x)).unwrap_or("'utf8").into()),
        101=>P::A("::".into()),
        x => P::A(format!("Unrecognised type {}", x))
    }
}

fn nest(n:usize,x:&k::K0)->String{
    match k::t(x) {
        98=>format!("+{}",nest(n,unsafe{&*k::kK(x)})),
        99=>format!("{}!{}",nest(n,unsafe{&*k::tk::<k::K>(x)[0]}),nest(n,unsafe{&*k::tk::<k::K>(x)[1]})),
        _=> match prim(n,x) {
            P::A(x) => x,
            P::V(Some(ds),Some(pre),Some(suf),x) => format!("{}{}{}",pre,join_with(ds,x),suf),
            P::V(_,_,_,x) => join_with(" ",x),
            P::M(x) => join_with(";",x)
        }
    }
}

fn plen(p:&P)->usize{
    match *p {
        P::A(ref a) => a.len(),
        P::V(Some(ref s),Some(ref pre), Some(ref suf), ref v) => v.iter().map(plen).sum::<usize>() + s.len() * (v.len() - 1) + pre.len() + suf.len(),
        P::V(_,_,_,ref v) => v.iter().map(plen).sum::<usize>() + v.len() - 1, // rendered: 1 2 3
        P::M(ref m) => m.iter().map(plen).sum::<usize>() + m.len() + 1  // rendered: (1;`abc;2)
    }
}


fn tab(n:usize, l:usize, c:usize, x:&k::K0)->String {

    let cols: Vec<String> = match prim(n, unsafe { &*k::tk::<k::K>(&*k::kK(x))[0] }) {
        P::V(_,_,_,ps) => ps.into_iter().map(|x| match x { P::A(s) => s, _ => panic!() }).collect(),
        _ => panic!()
    };

    let values: Vec<P> = match prim(n, unsafe { &*k::tk::<k::K>(&*k::kK(x))[1] }) {
        P::V(_,_,_,ps) => ps,
        P::M(ms) => ms,
        P::A(_) => panic!("unexpected atom in table!")
    };
    assert_eq!(cols.len(), values.len());

    let widths: Vec<usize> = cols.iter()
        .map(String::len)
        .zip(values.iter().map(|x| {
            match *x {
                P::A(_) => panic!("Unexpected atom in table"),
                P::M(ref y) | P::V(_,_,_,ref y) => y.iter().map(plen).max().unwrap_or(1)
            }
        }))
        .map(|(x,y)|cmp::max(x,y))
        .collect();

    assert_eq!(cols.len(), widths.len());

    let num_rows = match values[0] {
        P::A(_) => panic!("unexpected atom in table"),
        P::M(ref m) => m.len(),
        P::V(_,_,_,ref v)=>v.len()
    };

    let mut s = String::new();
    let mut ln = String::with_capacity(c);

    for (c0, w) in cols.iter().zip(widths.iter()) {
        write!(&mut ln, "{:width$} ", c0, width = w);
    }

    let len = ln.len();

    if ln.len() > c {
        writeln!(&mut s, "{}...", &ln[..c-3]);
    } else {
        writeln!(&mut s, "{}", ln);
    }

    if len > c {
        for c in iter::repeat('-').take(c-3) {
            s.push(c);
        }
        s.push_str("...");
    } else {
        for c in iter::repeat('-').take(len) {
            s.push(c);
        }
    }
    s.push('\n');

    let mut iters: Vec<_> = values.into_iter().map(|x| match x {
        P::V(_,_,_,vs) => vs.into_iter(),
        P::M(ms) => ms.into_iter(),
        P::A(_) => panic!("unexpected atom in table!")
    }).collect();

    for _ in 0..l {
        ln.clear();
        for (i, w) in widths.iter().enumerate() {
            let iter = &mut iters[i];
            match iter.next() {
                Some(P::A(a)) => write!(&mut ln, "{:width$}", a, width = w + 1),
                Some(P::V(Some(s),Some(pre),Some(suf),v)) => {
                    let body = join_with(s,v);
                    write!(&mut ln, "{}{}{}{:width$}", pre, body, suf, "", width = *w - cmp::max(*w + 1,pre.len() - suf.len() - body.len()))
                },
                Some(P::V(_,_,_,v)) => write!(&mut ln, "{:width$}", join_with(" ", v), width = w + 1),
                Some(P::M(m)) => {
                    let m0 = join_with(";", m);
                    write!(&mut ln, "({}){:width$}", m0, " ", width = w - m0.len() - 1)
                },
                None => Ok(())
            }.unwrap();
        }

        if ln.len() > c {
            writeln!(&mut s, "{}...", &ln[..c-3]);
        } else if ln.len() > 0 {
            writeln!(&mut s, "{}", ln);
        }
    }
    if l < num_rows {
        writeln!(&mut s, "..");
    }

    s
}

fn key(n:usize, l:usize, c:usize, x:&k::K0,y:&k::K0) -> String {
    let left = tab(n / 2, l, c, x);
    let right = tab(n / 2, l, c, y);

    let mut s = String::with_capacity(l * c);
    let mut ln = String::new();

    let len = left.lines().count();

    for (left_line, right_line) in left.lines().zip(right.lines()).take(if len > l { l - 1 } else { len }) {
        ln.clear();
        write!(&mut ln, "{}| {}", left_line, right_line);
        if ln.len() > c {
            writeln!(&mut s, "{}...", &ln[..c-3]);
        } else {
            writeln!(&mut s, "{}", ln);
        }
    }

    if len > l {
        writeln!(&mut s, "..");
    }

    s
}

fn dict(n:usize, l:usize, c:usize, x:&k::K0)->String {
    let keys = unsafe { &*k::tk::<k::K>(x)[0] };
    let vals = unsafe { &*k::tk::<k::K>(x)[1] };

    if (k::t(keys) == 98) && (k::t(vals) == 98) {
        return key(n, l, c, keys, vals);
    }

    let keys: Vec<P> = match prim(n, keys) {
        P::A(_) => panic!("Cannot have atom as dict key"),
        P::M(x) | P::V(_,_,_,x) => x
    };

    let vals: Vec<P> = match prim(n, vals) {
        P::A(_) => panic!("cannot have atom as dict val"),
        P::M(x) | P::V(_,_,_,x) => x
    };

    let n = keys.len();

    let key_width = keys.iter().map(plen).max().unwrap_or(1);
    let val_width = vals.iter().map(plen).max().unwrap_or(1);

    let mut s = String::new();
    let mut ln = String::new();

    for (k, v)  in keys.into_iter().zip(vals.into_iter()).take(if n <= l { l } else { l - 1 }) {
        ln.clear();
        write!(&mut ln, "{:kw$}| {:vw$}", render(k), render(v), kw = key_width, vw = val_width);
        if ln.len() > c {
            writeln!(&mut s, "{}...", &ln[..c-3]);
        } else {
            writeln!(&mut s, "{}", ln);
        }
    }

    if n > l {
        writeln!(&mut s, "..");
    }

    s
}

fn render(x:P)->String{
    match x {
        P::A(a) => a,
        P::V(Some(""), Some(""), Some(""), vs) => {
            let mut s = String::new();
            for v in vs { write!(&mut s, "{}", render(v)).unwrap() }
            s
        }
        P::V(Some(sep), Some(pre), Some(suf), vs) => {
            let mut s = String::new();
            let n = vs.len();
            for v in vs {
                write!(&mut s, "{}{}{}{}", pre, render(v), suf, sep).unwrap();
            }
            if n>0 {s.truncate(s.len()-sep.len())}
            s
        },
        P::V(_,_,_,vs)=>join_with(" ",vs),
        P::M(ms) => join_with(";",ms)
    }
}

fn join_with(sep: &str, x: Vec<P>) -> String {
    let mut s = String::new();
    let n = x.len();
    for p in x {
        match p {
            P::A(y) => write!(&mut s, "{}{}", y, sep).unwrap(),
            P::V(Some(ds), Some(pre), Some(suf), y) => write!(&mut s, "({}{}{}){}", pre, join_with(ds,y), suf, sep).unwrap(),
            P::V(_, _, _, y) => write!(&mut s, "({}){}", join_with(";",y), sep).unwrap(),
            P::M(y) => write!(&mut s, "({}){}", join_with(";",y), sep).unwrap(),
        }
    }

    if n > 0 {
        s.truncate(s.len() - sep.len());
    }

    s
}
