use chrono::{NaiveDate, NaiveTime, Duration};
use std::{ffi, str, fmt::{Write, Display}, f64, cmp};
use k;

const N: usize = 100;
const NULL_INT: i32 = -2147483648;
const NULL_LONG: i64 = -9223372036854775808;

pub fn pretty_print(lines: usize, cols: usize, x: k::K) {
    print!("\r{:width$}", ' ', width = cols);
    if x.is_null() {
        println!("\rnullptr");
        return
    }

    let x = unsafe { &*x };

    match k::t(x) {
        98 => { let x = k::kK(x); unsafe { let ks = k::tk::<k::K>(x); println!("\r{}", table(lines, cols, &*ks[0], &*ks[1])) } },
        99 => { unsafe { let ks = k::tk::<k::K>(x); println!("\r{}",  dict(lines, cols, &*ks[0], &*ks[1])) } },
        0 => {
            if let P::M(m) = format(x) {
                print!("\r");
                for x in m {
                    let s = render(x);
                    if s.len() > cols {
                        println!("{}..", &s[..cols-2]);
                    } else {
                        println!("{}", s);
                    }
                }
            } else {
                println!("\rinternal err: format 0h not P::M")
            }
        },
        t => {
            let s = render(format(x));
            if t == 10 && s.starts_with("\"{") && s.ends_with("}\"") {
                println!("\r{}", &s[1..s.len()-1]);
            } else if t == 10 && s.starts_with('\'') {
                println!("\r{}", s);
            } else {
                for (i, l) in s.lines().enumerate() {
                    if i > lines {
                        println!("..");
                        return;
                    }

                    if l.len() > cols {
                        println!("\r{}..", &l[..cols-2]);
                    } else {
                        println!("\r{}", l);
                    }
                }
            }
        },
    }
}

#[derive(Debug, Clone)]
enum P {
    A(String),
    M(Vec<P>),
    V {
        prefix: &'static str,
        infix: &'static str,
        suffix: &'static str,
        ps: Vec<P>
    }
}

fn render(p: P) -> String {
    match p {
        P::A(a) => a,
        P::M(m) => {
            let mut s = String::new();
            for x in m {
                s.push_str(&render(x));
                s.push(';');
            }

            if s.len() > 0 {
                s.truncate(s.len() - 1);
            }

            s
        },
        P::V { prefix, infix, suffix, ps } => {
            if ps.is_empty() {
                String::new()
            } else {
                let mut s: String = prefix.into();
                for x in ps {
                    s.push_str(&render(x));
                    s.push_str(infix);
                }
                s.truncate(s.len() - infix.len());
                s.push_str(suffix);
                s
            }
        }
    }
}

fn dict(lines: usize, cols: usize, k: &k::K0, v: &k::K0) -> String {
    if (k::t(k) == 98) && (k::t(k) == 98) {
        return keyed(lines, cols, k, v)
    }

    let keys = match format(k) {
        P::A(s) => vec!(s),
        P::M(m) => m.into_iter().map(|x| render(x)).collect(),
        P::V { prefix: _, infix: _, suffix: _, ps } => ps.into_iter().map(|x| render(x)).collect()
    };

    let vals = match format(v) {
        P::A(s) => vec!(s),
        P::M(m) => m.into_iter().map(|x| render(x)).collect(),
        P::V { prefix: _, infix: _, suffix: _, ps } => ps.into_iter().map(|x| render(x)).collect()
    };

    let key_width = keys.iter().map(String::len).max().unwrap_or(1);
    let val_width = vals.iter().map(String::len).max().unwrap_or(1);

    let mut s = String::new();
    let mut l = String::new();
    let n = keys.len();

    for (k, v) in keys.into_iter().zip(vals.into_iter()).take(if n > lines { n - 1 } else { n }) {
        write!(&mut l, "{:key_width$}| {:val_width$}", k, v, key_width = key_width, val_width = val_width).unwrap();
        if l.len() > cols {
            writeln!(&mut s, "{}..", &l[..cols-2]).unwrap();
        } else {
            writeln!(&mut s, "{}", l).unwrap();
        }
    }

    s
}

fn table(lines: usize, cols: usize, k: &k::K0, v: &k::K0) -> String {
    fn write_line(cols: usize, from: &str, to: &mut String) {
        if from.len() > cols {
            writeln!(to, "{}..", &from[..cols]).unwrap()
        } else {
            writeln!(to, "{}", from).unwrap()
        }
    }

    let names: Vec<String> = match format(k) {
        P::A(s) => vec!(s),
        P::M(m) => m.into_iter().map(render).collect(),
        P::V { prefix: _, infix: _, suffix: _, ps } => ps.into_iter().map(render).collect()
    };

    let vs = k::tk::<k::K>(v);

    let mut vals: Vec<Vec<String>> = Vec::with_capacity(names.len());
    for v in vs {
        match format(unsafe { &**v }) {
            P::A(s) => vals.push(s.chars().map(|x| format!("{}", x)).collect()),
            P::M(m) => vals.push(m.into_iter().map(render).collect()),
            P::V { prefix: _, infix: _, suffix: _, ps } => vals.push(ps.into_iter().map(render).collect())
        }
    }
    assert_eq!(vals.len(), names.len());
    let mut widths = Vec::with_capacity(names.len());
    for i in 0 .. names.len() {
        let mut n = names[i].len();
        n = cmp::max(n, vals[i].iter().map(String::len).max().unwrap_or(0));
        widths.push(n + 1);
    }

    let mut line = String::new();
    let mut s = String::new();

    for (c, w) in names.iter().zip(widths.iter().cloned()) {
        write!(&mut line, "{:width$}", c, width = w).unwrap();
    }

    write_line(cols, &line, &mut s);
    line.clear();

    for _ in 0 .. widths.iter().sum() {
        line.push('-');
    }
    write_line(cols, &line, &mut s);
    line.clear();

    let n = vals.iter().map(Vec::len).max().unwrap_or(0);
    for i in 0 .. if n > lines { lines - 1 } else { n } {
        for j in 0..names.len() {
            write!(&mut line, "{:width$}", vals[j][i], width = widths[j]).unwrap();
        }
        write_line(cols, &line, &mut s);
        line.clear();
    }

    if n > lines {
        writeln!(&mut s, "..").unwrap();
    }

    s
}

fn keyed(lines: usize, cols: usize, k: &k::K0, v: &k::K0) -> String {
    let (lk, lv) = unsafe { let x = k::kK(k); let x = k::tk::<k::K>(x); (&*x[0], &*x[1]) };
    let (rk, rv) = unsafe { let x = k::kK(v); let x = k::tk::<k::K>(x); (&*x[0], &*x[1]) };

    let lhs = table(lines, cols, lk, lv);
    let rhs = table(lines, cols, rk, rv);

    let mut s = String::new();
    let mut line = String::new();
    let len = lhs.lines().count();

    for (l, r) in lhs.lines().zip(rhs.lines()).take(if len > lines { len - 1 } else { len }) {
        write!(&mut line, "{}| {}", l, r).unwrap();
        if line.len() > cols {
            writeln!(&mut s, "{}..", &line[..cols-2]).unwrap();
        } else {
            writeln!(&mut s, "{}", line).unwrap();
        }
        line.clear();
    }

    if len > lines {
        writeln!(&mut s, "..").unwrap();
    }

    s
}

fn format(x: &k::K0) -> P {
    match k::t(x) {
        0 => {
            let elements = k::tk::<k::K>(x);
            let formatted = elements.iter().take(N).map(|x| unsafe { format(&**x)}).collect();
            P::M(formatted)
        },
        -1 => atom_bool(k::gK(x)),
        1 => array("", "", "b", |x| atom_bool(*x), k::tk::<u8>(x)),
        -2 => atom_guid(k::tk::<[u8;16]>(x).first().unwrap()),
        2 => array("", ", ", "", atom_guid, k::tk::<[u8;16]>(x)),
        -4 => atom_byte(k::gK(x)),
        4 => array("0x", "", "", |x| atom_byte(*x), k::tk::<u8>(x)),
        -5 => atom_auto(k::hK(x)),
        5 => array("", " ", "", atom_auto, k::tk::<k::H>(x)),
        -6 => atom_int(k::iK(x)),
        6 => array("", " ", "", |x| atom_int(*x), k::tk::<k::I>(x)),
        -7 => atom_long(k::jK(x)),
        7 => array("", " ", "", |x| atom_long(*x), k::tk::<k::J>(x)),
        -8 => atom_auto(k::eK(x)),
        8 => array("", " ", "", atom_auto, k::tk::<k::E>(x)),
        -9 => atom_float(k::fK(x)),
        9 => array("", " ", "", |x| atom_float(*x), k::tk::<k::F>(x)),
        -10 => atom_auto(k::gK(x) as char),
        10 => array("\"", "", "\"", |x| atom_auto((*x) as char), k::tk::<u8>(x)),
        -11 => atom_sym(k::sK(x)),
        11 => array("`", "`", "", |x| atom_sym(*x), k::tk::<k::S>(x)),
        -12 => atom_timestamp(k::jK(x)),
        12 => array(""," ","",|x| atom_timestamp(*x), k::tk::<k::J>(x)),
        -13 => atom_month(k::iK(x)),
        13 => array("", " ", "", |x| atom_month(*x), k::tk::<k::I>(x)),
        -14 => atom_date(k::iK(x)),
        14 => array("", " ", "", |x| atom_date(*x), k::tk::<k::I>(x)),
        -15 => atom_datetime(k::fK(x)),
        15 => array("", " ", "", |x| atom_datetime(*x), k::tk::<k::F>(x)),
        -16 => atom_timespan(k::jK(x)),
        16 => array("", " ", "", |x| atom_timespan(*x), k::tk::<k::J>(x)),
        -17 => atom_minute(k::iK(x)),
        17 => array("", " ", "", |x| atom_minute(*x), k::tk::<k::I>(x)),
        -18 => atom_second(k::iK(x)),
        18 => array("", " ", "", |x| atom_second(*x), k::tk::<k::I>(x)),
        -19 => atom_time(k::iK(x)),
        19 => array("", " ", "", |x| atom_time(*x), k::tk::<k::I>(x)),
        98 => { unsafe { let k = k::kK(x); flat_tab(&*k::tk::<k::K>(k)[0], &*k::tk::<k::K>(k)[1]) } },
        99 => { let k; let v; let ks = k::tk::<k::K>(x); unsafe { k = &*ks[0]; v = &* ks[1]; } flat_dict(k, v) },
        x if ((x > -80) && (x < -19)) || ((x > 19) && (x < 80)) => P::A(format!("unsupported enum type {}", x)),
        -128 => atom_err(k::sK(x)),
        100 => P::A(str::from_utf8(k::tk::<u8>(x)).unwrap_or("invalid utf8").into()),
        101 => monad(k::gK(x)),
        102 => dyad(k::gK(x)),
        103 => adverb(k::gK(x)),
        t => P::A(format!("unsupported type {}", t))
    }
}

fn array<'a, T: 'a, F: 'a + Fn(&'a T) -> P>(prefix: &'static str, infix: &'static str, suffix: &'static str, f: F, x: &'a [T]) -> P {
    let ps = x.iter()
        .take(N)
        .map(f)
        .collect();
    P::V { prefix, infix, suffix, ps }
}

fn flat_dict(k: &k::K0, v: &k::K0) -> P {

    let keys = match format(k) {
        P::A(a) => a,
        P::M(m) => {
            if m.is_empty() {
                "()".into()
            } else {
                let mut s = String::from("(");
                for x in m {
                    write!(&mut s, "{};", render(x)).unwrap();
                }
                s.pop();
                s.push(')');
                s
            }
        },
        P::V { prefix, infix, suffix, ps } => {
            if ps.is_empty() {
                String::from("()")
            } else {
                let mut s: String = format!("({}", prefix);
                for x in ps {
                    s.push_str(&render(x));
                    s.push_str(infix);
                }
                s.truncate(s.len() - infix.len());
                s.push_str(suffix);
                s.push(')');
                s
            }
        }
    };

    let vals = match format(v) {
        P::A(a) => a,
        P::M(m) => {
            if m.is_empty() {
                "()".into()
            } else {
                let mut s = String::from("(");
                for x in m {
                    write!(&mut s, "{};", render(x)).unwrap();
                }
                s.pop();
                s.push(')');
                s
            }
        },
        P::V { prefix, infix, suffix, ps } => {
            if ps.is_empty() {
                String::from("()")
            } else {
                let mut s: String = format!("({}", prefix);
                for x in ps {
                    s.push_str(&render(x));
                    s.push_str(infix);
                }
                s.truncate(s.len() - infix.len());
                s.push_str(suffix);
                s.push(')');
                s
            }
        }
    };

    P::A(format!("{}!{}", keys, vals))
}

fn flat_tab(n: &k::K0, c: &k::K0) -> P {
    let keys = match format(n) {
        P::A(a) => a,
        P::M(m) => {
            if m.is_empty() {
                "()".into()
            } else {
                let mut s = String::from("(");
                for x in m {
                    write!(&mut s, "{};", render(x)).unwrap();
                }
                s.pop();
                s.push(')');
                s
            }
        },
        P::V { prefix, infix, suffix, ps } => {
            if ps.is_empty() {
                String::from("()")
            } else {
                let mut s: String = format!("({}", prefix);
                for x in ps {
                    s.push_str(&render(x));
                    s.push_str(infix);
                }
                s.truncate(s.len() - infix.len());
                s.push_str(suffix);
                s.push(')');
                s
            }
        }
    };

    let vals = match format(c) {
        P::A(a) => a,
        P::M(m) => {
            if m.is_empty() {
                "()".into()
            } else {
                let mut s = String::from("(");
                for x in m {
                    write!(&mut s, "{};", render(x)).unwrap();
                }
                s.pop();
                s.push(')');
                s
            }
        },
        P::V { prefix, infix, suffix, ps } => {
            if ps.is_empty() {
                String::from("()")
            } else {
                let mut s: String = format!("({}", prefix);
                for x in ps {
                    s.push_str(&render(x));
                    s.push_str(infix);
                }
                s.truncate(s.len() - infix.len());
                s.push_str(suffix);
                s.push(')');
                s
            }
        }
    };

    P::A(format!("+{}!{}", keys, vals))
}

fn atom_bool(x: u8) -> P {
    P::A(format!("{}", if x != 0 { 1 } else { 0 }))
}

fn atom_guid(x: &[u8; 16]) -> P {
    let mut s = String::new();
    for (i, b) in x.iter().enumerate() {
        write!(&mut s, "{:x}", b).unwrap();
        if 3 == (i % 4) {
            s.push('-');
        }
    }

    s.pop();
    P::A(s)
}

fn atom_byte(x: k::G) -> P {
    P::A(format!("{:x}", x))
}

fn atom_auto<T: Display>(x: T) -> P {
    P::A(format!("{}", x))
}

fn atom_int(x: k::I) -> P {
    if x == NULL_INT {
        P::A("0Ni".into())
    } else {
        P::A(format!("{}", x))
    }
}

fn atom_long(x: k::J) -> P {
    if x == NULL_LONG {
        P::A("0N".into())
    } else {
        P::A(format!("{}", x))
    }
}

fn atom_float(x: k::F) -> P {
    if x.is_nan() {
        P::A("0n".into())
    } else {
        P::A(format!("{}", x))
    }
}


fn atom_sym(x: k::S) -> P {
    let s = unsafe {
        ffi::CStr::from_ptr(x).to_str().unwrap_or("invalid utf8").into()
    };

    P::A(s)
}

fn atom_err(x: k::S) -> P {
    let s = unsafe {
        ffi::CStr::from_ptr(x).to_str().unwrap_or("invalid utf8")
    };

    P::A(format!("'{}", s))
}

fn atom_timestamp(x: k::J) -> P {
    if x == NULL_LONG {
        P::A("0Np".into())
    } else {
        let nd = NaiveDate::from_ymd(2000,1,1).and_hms(0,0,0) + Duration::nanoseconds(x);
        P::A(format!("{}", nd))
    }
}

fn atom_month(x: k::I) -> P {
    if x == NULL_INT {
        P::A("0Nm".into())
    } else {
        let y = 2000 + x / 12;
        let m = x % 12;
        P::A(format!("{}.{}", y, m))
    }
}

fn atom_date(x: k::I) -> P {
    if x == NULL_INT {
        P::A("0Nd".into())
    } else {
        let d = NaiveDate::from_ymd(2000,1,1) + Duration::days(x as i64);
        P::A(format!("{}", d))
    }
}

fn atom_datetime(x: k::F) -> P {
    let d = NaiveDate::from_ymd(2000, 1, 1).and_hms(0, 0, 0) + Duration::days(x.trunc() as i64);
    let t = Duration::nanoseconds((x.fract() * (Duration::days(1).num_nanoseconds().unwrap() as f64)) as i64);
    P::A(format!("{}", d + t))
}

fn atom_timespan(x: k::J) -> P {
    if x == NULL_LONG {
        P::A("0Nn".into())
    } else {
        P::A(format!("{}", Duration::nanoseconds(x)))
    }
}

fn atom_minute(x: k::I) -> P {
    if x == NULL_INT {
        P::A("0Nu".into())
    } else {
        P::A(format!("{}", NaiveTime::from_hms(0, 0, 0) + Duration::minutes(x as i64)))
    }
}

fn atom_second(x: k::I) -> P {
    if x == NULL_INT {
        P::A("0Nv".into())
    } else {
        P::A(format!("{}", NaiveTime::from_hms(0, 0, 0) + Duration::seconds(x as i64)))
    }
}

fn atom_time(x: k::I) -> P {
    if x == NULL_INT {
        P::A("0Nt".into())
    } else {
        P::A(format!("{}", NaiveTime::from_hms(0, 0, 0) + Duration::milliseconds(x as i64)))
    }
}

fn monad(x:u8)->P{
    let x: &str = ["::","+:","-:","*:","%:","&:","|:","^:","=:","<:",">:","$:", ",:",
                   "#:","_:","~:","!:","?:","@:",".:","0::","1::","avg", "last","sum",
                   "prd","min","max","exit","getenv","abs", "sqrt","log","exp","sin",
                   "asin","cos","acos","tan","atan","enlist","var","dev"]
        .get(x as usize)
        .unwrap_or(&"unrecognised monad");
    P::A(x.into())
}

fn dyad(x:u8)->P{
    let x:&str=[":","+","-","*","%","&","|","^","=","<",">","$",",","#","_","~","!",
                "?","@",".","0:","1:","2:","in","within","like","bin","ss","insert",
                "wsum","wavg","div","xexp","setenv","binr","cov","cor"]
        .get(x as usize)
        .unwrap_or(&"unrecognised dyad");
    P::A(x.into())
}

fn adverb(x:u8)->P{
    let x:&str=["'","/","\\","':","/:","\\:"].get(x as usize).unwrap_or(&"unrecognised adverb");
    P::A(x.into())
}
