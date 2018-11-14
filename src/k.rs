#![allow(non_snake_case,unused)]
use libc;use std::slice;pub type C=libc::c_char;pub type J=libc::c_long;
pub type G=libc::c_uchar;pub type S=*const C;pub type SC=libc::c_schar;
pub type H=libc::c_short;pub type I=libc::c_int;pub type E=libc::c_float;
pub type F=libc::c_double;pub type K=*const K0;
pub const KCAP:u8=3;
#[repr(C)]#[derive(Debug,Copy,Clone)]pub struct KA{n:J,g0:[G;1]}
#[repr(C)]pub union KU{g:G,h:H,i:I,j:J,e:E,f:F,s:S,k:K,v:KA}
#[repr(C)]pub struct K0{m:SC,a:SC,t:SC,u:C,r:I,k:KU}
#[link(name="kdb")]
extern"C"{pub fn okx(x:K)->I;pub fn b9(x:I,y:K)->K;pub fn d9(x:K)->K;pub fn ktn(t:I,n:J)->K;pub fn r0(x:K);pub fn khp(x:S,y:I)->I;pub fn ee(x:K)->K;}
pub fn tk<'a,T>(k:K)->&'a [T]
{unsafe{slice::from_raw_parts((&(*k).k.v.g0)as *const G as *const T,((*k).k.v.n) as usize)} }
pub fn mtk<'a,T>(k:K)->&'a mut [T]
{unsafe{slice::from_raw_parts_mut((&(*k).k.v.g0)as *const G as *const T as *mut T, ((*k).k.v.n) as usize)}}

pub fn gK(k:&K0)->G{unsafe{k.k.g}}
pub fn hK(k:&K0)->H{unsafe{k.k.h}}
pub fn iK(k:&K0)->I{unsafe{k.k.i}}
pub fn jK(k:&K0)->J{unsafe{k.k.j}}
pub fn eK(k:&K0)->E{unsafe{k.k.e}}
pub fn fK(k:&K0)->F{unsafe{k.k.f}}
pub fn sK(k:&K0)->S{unsafe{k.k.s}}
pub fn kK(k:&K0)->K{unsafe{k.k.k}}
pub fn vK(k:&K0)->KA{unsafe{k.k.v}}
pub fn t(k:&K0)->SC{k.t}
