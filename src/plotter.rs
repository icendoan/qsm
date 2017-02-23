use common::*;
use krust::kbindings::{KOwned, KVal, KData};
use std::process;
use std::ops::Drop;

trait GnuplotHandle
{
    fn clear(&mut self);
    fn write(&mut self, command: &str);
}

impl GnuplotHandle for Figure
{
    fn clear(&mut self) { self.commands.clear(); }
    fn write(&mut self, comm: &str) { self.commands.push_str(comm); self.commands.push('\n'); }
}

pub struct Figure
{
    commands: String,
    gnuplot: process::Child,
}

impl Figure
{
    pub fn new() -> R<Self>
    {
        let mut comm = process::Command::new("gnuplot");
        let child = comm.spawn().map_err(|_| Error::WithMsg("Cannot find GNUPlot - is it installed?"))?;
        Ok(Figure
        {
            commands: String::new(),
            gnuplot: child
        })
    }

    pub fn figure2d(&mut self) -> Figure2D
    {
        Figure2D(self)
    }

    pub fn show(&mut self)
    {
        
    }

    pub fn clear(&mut self) { self.commands.clear() }

    fn write_command(&mut self, command: &str)
    {
        self.commands.push_str(command);
        self.commands.push('\n');
    }
}

pub struct Figure2D<'a>(&'a mut Figure);

impl <'a> Drop for Figure2D<'a>
{
    fn drop(&mut self)
    {
    }
}

impl <'a> Figure2D<'a>
{
    pub fn lines(mut self) -> LineGraph<'a>
    {
        self.0.write_command("");
        LineGraph(self)
    }

    pub fn bars(mut self) -> BarGraph<'a>
    {
        self.0.write_command("");
        BarGraph(self)
    }

    pub fn candlesticks(mut self) -> Candlesticks<'a>
    {
        self.0.write_command("");
        Candlesticks(self)
    }
}

pub struct LineGraph<'a>(Figure2D<'a>);
impl<'a> Drop for LineGraph<'a> {
    fn drop(&mut self)
    {
        
    }
}
pub struct BarGraph<'a>(Figure2D<'a>);
impl<'a> Drop for BarGraph<'a>
{
    fn drop(&mut self)
    {
        
    }
}

pub struct Candlesticks<'a>(Figure2D<'a>);
impl <'a> Drop for Candlesticks<'a>
{
    fn drop(&mut self)
    {
        
    }
}

type KTable<'a> = Vec<(&'a str, KVal<'a>)>;
fn keyed(t: KVal) -> R<(KTable, KTable)>
{
    match t
    {
        KVal::Dict(box k, box v) => Ok((unkeyed(k)?, unkeyed(v)?)),
        KVal::Table(box k) => Ok((Vec::new(), unkeyed(k)?)),
        _ => Err(Error::WithMsg("Plot output is not a table.")),
    }

}

fn unkeyed(t: KVal) -> R<KTable>
{
    fn extract_dict(d: KVal) -> R<(Box<KVal>, Box<KVal>)>
    {
        match d
        {
            KVal::Dict(k, v) => Ok((k, v)),
            _ => Err(Error::Warning("")),
        }
    }

    let bad_type = Error::WithMsg("Plot expression yields an incorrect type.");

    if let KVal::Table(boxed_d) = t
    {
        let d = *boxed_d;
        let (boxed_keys, boxed_vals) = extract_dict(d)?;
        let names: &[*const i8] = match *boxed_keys
        {
            KVal::Symbol(KData::List(slice)) => slice,
            _ => return Err(bad_type),
        };

        let cols: Vec<KVal> = match *boxed_vals
        {
            KVal::Mixed(v) => v,
            _ => return Err(bad_type),
        };

        let mut out = Vec::with_capacity(names.len());

        for (nameptr, col) in names.iter().zip(cols.into_iter())
        {
            out.push((sym_str(nameptr), col));
        }

        Ok(out)
    }
    else
    {
        Err(bad_type)
    }
}

pub fn plot(fig: &mut Figure, k: KOwned) -> R<()>
{
    fn lines(fig: &mut Figure2D, label: &str, axis: KVal, data: &[(&str, KVal)]) -> R<()>
    {
        Ok(())
    }

    fn bars(fig: &mut Figure2D, label: &str, data: &[(&str, KVal)]) -> R<()>
    {
        Ok(())
    }

    fn candlesticks(fig: &mut Figure2D, label: &str, axis: KVal, data: &[(&str, KVal)]) -> R<()>
    {Ok(())}

    Ok(())
}
