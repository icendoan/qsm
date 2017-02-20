use super::*;
use gnuplot;
use krust::kbindings::{self, KOwned, KVal, KData};

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

pub fn plot(fig: &mut gnuplot::Figure, k: KOwned) -> R<()>
{
    fn lines(fig: &mut gnuplot::Figure, label: &str, axis: KVal, mut data: Vec<(&str, KVal)>) -> R<()>
    {
        if data.is_empty() || axis.len() == 0
        {
            return Err(Error::WithMsg("Plot output is empty."));
        }

        let line_options = [gnuplot::TickOption::Mirror(false), gnuplot::TickOption::Inward(false), gnuplot::TickOption::OnAxis(true)];
        let colours = ["green", "red", "blue", "yellow", "orange"]; // ...

        fig.clear_axes();

        let axes: &mut gnuplot::Axes2D = fig.axes2d();
        axes.set_x_label(label, &[])
            .set_x_ticks(Some((gnuplot::AutoOption::Auto, axis.len() as u32)),
                         &line_options[..],
                         &[])
            .set_y_ticks(Some((gnuplot::AutoOption::Auto, data[0].0.len() as u32)),
                         &line_options[..],
                         &[]);

        for (i, (name, col)) in data.drain(..).enumerate()
        {
            let opts = &[gnuplot::PlotOption::Color(colours[i % colours.len()]), gnuplot::PlotOption::Caption(name)];
            let badtype = Error::WithMsg("Plot output has an invalid type.");

            //        axes.lines(as_datatype_slice(axis),
            //                   as_datatype_slice(col),
            //                   opts);


            axes.lines()

        }

        Ok(())
    }

    fn bars(fig: &mut gnuplot::Figure, label: &str, data: Vec<(&str, KVal)>) -> R<()>
    {
        Ok(())
    }

    Ok(())
}
