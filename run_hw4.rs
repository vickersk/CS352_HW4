mod hw4;

use std::env;
use std::fs::File;
use std::io;
use std::io::Write;

/// Runs read_item on each lne of the file, printing the results
fn read_all_items(
    inp: &mut dyn io::BufRead, out: &mut dyn io::Write)
-> io::Result<()> {
    loop { 
        let mut line = String::new();
        let n_read = inp.read_line(&mut line)?;
        if n_read == 0 { return Ok(()); } // end-of-file

        match hw4::read_item(&line) {
            Ok(item) => 
                writeln!(out, "{}", item)?,
            Err(hw4::ParseError::InvalidItemCode(s)) =>
                writeln!(out, "Invalid item code: {}", s)?,
            Err(hw4::ParseError::InvalidPrice(s)) =>
                writeln!(out, "Invalid price: {}", s)?,
            Err(hw4::ParseError::WrongFieldCount(n)) =>
                writeln!(out, "Wrong number of fields: {}", n)?,
        };
    }
}

// return type for main lets us use the ? operator for file errors
fn main() -> io::Result<()> {
    // collect arguments into iterator
    let args : Vec<_> = env::args().collect();
    // set up input according to arguments
    match args.len() {
        1 => read_all_items(&mut io::stdin().lock(), &mut io::stdout()),
        2 => read_all_items(
            &mut io::BufReader::new(File::open(&args[1])?), 
            &mut io::stdout()),
        3 => read_all_items(
            &mut io::BufReader::new(File::open(&args[1])?), 
            &mut File::create(&args[2])?),
        _ => writeln!(io::stdout(), "usage: ./hw4 [infile [outfile]]")
    }
}