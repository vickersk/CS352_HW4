mod hw4;

use std::env;
use std::fs::File;
use std::io;
use std::io::Write;

/// Runs parseRecord on each line of the file, printing the results
fn parse_all_records(
        inp: &mut dyn io::BufRead, out: &mut dyn io::Write) -> io::Result<()> {
    loop { 
        let mut line = String::new();
        let n_read = inp.read_line(&mut line)?;
        if n_read == 0 { return Ok(()); } // end-of-file

        match hw4::parse_record(&line) {
            Ok(rec) => 
                writeln!(out, "{}", rec)?,
            Err(hw4::ParseError::WrongFieldCount(n)) =>
                writeln!(out, "Wrong number of fields: {}", n)?,
            Err(hw4::ParseError::InvalidCourseNum(s)) =>
                writeln!(out, "Invalid course number: {}", s)?,
            Err(hw4::ParseError::InvalidNumCredits(s)) =>
                writeln!(out, "Invalid number of credits: {}", s)?,
            Err(hw4::ParseError::InvalidGrade(s)) =>
                writeln!(out, "Invalid grade: {}", s)?
        };
    }
}

// return type for main lets us use the ? operator for file errors
fn main() -> io::Result<()> {
    let args : Vec<_> = env::args().collect(); // collect arguments into iterator
    match args.len() {
        1 => parse_all_records(&mut io::stdin().lock(), &mut io::stdout()),
        2 => parse_all_records(
            &mut io::BufReader::new(File::open(&args[1])?), &mut io::stdout()),
        3 => parse_all_records(
            &mut io::BufReader::new(File::open(&args[1])?), 
            &mut File::create(&args[2])?),
        _ => writeln!(io::stdout(), "usage: ./hw4 [infile [outfile]]")
    }
}