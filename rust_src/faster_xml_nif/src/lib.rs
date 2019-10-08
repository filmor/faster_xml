use std::io::Cursor;

use rustler;
use rustler::{Encoder, Env, NifResult, Term, Error};
use rustler::types::{Binary, Pid};

use quick_xml::Reader;
use quick_xml::events::Event;

mod atoms {
    rustler::rustler_atoms! {
        atom ok;
        atom start;
        atom end;
        atom empty;
    }
}

rustler::rustler_export_nifs! {
    "faster_xml",
    [
        ("parse", 2, parse),
    ],
    Some(on_load)
}

pub fn on_load<'a>(_env: Env<'a>, _load_info: Term<'a>) -> bool {
    true
}

fn parse<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let pid: Pid = args[0].decode()?;
    let data: Binary = args[1].decode()?;

    let reader = Cursor::new(data.as_slice());
    let mut reader = Reader::from_reader(reader);

    let mut buf = Vec::new();

    loop {
        match reader.read_event(&mut buf) {
            Ok(Event::Start(ref t)) => {
                env.send(&pid, (atoms::start(), t.name()).encode(env))
            },
            Ok(Event::End(ref t)) => {
                env.send(&pid, (atoms::end(), t.name()).encode(env))
            },
            Ok(Event::Empty(ref t)) => {
                env.send(&pid, (atoms::empty(), t.name()).encode(env))
            }
            Ok(Event::Eof) => break,
            Ok(_) => (),
            Err(_) => return Err(Error::Atom("broken")),
        }
    }

    Ok((atoms::ok(),).encode(env))
}
