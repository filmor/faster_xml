mod read_spec;
mod element;
use crate::read_spec::ReadSpec;

use std::io::Cursor;
use std::thread;

use rustler;
use rustler::types::{Binary, Pid};
use rustler::{Encoder, Env, NifResult, OwnedEnv, Term, Error};

use quick_xml::events::Event;
use quick_xml::Reader;

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
        ("parse", 3, parse),
    ],
    Some(on_load)
}

pub fn on_load<'a>(_env: Env<'a>, _load_info: Term<'a>) -> bool {
    true
}

fn parse<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let thread_env = OwnedEnv::new();
    let copied_pid = thread_env.save(args[0]);
    let copied_data = thread_env.save(args[1]);

    let spec = spec_from_map(args[1]);

    thread::spawn(move || {
        thread_env.run(|env| {
            let pid: Pid = match copied_pid.load(env).decode() {
                Ok(pid) => pid,
                Err(err) =>
                panic!(format!("Failed to get PID: {}", format_error(err))),
            };

            let data: Binary = match copied_data.load(env).decode() {
                Ok(data) => data,
                Err(_) => panic!("Failed to get Binary"),
            };

            let reader = Cursor::new(data.as_slice());
            let mut reader = Reader::from_reader(reader);

            let mut buf = Vec::new();

            loop {
                match reader.read_event(&mut buf) {
                    Ok(Event::Start(ref t)) => {
                        env.send(&pid, (atoms::start(), t.name()).encode(env))
                    }
                    Ok(Event::Text(ref t)) => {}
                    Ok(Event::End(ref t)) => env.send(&pid, (atoms::end(), t.name()).encode(env)),
                    Ok(Event::Empty(ref t)) => {
                        env.send(&pid, (atoms::empty(), t.name()).encode(env))
                    }
                    Ok(Event::Eof) => break (),
                    Ok(_) => (),
                    Err(_) => (),
                }
            }
        })
    });

    Ok((atoms::ok()).encode(env))
}


fn format_error(err: Error) -> String {
    match err {
        Error::BadArg => format!("badarg"),
        Error::Atom(s) => format!("{}", s),
        Error::RaiseAtom(s) => format!("raise {}", s),
        Error::RaiseTerm(_enc) => format!("raising term")
    }
}


fn spec_from_map<'a>(term: Term<'a>) -> ReadSpec {
    unimplemented!()
}