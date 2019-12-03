mod element;
mod emitter;
mod read_spec;

use crate::element::{Element, Type};
use crate::emitter::Emitter;
use crate::read_spec::ReadSpec;

use rustler::Atom;
use rustler::MapIterator;

use std::collections::HashMap;
use std::io::Cursor;
use std::thread;

use rustler;
use rustler::types::{Binary, Pid};
use rustler::{Encoder, Env, Error, NifResult, OwnedEnv, Term};

use quick_xml::events::Event;
use quick_xml::Reader;

mod atoms {
    rustler::rustler_atoms! {
        atom ok;
        atom start;
        atom end;
        atom empty;

        atom timestamp;
        atom int;
        atom float;
        atom string;
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
    let copied_data = thread_env.save(args[1]);

    let copied_pid = thread_env.save(args[0]);
    let spec = spec_from_map(env, args[2].decode()?)?;

    thread::spawn(move || {
        thread_env.run(|env| {
            let pid: Pid = match copied_pid.load(env).decode() {
                Ok(pid) => pid,
                Err(err) => panic!(format!("Failed to get PID: {}", format_error(err))),
            };

            let data: Binary = match copied_data.load(env).decode() {
                Ok(data) => data,
                Err(_) => panic!("Failed to get Binary"),
            };

            let reader = Cursor::new(data.as_slice());
            let mut reader = Reader::from_reader(reader);

            let mut buf = Vec::new();

            let mut pending_emitters: Vec<Emitter> = vec![];

            loop {
                match reader.read_event(&mut buf) {
                    Ok(Event::Start(ref t)) => {
                        for emitter in pending_emitters.iter_mut() {
                            emitter.start_child(t);
                        }

                        if let Some(el) = spec.patterns.get(std::str::from_utf8(t.name()).unwrap()) {
                            pending_emitters.push(Emitter::start(env, el, t));
                        }
                    }

                    Ok(Event::Text(ref t)) => {
                        for emitter in pending_emitters.iter_mut() {
                            emitter.text(t);
                        }
                    }

                    Ok(Event::End(ref t)) => {
                        let mut to_delete = vec![];
                        for (n, emitter) in pending_emitters.iter_mut().enumerate() {
                            if emitter.end(t) {
                                env.send(&pid, emitter.output());
                                to_delete.push(n);
                            }
                        }

                        for n in to_delete.iter().rev() {
                            pending_emitters.swap_remove(*n);
                        }
                    }
                    Ok(Event::Empty(ref t)) => {
                        if let Some(el) = spec.patterns.get(std::str::from_utf8(t.name()).unwrap()) {
                            env.send(&pid, Emitter::start(env, el, t).output());
                        }
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
        Error::RaiseTerm(_enc) => format!("raising term"),
    }
}

fn spec_from_map<'a>(env: Env<'a>, map: MapIterator<'a>) -> NifResult<ReadSpec> {
    let mut spec = ReadSpec::new();

    println!("Trying to read map");

    for (key, value) in map {
        let name: &str = key.decode()?;

        dbg!(name);

        spec.add(name, element_from_map(env, name, value)?);
    }

    Ok(spec)
}

fn element_from_map<'a>(env: Env<'a>, name: &str, term: Term<'a>) -> NifResult<Element> {
    if term.is_map() {
        if name.starts_with('@') {
            dbg!(name);
            return Err(Error::BadArg);
        }

        let mut attributes = HashMap::new();
        let mut content = HashMap::new();
        let map: MapIterator = term.decode()?;

        for (key, val) in map {
            let inner_name: &str = key.decode()?;
            dbg!(inner_name);

            if inner_name.starts_with('@') {
                let value = as_type(val)?;
                attributes.insert(inner_name.chars().skip(1).collect(), value);
            } else {
                let value = element_from_map(env, inner_name, val)?;
                content.insert(inner_name.to_owned(), value);
            }
        }

        Ok(Element::object(name, attributes, content))
    } else if term.is_atom() {
        Ok(Element::element(name, HashMap::new(), as_type(term)?))
    } else {
        dbg!(term);
        Err(Error::BadArg)
    }
}

fn as_type<'a>(term: Term<'a>) -> NifResult<Type> {
    let type_: Atom = term.decode()?;

    if type_ == atoms::int() {
        Ok(Type::Int)
    } else if type_ == atoms::float() {
        Ok(Type::Float)
    } else if type_ == atoms::timestamp() {
        Ok(Type::Timestamp)
    } else if type_ == atoms::string() {
        Ok(Type::String)
    } else {
        Err(Error::BadArg)
    }
}
