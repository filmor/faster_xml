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
use std::str::from_utf8;
use std::thread;

use rustler;
use rustler::types::{Binary, LocalPid};
use rustler::{Encoder, Env, Error, NifResult, OwnedEnv, Term};

use quick_xml::events::Event;
use quick_xml::Reader;

mod atoms {
    rustler::atoms! {
        ok,
        open,
        close,
        element,
        empty,

        timestamp,
        int,
        float,
        string,
        list,

        done,
    }
}

rustler::init! { "faster_xml", [parse] }

#[rustler::nif]
fn parse<'a>(
    env: Env<'a>,
    pid: LocalPid,
    reference: Term<'a>,
    data: Term<'a>,
    spec: Term<'a>,
) -> NifResult<Term<'a>> {
    let thread_env = OwnedEnv::new();
    let copied_data = thread_env.save(data);

    let copied_pid = thread_env.save(pid.encode(env));
    let copied_ref = thread_env.save(reference);
    let spec = spec_from_map(env, spec.decode()?)?;

    thread::spawn(move || {
        thread_env.run(|env| {
            let pid: LocalPid = copied_pid.load(env).decode().unwrap();
            let data: Binary = copied_data.load(env).decode().unwrap();
            let reference: Term<'_> = copied_ref.load(env);
            let reader = Cursor::new(data.as_slice());
            let mut reader = Reader::from_reader(reader);

            let mut buf = Vec::new();

            let mut pending_emitters: Vec<Emitter> = vec![];

            loop {
                match reader.read_event_into(&mut buf) {
                    Ok(Event::Start(ref t)) => {
                        for emitter in pending_emitters.iter_mut() {
                            emitter.handle_start_child(t);
                        }

                        if let Some(el) = spec.patterns.get(from_utf8(t.name().into_inner()).unwrap()) {
                            let emitter = Emitter::new(env, el, t);

                            env.send(
                                &pid,
                                (reference, emitter.name(), atoms::open(), emitter.output())
                                    .encode(env),
                            );

                            pending_emitters.push(emitter);
                        }
                    }

                    Ok(Event::Text(ref t)) => {
                        for emitter in pending_emitters.iter_mut() {
                            emitter.handle_text(t);
                        }
                    }

                    Ok(Event::End(ref t)) => {
                        let mut to_delete = vec![];
                        for (n, emitter) in pending_emitters.iter_mut().enumerate() {
                            if emitter.handle_end(t) {
                                env.send(
                                    &pid,
                                    (reference, emitter.name(), atoms::close(), emitter.output())
                                        .encode(env),
                                );
                                to_delete.push(n);
                            }
                        }

                        for n in to_delete.iter().rev() {
                            pending_emitters.swap_remove(*n);
                        }
                    }
                    Ok(Event::Empty(ref t)) => {
                        if let Some(el) = spec.patterns.get(from_utf8(t.name().as_ref()).unwrap()) {
                            let emitter = Emitter::new(env, el, t);
                            env.send(
                                &pid,
                                (
                                    reference,
                                    emitter.name(),
                                    atoms::element(),
                                    emitter.output(),
                                )
                                    .encode(env),
                            );
                        }
                    }
                    Ok(Event::Eof) => break (), // TODO
                    Ok(_) => (),
                    Err(_) => (),
                }
            }

            env.send(&pid, (reference, atoms::done()).encode(env));
        })
    });

    Ok((atoms::ok()).encode(env))
}

fn spec_from_map<'a>(env: Env<'a>, map: MapIterator<'a>) -> NifResult<ReadSpec> {
    let mut spec = ReadSpec::new();

    for (key, value) in map {
        let name: &str = key.decode()?;

        // dbg!(name);

        spec.add(name, element_from_map(env, name, value)?);
    }

    Ok(spec)
}

fn element_from_map<'a>(env: Env<'a>, name: &str, term: Term<'a>) -> NifResult<Element> {
    if term.is_map() {
        if name.starts_with('@') {
            // dbg!(name);
            return Err(Error::BadArg);
        }

        let mut attributes = HashMap::new();
        let mut content = HashMap::new();
        let map: MapIterator = term.decode()?;

        for (key, val) in map {
            let inner_name: &str = key.decode()?;
            // dbg!(inner_name);

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
        // dbg!(term);
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
