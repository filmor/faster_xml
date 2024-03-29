use crate::element::{Content, Element, Type};
use chrono::{Datelike, Timelike};
use quick_xml::events::{BytesStart, BytesText};
use rustler::{Term, Encoder, Env};

mod atoms {
    rustler::atoms! { undefined }
}

pub(crate) struct Emitter<'a, 'b> {
    env: Env<'a>,
    element: &'b Element,
    child: Option<Box<Emitter<'a, 'b>>>,
    output: Term<'a>,
    depth: i32,
    is_object: bool,
}

impl<'a, 'b> Emitter<'a, 'b> {
    pub fn new<'c>(env: Env<'a>, element: &'b Element, start_tag: &BytesStart<'c>) -> Self {
        let mut res = Emitter {
            env,
            element,
            child: None,
            output: atoms::undefined().encode(env),
            depth: 1,
            is_object: match element.content {
                Content::Element(_) => false,
                Content::Object(_) => true,
            },
        };

        assert!(start_tag.name().into_inner() == element.name.as_bytes());
        // dbg!(&element.name);

        if res.is_object {
            let mut output = Term::map_new(env);

            for attr in start_tag.attributes().filter_map(|x| x.ok()) {
                let key = String::from_utf8(attr.key.into_inner().to_vec()).unwrap();
                if let Some(typ) = element.attributes.get(&key) {
                    let attr_value = std::str::from_utf8(&attr.value).unwrap();
                    if let Some(result) = to_erlang(env, *typ, attr_value) {
                        let attr_key = format!("@{}", key).encode(env);
                        output = output.map_put(attr_key, result).ok().unwrap();
                    }
                }
            }

            res.output = output;
        }

        res
    }

    pub fn handle_start_child(&mut self, start_tag: &BytesStart<'_>) {
        self.depth += 1;

        if let Some(ref mut child) = self.child {
            child.handle_start_child(start_tag);
        } else if let Content::Object(children) = &self.element.content {
            let key = String::from_utf8(start_tag.name().into_inner().to_vec()).unwrap();
            if let Some(child_element) = children.get(&key) {
                self.child = Some(Box::new(Self::new(self.env, child_element, start_tag)))
            }
        }
    }

    pub fn handle_text(&mut self, text: &BytesText<'_>) {
        if let Some(ref mut child) = self.child {
            child.handle_text(text);
        }

        if self.depth == 1 && !self.is_object {
            if let Content::Element(typ) = self.element.content {
                if let Some(value) = to_erlang(self.env, typ, std::str::from_utf8(text).unwrap()) {
                    self.output = value;
                }
            }
        }
    }

    pub fn handle_end(&mut self) -> bool {
        self.depth -= 1;

        if let Some(ref mut child) = self.child {
            if child.handle_end() {
                self.output = self
                    .output
                    .map_put(child.element.name.encode(self.env), child.output())
                    .ok()
                    .unwrap();

                self.child = None;
            }
        }

        self.depth <= 0
    }

    pub fn output(&self) -> Term<'a> {
        self.output
    }

    pub fn name(&self) -> &str {
        &self.element.name
    }
}

fn to_erlang<'a>(env: Env<'a>, typ: Type, s: &str) -> Option<Term<'a>> {
    use Type::*;

    match typ {
        Int => Some(s.parse::<i64>().ok()?.encode(env)),
        Float => Some(s.parse::<f64>().ok()?.encode(env)),
        Timestamp => {
            let dt = chrono::DateTime::parse_from_rfc3339(s).ok()?;
            Some(
                (
                    (
                        (dt.year(), dt.month(), dt.day()),
                        (dt.hour(), dt.minute(), dt.second()),
                    ),
                    dt.timestamp_subsec_millis(),
                )
                    .encode(env),
            )
        }
        String => Some(s.encode(env)),
    }
}
