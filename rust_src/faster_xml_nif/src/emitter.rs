use crate::element::{Content, Element, Type};
use quick_xml::events::BytesEnd;
use quick_xml::events::BytesStart;
use quick_xml::events::BytesText;
use rustler::Term;
use rustler::{Encoder, Env};

mod atoms {
    rustler::rustler_atoms! {
        atom undefined;
    }
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
    pub fn start<'c>(env: Env<'a>, element: &'b Element, start_tag: &BytesStart<'c>) -> Self {
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

        assert!(start_tag.name() == element.name.as_bytes());
        // dbg!(&element.name);

        if res.is_object {
            let mut output = Term::map_new(env);

            for attr in start_tag.attributes().filter_map(|x| x.ok()) {
                let key = String::from_utf8(attr.key.to_vec()).unwrap();
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

    pub fn start_child<'c>(&mut self, start_tag: &BytesStart<'c>) {
        self.depth += 1;

        if let Some(ref mut child) = self.child {
            child.start_child(start_tag);
        } else {
            if let Content::Object(children) = &self.element.content {
                let key = String::from_utf8(start_tag.name().to_vec()).unwrap();
                if let Some(child_element) = children.get(&key) {
                    self.child = Some(Box::new(Self::start(self.env, &child_element, start_tag)))
                }
            }
        }
    }

    pub fn text<'c>(&mut self, text: &BytesText<'c>) {
        if let Some(ref mut child) = self.child {
            child.text(text);
        }

        if self.depth == 1 && !self.is_object {
            if let Content::Element(typ) = self.element.content {
                if let Some(value) = to_erlang(self.env, typ, std::str::from_utf8(text).unwrap()) {
                    self.output = value;
                }
            }
        }
    }

    pub fn end<'c>(&mut self, end: &BytesEnd<'c>) -> bool {
        self.depth -= 1;

        if let Some(ref mut child) = self.child {
            if child.end(end) {
                self.output = self.output
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
        Timestamp => Some(atoms::undefined().encode(env)),
        String => Some(s.encode(env)),
    }
}
