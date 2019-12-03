use crate::element::{Content, Element, Type};
use quick_xml::events::BytesEnd;
use quick_xml::events::BytesStart;
use quick_xml::events::BytesText;
use rustler::NifResult;
use rustler::Term;
use rustler::{Encoder, Env};
use std::collections::HashMap;

type Output<'a> = HashMap<&'a str, Term<'a>>;

pub(crate) struct Emitter<'a> {
    env: Env<'a>,
    element: &'a Element,
    child: Option<Box<Emitter<'a>>>,
    output: Output<'a>,
    depth: i32,
}

impl<'a> Emitter<'a> {
    pub fn start<'b>(env: Env<'a>, element: &'a Element, start_tag: &BytesStart<'b>) -> Self {
        let mut res = Emitter {
            env,
            element,
            child: None,
            output: HashMap::new(),
            depth: 0,
        };

        assert!(start_tag.name() == element.name.as_bytes());

        for attr in start_tag.attributes().filter_map(|x| x.ok()) {
            let key = String::from_utf8(attr.key.to_vec()).unwrap();
            if let Some(typ) = element.attributes.get(&key) {
                let attr_value = std::str::from_utf8(&attr.value).unwrap();
                if let Some(result) = to_erlang(env, *typ, attr_value) {
                    let attr_key = format!("@{}", key);
                    res.output[attr_key.as_str()] = result;
                }
            }
        }

        res
    }

    pub fn start_child<'b>(&mut self, start_tag: &BytesStart) {
        self.depth += 1;

        if let Some(child) = self.child {
            child.start_child(start_tag);
        } else {
            if let Content::Object(children) = self.element.content {
                let key = String::from_utf8(start_tag.name().to_vec()).unwrap();
                if let Some(child_element) = children.get(&key) {
                    self.child = Some(Box::new(Self::start(self.env, &child_element, start_tag)))
                }
            }
        }
    }

    pub fn text<'b>(&mut self, text: &BytesText<'b>) {
        if let Some(child) = self.child {
            child.text(text);
        }

        if self.depth == 0 {
            if let Content::Element(typ) = self.element.content {
                
            }
        }
    }

    pub fn end<'b>(&mut self, end: &BytesEnd<'b>) -> bool {
        self.depth -= 1;

        if let Some(child) = self.child {
            if child.end(end) {
                if let Some(output) = child.output().ok() {
                    self.output[child.element.name.as_str()] = output;
                }

                self.child = None;
            }
        }

        self.depth <= 0
    }

    pub fn output(self) -> NifResult<Term<'a>> {
        let map = Term::map_new(self.env);
        for (k, v) in self.output {
            map.map_put(k.encode(self.env), v)?;
        }

        Ok(map)
    }
}

fn to_erlang<'a>(env: Env<'a>, typ: Type, s: &str) -> Option<Term<'a>> {
    use Type::*;

    match typ {
        Int => Some(s.parse::<i64>().ok()?.encode(env)),
        Float => Some(s.parse::<f64>().ok()?.encode(env)),
        Timestamp => None,
        String => Some(s.encode(env)),
    }
}
