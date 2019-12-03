use std::collections::HashMap;

#[derive(Debug)]
pub(crate) struct Element {
    pub name: String,
    pub attributes: HashMap<String, Type>,
    pub content: Content,
}

impl Element {
    fn new(name: &str, attributes: HashMap<String, Type>, content: Content) -> Self {
        Element {
            name: name.to_owned(),
            attributes,
            content,
        }
    }

    pub fn element(name: &str, attributes: HashMap<String, Type>, content: Type) -> Self {
        Self::new(name, attributes, Content::Element(content))
    }

    pub fn object(name: &str, attributes: HashMap<String, Type>, content: HashMap<String, Element>) -> Self {
        Self::new(name, attributes, Content::Object(content))
    }
}

#[derive(Debug)]
pub(crate) enum Content {
    Element(Type),
    Object(HashMap<String, Element>),
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Type {
    Int,
    Float,
    Timestamp,
}
