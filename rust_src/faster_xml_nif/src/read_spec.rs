use std::collections::HashMap;
use crate::element::Element;

#[derive(Debug, Clone)]
pub(crate) struct ReadSpec {
    pub patterns: HashMap<String, Element>
}


impl ReadSpec {
    pub fn new() -> Self {
        ReadSpec{
            patterns: HashMap::new()
        }
    }

    pub fn add(&mut self, name: &str, element: Element) {
        self.patterns.insert(name.to_string(), element);
    }
}