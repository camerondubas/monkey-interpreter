const TRACE_INDENT_PLACEHOLDER: &str = "\t";

pub struct Tracer {
    indent_level: i64,
    enabled: bool,
}

impl Tracer {
    pub fn new() -> Self {
        Tracer {
            indent_level: 1,
            enabled: false,
        }
    }

    fn indent_str(&self) -> String {
        let mut indent_str = String::new();

        for _ in 0..self.indent_level {
            indent_str.push_str(TRACE_INDENT_PLACEHOLDER);
        }

        indent_str
    }
    pub fn trace(&mut self, message: &str) {
        if !self.enabled {
            return;
        }
        self.indent_level += 1;
        println!("{}START: {}", &self.indent_str(), message);
    }

    pub fn untrace(&mut self, message: &str) {
        if !self.enabled {
            return;
        }
        println!("{}END: {}", &self.indent_str(), message);
        self.indent_level -= 1;
    }

    pub fn enable(&mut self) {
        self.enabled = true;
    }

    pub fn disable(&mut self) {
        self.enabled = true;
    }
}
