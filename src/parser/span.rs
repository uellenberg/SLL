use std::fmt::Display;
use std::ops::Range;
use std::path::Path;

#[derive(Clone, Debug)]
pub struct Span<'a>(&'a Path, Range<usize>, &'a str);

/// Converts a pest Span to an ariadne Span.
pub fn to_span<'a>(file: &'a Path, span: pest::Span<'a>) -> Span<'a> {
    Span(file, span.start()..span.end(), span.as_str())
}

impl<'a> ariadne::Span for Span<'a> {
    type SourceId = Path;

    fn source(&self) -> &Self::SourceId {
        self.0
    }

    fn start(&self) -> usize {
        self.1.start
    }

    fn end(&self) -> usize {
        self.1.end
    }
}

impl<'a> Display for Span<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.2)
    }
}
