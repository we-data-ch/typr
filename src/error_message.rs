use miette::{Diagnostic, NamedSource, SourceSpan, Result};
use thiserror::Error;
use miette::SourceCode;
use std::fs;
use nom_locate::LocatedSpan;

#[derive(Debug, Error, Diagnostic)]
#[error("Syntax error: unexpected character")]
struct SyntaxError<S: SourceCode + 'static + std::fmt::Debug> {
    #[label("here")]
    span: SourceSpan,

    #[source_code]
    src: NamedSource<S>,

    // Message additionnel
    #[help]
    help: Option<String>,
}

pub fn syntax_error(ls: LocatedSpan<&str, String>, help: &str) {
    let offset = ls.location_offset();
    let file_name = ls.clone().extra;
    let text = fs::read_to_string(&file_name)
        .unwrap_or_else(|e| panic!("Error while reading file: {}", e));
    let diagnose: Result<()> = Err(SyntaxError {
        span: (offset, 1).into(),
        src: NamedSource::new(file_name.to_string(), text.to_string()),
        help: Some(help.into()),
    }.into());
    println!("{:?}", diagnose);
}

