use miette::Diagnostic;
use miette::NamedSource;
use miette::SourceCode;
use miette::SourceSpan;
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
pub enum MsgTemplate<S: SourceCode + 'static + std::fmt::Debug> {
    #[error("Type error: {text}")]
    Single {
        text: String,
        #[label("{pos_text}")]
        pos: SourceSpan,
        pos_text: String,
        #[source_code]
        file: NamedSource<S>,
        #[help]
        help: Option<String>,
    },
    #[error("Type error: {text}")]
    Double {
        text: String,

        #[label("{pos_text1}")]
        pos1: SourceSpan,
        pos_text1: String,
        #[source_code]
        file1: NamedSource<S>,

        #[label("{pos_text2}")]
        pos2: SourceSpan,
        pos_text2: String,
        #[source_code]
        file2: NamedSource<S>,

        #[help]
        help: Option<String>,
    },
}
