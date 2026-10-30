use std::ffi::OsString;
use std::process::Command;

/// A subprocess to spawn: program, argv, extra environment variables.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cmd {
    pub program: String,
    pub args: Vec<OsString>,
    pub envs: Vec<(OsString, OsString)>,
}

impl Cmd {
    pub(super) fn new(program: &str) -> Self {
        Cmd { program: program.to_string(), args: Vec::new(), envs: Vec::new() }
    }

    pub(super) fn arg(mut self, arg: impl Into<OsString>) -> Self {
        self.args.push(arg.into());
        self
    }

    pub fn command(&self) -> Command {
        let mut cmd = Command::new(&self.program);
        cmd.args(&self.args);
        cmd.envs(self.envs.iter().map(|(k, v)| (k, v)));
        cmd
    }
}
