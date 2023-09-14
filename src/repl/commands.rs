use monkey_interpreter::object::Environment;
use std::{cell::RefCell, rc::Rc, str::FromStr};

use super::{
    error::{ReplError, Result},
    Repl,
};

pub enum ReplCommand {
    Help,
    Quit,
    Unknown,
    DisplayEnv,
    ClearEnv,
    DisplayMode,
    SetMode(String),
    Execute(String),
}

impl FromStr for ReplCommand {
    type Err = ReplError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let command = match s {
            ":help" => ReplCommand::Help,
            ":exit" => ReplCommand::Quit,
            ":env" => ReplCommand::DisplayEnv,
            ":clear" => ReplCommand::ClearEnv,
            ":mode" => ReplCommand::DisplayMode,
            ":quit" => ReplCommand::Quit,
            line if line.starts_with(":mode") => ReplCommand::SetMode(line[6..].to_string()),
            line if line.starts_with(':') => ReplCommand::Unknown,
            cmd => ReplCommand::Execute(cmd.to_string()),
        };

        Ok(command)
    }
}

impl ReplCommand {
    pub fn run(&self, repl: &mut Repl) -> Result<()> {
        match self {
            ReplCommand::Help => repl.print_help(),
            ReplCommand::DisplayEnv => println!("{}", repl.environment.borrow()),
            ReplCommand::ClearEnv => {
                repl.environment = Rc::new(RefCell::new(Environment::new()));
                println!("Environment cleared");
            }
            ReplCommand::DisplayMode => repl.print_mode(),
            ReplCommand::SetMode(mode) => {
                repl.set_mode(mode.as_str())?;
                repl.print_mode();
            }
            ReplCommand::Unknown => {
                return Err(ReplError::UnknownCommand);
            }
            ReplCommand::Quit => {
                return Err(ReplError::Quit);
            }
            ReplCommand::Execute(line) => {
                return repl.execute(line.clone());
            }
        };

        Ok(())
    }
}
