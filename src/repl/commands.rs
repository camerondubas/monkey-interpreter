use std::{cell::RefCell, rc::Rc};

use crate::object::Environment;

use super::Repl;

pub enum ReplCommand {
    Help,
    Quit,
    Unknown,
    DisplayEnv,
    ClearEnv,
    DisplayMode,
    SetMode(String),
}

impl ReplCommand {
    pub fn from_str(command: &str) -> ReplCommand {
        match command {
            "help" => ReplCommand::Help,
            "exit" => ReplCommand::Quit,
            "env" => ReplCommand::DisplayEnv,
            "clear" => ReplCommand::ClearEnv,
            "mode" => ReplCommand::DisplayMode,
            line if line.starts_with("mode") => ReplCommand::SetMode(line[5..].to_string()),

            _ => ReplCommand::Unknown,
        }
    }

    pub fn run(&self, repl: &mut Repl) -> Result<(), ()> {
        match self {
            ReplCommand::Help => repl.print_help(),
            ReplCommand::DisplayEnv => println!("{}", repl.environment.borrow()),
            ReplCommand::ClearEnv => {
                repl.environment = Rc::new(RefCell::new(Environment::new()));
                println!("Environment cleared");
            }
            ReplCommand::DisplayMode => repl.print_mode(),
            ReplCommand::SetMode(mode) => {
                repl.set_mode(mode.as_str()).map_err(|_| ())?;
                repl.print_mode();
            }
            ReplCommand::Unknown => {
                println!("Unknown Command");
                return Ok(());
            }
            ReplCommand::Quit => {
                println!("Bye! 👋");
                return Err(());
            }
        };

        Ok(())
    }
}
