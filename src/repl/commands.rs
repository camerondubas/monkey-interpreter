use std::{cell::RefCell, rc::Rc};

use crate::object::Environment;

use super::Repl;

pub enum ReplCommand {
    Help,
    Quit,
    Unknown,
    DisplayEnv,
    ClearEnv,
    ToggleTracer,
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
            "tracer" => ReplCommand::ToggleTracer,
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
            ReplCommand::ToggleTracer => {
                let message;

                if repl.tracer_enabled {
                    repl.tracer_enabled = false;
                    message = "Disabled";
                } else {
                    repl.tracer_enabled = true;
                    message = "Enabled";
                }

                println!("Tracer {}", message);
            }
            ReplCommand::DisplayMode => repl.print_mode(),
            ReplCommand::SetMode(mode) => {
                repl.set_mode(mode.as_str()).map_err(|_| ())?;
                repl.print_mode();
            }
            ReplCommand::Unknown => {
                println!("Unknown Command");
                return Err(());
            }
            ReplCommand::Quit => {
                println!("Bye! 👋");
                return Err(());
            }
        };

        Ok(())
    }
}
