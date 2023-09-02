use std::fmt::Display;

use crate::{code::Opcode, object::Object};

pub type Result<T = ()> = std::result::Result<T, VirtualMachineError>;
pub enum VirtualMachineError {
    StackOverflow,
    StackUnderflow,
    UnsupportedAddition(Object, Object),
    UnknownIntegerOperator(Opcode),
    UnknownBooleanOperator(Opcode),
}

impl Display for VirtualMachineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            VirtualMachineError::StackOverflow => write!(f, "Stack overflow"),
            VirtualMachineError::StackUnderflow => write!(f, "Stack underflow"),
            VirtualMachineError::UnsupportedAddition(left, right) => {
                write!(f, "Unsupported addition: {:?} + {:?}", left, right)
            }
            VirtualMachineError::UnknownIntegerOperator(opcode) => {
                write!(f, "Unknown integer operator: {:?}", opcode)
            }
            VirtualMachineError::UnknownBooleanOperator(opcode) => {
                write!(f, "Unknown boolean operator: {:?}", opcode)
            }
        }
    }
}
