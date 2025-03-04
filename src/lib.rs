#![warn(clippy::cognitive_complexity)]
#![deny(clippy::unused_async)]
#![deny(clippy::await_holding_invalid_type)]

pub mod config;
pub mod index;
pub mod str;
pub mod utils;
pub use str::ImStr;

pub mod analyze;
pub mod component;
pub mod model;
pub mod record;
pub mod template;

pub(crate) mod test_utils;
