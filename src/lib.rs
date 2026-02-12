// #![warn(clippy::cognitive_complexity)]
#![deny(clippy::unused_async)]
#![deny(clippy::await_holding_invalid_type)]

pub mod config;
pub mod index;
pub mod str;
pub mod utils;
pub use str::ImStr;

pub mod analyze;
pub mod backend;
pub mod component;
pub mod model;
pub mod record;
pub mod server;
pub mod stub_finder;
pub mod template;

mod js;
pub mod python;
pub mod xml;

pub mod version;
pub use version::{GIT_VERSION, GITVER, NAME, VERSION};

pub mod prelude;

pub mod test_utils;
