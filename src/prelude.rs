//! Useful common imports.

pub use anyhow::{Context, anyhow};
pub use futures::{StreamExt, stream::FuturesUnordered};
pub use lasso::{Key, Spur};
pub use ropey::{Rope, RopeSlice};
pub use serde::{Deserialize, Serialize};
pub use tower_lsp_server::UriExt;
pub use tower_lsp_server::lsp_types::Range;
pub use tower_lsp_server::lsp_types::Uri;
pub use tracing::{debug, error, info, instrument, trace, warn};
pub use tree_sitter::{Node, Parser, QueryCursor};
pub use ts_macros::query;

pub use crate::index::symbol::{_G, _I, _P, _R, PathSymbol, Symbol};
pub use crate::utils::*;
pub use crate::{ImStr, dig, errloc, format_loc, loc, ok, some, test_utils};
