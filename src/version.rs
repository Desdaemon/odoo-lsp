pub const NAME: &str = env!("CARGO_PKG_NAME");
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const GIT_VERSION: &str = git_version::git_version!(args = ["--tags", "--candidates=0"], fallback = "");
const GIT_TAG: &str = git_version::git_version!();
pub const GITVER: &str = if GIT_VERSION.is_empty() { GIT_TAG } else { GIT_VERSION };
