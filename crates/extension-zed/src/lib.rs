use std::fs;

use serde::{Deserialize, Serialize};
use zed_extension_api::{self as zed, Result, serde_json, settings::LspSettings};

#[derive(Serialize, Deserialize, Default)]
struct Extension {
	last_version: Option<String>,
}

macro_rules! format_loc {
	($lit:literal, $($tt:tt)*) => {
		format!(concat!("[", file!(), ":", line!(), "] ", $lit), $($tt)*)
	};
}

const STATE_FILE: &str = ".odoo-lsp.state.json";

impl zed::Extension for Extension {
	#[inline]
	fn new() -> Self
	where
		Self: Sized,
	{
		fs::File::open(STATE_FILE)
			.ok()
			.and_then(|file| serde_json::from_reader(file).ok())
			.unwrap_or_default()
	}

	fn language_server_command(&mut self, lsid: &zed::LanguageServerId, dir: &zed::Worktree) -> Result<zed::Command> {
		let env = self.prepare_env(dir);

		if let Some(binary) = LspSettings::for_worktree("odoo-lsp", dir)
			.ok()
			.and_then(|settings| settings.binary)
		{
			if let Some(command) = binary.path.clone() {
				return Ok(zed::Command {
					command,
					args: binary.arguments.unwrap_or_default(),
					env,
				});
			}
		}

		if let Some(command) = dir.which("odoo-lsp") {
			return Ok(zed::Command {
				command,
				args: vec![],
				env,
			});
		}

		// ==========================================================================================================
		zed::set_language_server_installation_status(lsid, &zed::LanguageServerInstallationStatus::CheckingForUpdate);

		let release = zed::latest_github_release(
			"Desdaemon/odoo-lsp",
			zed::GithubReleaseOptions {
				require_assets: true,
				pre_release: true,
			},
		)?;
		let version = release.version;

		let (platform, _arch) = zed::current_platform();
		let (archive_ext, binary_ext, archive_type) = match platform {
			zed::Os::Windows => (".zip", ".exe", zed::DownloadedFileType::Zip),
			zed::Os::Mac | zed::Os::Linux => (".tgz", "", zed::DownloadedFileType::GzipTar),
		};

		let binary_dir = format!("odoo-lsp-{version}");
		let binary_path = format!("{binary_dir}/odoo-lsp{binary_ext}");

		// TODO: semantic version comparison
		if is_file(&binary_path) && (self.last_version.as_ref()).is_some_and(|last_version| version == *last_version) {
			return Ok(zed::Command {
				command: binary_path,
				args: vec![],
				env,
			});
		}

		// ==========================================================================================================
		zed::set_language_server_installation_status(lsid, &zed::LanguageServerInstallationStatus::Downloading);
		let target = guess_rust_target()?;

		let url =
			format!("https://github.com/Desdaemon/odoo-lsp/releases/download/{version}/odoo-lsp-{target}{archive_ext}");
		zed::download_file(&url, &binary_dir, archive_type)?;
		zed::make_file_executable(&binary_path)?;

		// remove old versions
		let entries = fs::read_dir(".").map_err(|err| format_loc!("read_dir failed: {}", err))?;
		for entry in entries {
			let Ok(entry) = entry else { continue };
			if (entry.file_name().to_str())
				.is_none_or(|path| !path.ends_with(&binary_dir) && !path.ends_with(STATE_FILE))
			{
				_ = fs::remove_dir_all(entry.path());
			}
		}

		self.last_version = Some(version);
		_ = serde_json::to_writer(
			fs::File::create(STATE_FILE).map_err(|err| format_loc!("failed to open state file: {}", err))?,
			&self,
		);

		Ok(zed::Command {
			command: binary_path,
			args: vec![],
			env,
		})
	}

	fn language_server_workspace_configuration(
		&mut self,
		_lsid: &zed::LanguageServerId,
		worktree: &zed::Worktree,
	) -> Result<Option<zed::serde_json::Value>> {
		let settings = LspSettings::for_worktree("odoo-lsp", worktree)
			.ok()
			.and_then(|lsp_settings| lsp_settings.settings.clone())
			.unwrap_or_default();
		Ok(Some(settings))
	}
}

impl Extension {
	fn prepare_env(&self, dir: &zed::Worktree) -> Vec<(String, String)> {
		let (platform, _arch) = zed::current_platform();
		let mut env = match platform {
			zed::Os::Mac | zed::Os::Linux => dir.shell_env(),
			zed::Os::Windows => vec![],
		};

		if !has_env_key(&env, "RUST_LOG") {
			env.push(("RUST_LOG".to_string(), "warn,odoo_lsp=info".to_string()));
		}
		if !has_env_key(&env, "RUST_LOG_STYLE") {
			env.push(("RUST_LOG_STYLE".to_string(), "never".to_string()));
		}
		if !has_env_key(&env, "NO_COLOR") {
			env.push(("NO_COLOR".to_string(), "1".to_string()));
		}

		env
	}
}

fn guess_rust_target() -> Result<&'static str> {
	Ok(match zed::current_platform() {
		(zed::Os::Linux, zed::Architecture::X8664) => "x86_64-unknown-linux-gnu",
		(zed::Os::Linux, zed::Architecture::X86) => "i686-unknown-linux-gnu",
		(zed::Os::Linux, zed::Architecture::Aarch64) => return Err("Linux ARM64 not supported".into()),
		(zed::Os::Mac, zed::Architecture::X8664) => "x86_64-apple-darwin",
		(zed::Os::Mac, zed::Architecture::Aarch64) => "aarch64-apple-darwin",
		(zed::Os::Mac, zed::Architecture::X86) => return Err("MacOS x86 not supported".into()),
		(zed::Os::Windows, zed::Architecture::X8664) => "x86_64-pc-windows-msvc",
		(zed::Os::Windows, zed::Architecture::X86) => "i686-pc-windows-msvc",
		(zed::Os::Windows, zed::Architecture::Aarch64) => return Err("Windows ARM64 not supported".into()),
	})
}

fn has_env_key(env: &[(String, String)], key: &str) -> bool {
	env.iter().any(|(envkey, _)| envkey == key)
}

#[inline]
fn is_file(path: &str) -> bool {
	fs::metadata(path).is_ok_and(|stat| stat.is_file())
}

zed::register_extension!(Extension);
