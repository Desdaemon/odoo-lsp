/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import { dirname } from "node:path";
import { mkdir, rm } from "node:fs/promises";
import { ObjectEncodingOptions, existsSync } from "node:fs";
import { exec, spawn, ExecOptions } from "node:child_process";
import { workspace, window, ExtensionContext, ExtensionMode } from "vscode";

import { LanguageClient, LanguageClientOptions, ServerOptions } from "vscode-languageclient/node";

let client: LanguageClient;

function guessRustTarget() {
	const platform = process.platform;
	const arch = process.arch;
	if (platform === "win32") {
		if (arch === "x64") {
			return "x86_64-pc-windows-msvc";
		} else {
			return "i686-pc-windows-msvc";
		}
	} else if (platform === "darwin") {
		if (arch == "x64") {
			return "x86_64-apple-darwin";
		} else if (arch == "arm64") {
			return "aarch64-apple-darwin";
		}
	} else if (platform === "linux") {
		if (arch === "x64") {
			return "x86_64-unknown-linux-gnu";
		} else {
			return "i686-unknown-linux-gnu";
		}
	}
}

function execAsync(command: string, options?: ObjectEncodingOptions & ExecOptions) {
	return new Promise<{ stdout: string | Buffer; stderr: string | Buffer }>((resolve, reject) => {
		exec(command, options, (err, stdout, stderr) => {
			if (err) reject(err);
			else resolve({ stdout, stderr });
		});
	});
}

const REPO = "https://github.com/Desdaemon/odoo-lsp";

async function downloadLspBinary(context: ExtensionContext) {
	const isWindows = process.platform === "win32";
	const archiveExtension = isWindows ? ".zip" : ".tar.gz";
	const runtimeDir = context.globalStorageUri.fsPath;
	await mkdir(runtimeDir, { recursive: true });

	// We follow nightly releases, so only download if today's build is not already downloaded.
	// The format is nightly-YYYYMMDD
	const today = new Date().toISOString().slice(0, 10).replace(/-/g, "");
	const release = `nightly-${today}`;
	const latest = `${runtimeDir}/${release}${archiveExtension}`;
	const exeExtension = isWindows ? ".exe" : "";
	const odooLspBin = `${runtimeDir}/odoo-lsp${exeExtension}`;

	const target = guessRustTarget();
	if (!target) {
		const enum Actions {
			Ok = "OK",
			Repo = "Go to odoo-lsp",
			InstallSource = "Install from source",
		}
		const actions = [Actions.Ok, Actions.Repo];
		// TODO: setup for cargo-binstall
		if (await which("cargo")) {
			actions.push(Actions.InstallSource);
		}
		const resp = await window.showInformationMessage(
			`odoo-lsp: No prebuilt binaries available for your platform (platform=${process.platform}, arch=${process.arch}).
			Please file an issue at ${REPO}, or install odoo-lsp from source.`,
			...actions,
		);
		switch (resp) {
			case Actions.Repo:
				await openLink(REPO);
				break;
			case Actions.InstallSource:
				const channel = window.createOutputChannel("cargo install odoo-lsp");
				channel.show();
				context.subscriptions.push(channel);
				const cargo = spawn("cargo install --git https://github.com/Desdaemon/odoo-lsp", {
					shell: true,
				});
				cargo.stdout.on("data", (data) => {
					channel.append(data.toString());
				});
				cargo.stderr.on("data", (data) => {
					channel.append(data.toString());
				});
				await new Promise((resolve, reject) =>
					cargo.on("exit", (err) => {
						if (err) reject(err);
						else resolve(void 0);
					}),
				);
				channel.hide();
				break;
		}
		return;
	}
	const link = `https://github.com/Desdaemon/odoo-lsp/releases/download/${release}/odoo-lsp-${target}${archiveExtension}`;
	const shaLink = `${link}.sha256`;
	const shaOutput = `${latest}.sha256`;

	if (!existsSync(latest)) {
		try {
			if (isWindows) {
				const powershell = { shell: "powershell.exe" };
				await execAsync(`Invoke-WebRequest -Uri ${link} -OutFile ${latest}`, powershell);
				await execAsync(`Invoke-WebRequest -Uri ${shaLink} -OutFile ${shaOutput}`, powershell);
				const { stdout } = await execAsync(
					`(Get-FileHash ${latest} -Algorithm SHA256).Hash -eq (Get-Content ${shaOutput})`,
					powershell,
				);
				if (stdout.toString().trim() !== "True") throw new Error("Checksum verification failed");
				await execAsync(`Expand-Archive -Path ${latest} -DestinationPath ${runtimeDir}`, powershell);
			} else {
				const sh = { shell: "sh" };
				await execAsync(`wget -O ${latest} ${link}`, sh);
				await execAsync(`wget -O ${shaOutput} ${shaLink}`, sh);
				await execAsync(
					`if [ "$(shasum -a 256 ${latest} | cut -d ' ' -f 1)" != "$(cat ${shaOutput})" ]; then exit 1; fi`,
					sh,
				);
				await execAsync(`tar -xzf ${latest} -C ${runtimeDir}`, sh);
			}
		} catch (err) {
			await window.showErrorMessage(`Failed to download odoo-lsp binary: ${err}`);
			await rm(latest);
		}
	}

	if (existsSync(odooLspBin)) return odooLspBin;
}

function which(bin: string) {
	const checker = process.platform === "win32" ? "where.exe" : "which";
	return new Promise<boolean>((resolve) => {
		exec(`${checker} ${bin}`, (err) => resolve(err === null));
	});
}

async function openLink(url: string) {
	let opener: string;
	if (process.platform === "win32") {
		opener = "start";
	} else if (process.platform === "darwin") {
		opener = "open";
	} else if (await which("wslview")) {
		// from wslu
		opener = "wslview";
	} else {
		opener = "xdg-open";
	}
	return await execAsync(`${opener} ${url}`);
}

export async function activate(context: ExtensionContext) {
	// let disposable = commands.registerCommand("helloworld.helloWorld", async (uri) => {
	// 	// The code you place here will be executed every time your command is executed
	// 	// Display a message box to the user
	// 	const url = Uri.parse("/home/victor/Documents/test-dir/nrs/another.nrs");
	// 	let document = await workspace.openTextDocument(uri);
	// 	await window.showTextDocument(document);

	// 	// console.log(uri)
	// 	window.activeTextEditor.document;
	// 	let editor = window.activeTextEditor;
	// 	let range = new Range(1, 1, 1, 1);
	// 	editor.selection = new Selection(range.start, range.end);
	// });

	// context.subscriptions.push(disposable);

	const traceOutputChannel = window.createOutputChannel("Odoo LSP");
	let command = process.env.SERVER_PATH || "odoo-lsp";
	if (!(await which(command)) && context.extensionMode === ExtensionMode.Production) {
		command = (await downloadLspBinary(context)) || command;
	}
	traceOutputChannel.appendLine(`odoo-lsp executable: ${command}`);
	if (!(await which(command))) {
		await window.showErrorMessage(`no odoo-lsp executable present: ${command}`);
		return;
	}
	const cwd = workspace.workspaceFolders?.length ? dirname(workspace.workspaceFolders![0]!.uri.fsPath) : void 0;
	const serverOptions: ServerOptions = {
		run: {
			command,
			options: {
				cwd,
				env: { ...process.env, RUST_LOG: process.env.RUST_LOG || "info" },
			},
		},
		debug: {
			command,
			options: {
				cwd,
				env: { ...process.env, RUST_LOG: process.env.RUST_LOG || "debug" },
			},
		},
	};
	let clientOptions: LanguageClientOptions = {
		documentSelector: [
			{ language: "xml", scheme: "file" },
			{ language: "python", scheme: "file" },
			{ language: "javascript", scheme: "file" },
		],
		synchronize: {
			fileEvents: workspace.createFileSystemWatcher("**/.odoo_lsp*"),
		},
		traceOutputChannel,
	};

	client = new LanguageClient("odoo-lsp", "Odoo LSP", serverOptions, clientOptions);
	await client.start();
	traceOutputChannel.appendLine("Odoo LSP started");
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
