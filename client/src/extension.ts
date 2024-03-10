/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import { mkdir, rm } from "node:fs/promises";
import { existsSync } from "node:fs";
import { exec, spawn } from "node:child_process";
import * as vscode from "vscode";
import { get } from "node:https";
import { LanguageClient, LanguageClientOptions, ServerOptions } from "vscode-languageclient/node";
import { registerXmlFileAssociations, registerXPathSemanticTokensProvider } from "./xml";
import { execAsync as $, guessRustTarget, makeStates } from "./utils";

let client: LanguageClient;

const repo = "https://github.com/Desdaemon/odoo-lsp";

async function downloadLspBinary(context: vscode.ExtensionContext) {
	const isWindows = process.platform === "win32";
	const archiveExtension = isWindows ? ".zip" : ".tgz";
	const runtimeDir = context.globalStorageUri.fsPath;
	await mkdir(runtimeDir, { recursive: true });
	const preferNightly = !!vscode.workspace.getConfiguration("odoo-lsp.binary").get("preferNightly");
	const overrideVersion: string | undefined = vscode.workspace
		.getConfiguration("odoo-lsp.binary")
		.get("overrideVersion");

	let release = overrideVersion;
	if (!preferNightly && !overrideVersion) {
		release = context.extension.packageJSON._release || release;
	} else if (preferNightly) {
		release =
			(await new Promise<string | undefined>((resolve, reject) =>
				get(
					"https://api.github.com/repos/Desdaemon/odoo-lsp/releases?per_page=5",
					{
						headers: {
							accept: "application/vnd.github+json",
							"user-agent": "vscode-odoo-lsp",
						},
					},
					(resp) => {
						const chunks: Buffer[] = [];
						resp.on("data", chunks.push.bind(chunks)).on("end", () => {
							try {
								const releases: { tag_name: string; name: string }[] = JSON.parse(Buffer.concat(chunks).toString());
								const latest = releases.find((r) => r.name === "nightly");
								resolve(latest?.tag_name);
							} catch (err) {
								vscode.window.showErrorMessage(`Unable to fetch nightly release: ${err}`);
								resolve(context.extension.packageJSON._release);
							}
						});
					},
				),
			)) || release;
	}
	if (typeof release !== "string" || !release) {
		vscode.window.showErrorMessage(`Bug: invalid release "${release}"`);
		return;
	}
	const latest = `${runtimeDir}/${release}${archiveExtension}`;
	const exeExtension = isWindows ? ".exe" : "";
	const odooLspBin = `${runtimeDir}/odoo-lsp${exeExtension}`;

	const target = guessRustTarget();
	if (!target) {
		enum Actions {
			Ok = "OK",
			Repo = "Go to odoo-lsp",
			InstallSource = "Install from source",
			Binstall = "Install with cargo-binstall",
		}
		const actions = [Actions.Ok, Actions.Repo];
		if (await which("cargo")) {
			actions.push(Actions.InstallSource);
		}
		if (await which("cargo-binstall")) {
			actions.push(Actions.Binstall);
		}
		const resp = await vscode.window.showInformationMessage(
			`odoo-lsp: No prebuilt binaries available for your platform (platform=${process.platform}, arch=${process.arch}).
			Please file an issue at ${repo}, or install odoo-lsp from source.`,
			...actions,
		);
		switch (resp) {
			case Actions.Repo:
				await openLink(repo);
				break;
			case Actions.InstallSource: {
				const channel = vscode.window.createOutputChannel("cargo install odoo-lsp");
				channel.show();
				context.subscriptions.push(channel);
				const cargo = spawn(`cargo install --git ${repo}`, { shell: true });
				cargo.stdout.on("data", (data) => channel.append(data.toString()));
				cargo.stderr.on("data", (data) => channel.append(data.toString()));
				await new Promise((resolve, reject) =>
					cargo.on("exit", (err) => {
						if (err) reject(err);
						else resolve(void 0);
					}),
				);
				channel.hide();
				break;
			}
			case Actions.Binstall: {
				const channel = vscode.window.createOutputChannel("cargo-binstall odoo-lsp");
				channel.show();
				context.subscriptions.push(channel);
				const cargo = spawn("cargo binstall odoo-lsp", { shell: true });
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
		}
		return;
	}
	const link = `${repo}/releases/download/${release}/odoo-lsp-${target}${archiveExtension}`;
	const shaLink = `${link}.sha256`;
	const shaOutput = `${latest}.sha256`;

	const powershell = { shell: "powershell.exe" };
	const sh = { shell: "sh" };
	if (!existsSync(latest)) {
		vscode.window.setStatusBarMessage(`Downloading odoo-lsp@${release}...`, 5);
		try {
			if (isWindows) {
				await $(`Invoke-WebRequest -Uri ${link} -OutFile ${latest}`, powershell);
				await $(`Invoke-WebRequest -Uri ${shaLink} -OutFile ${shaOutput}`, powershell);
				const { stdout } = await $(
					`(Get-FileHash ${latest} -Algorithm SHA256).Hash -eq (Get-Content ${shaOutput})`,
					powershell,
				);
				if (stdout.toString().trim() !== "True") throw new Error("Checksum verification failed");
				await $(`Expand-Archive -Path ${latest} -DestinationPath ${runtimeDir}`, powershell);
			} else {
				await $(`wget -O ${latest} ${link}`, sh);
				await $(`wget -O ${shaOutput} ${shaLink}`, sh);
				await $(
					`if [ "$(shasum -a 256 ${latest} | cut -d ' ' -f 1)" != "$(cat ${shaOutput})" ]; then exit 1; fi`,
					sh,
				);
				await $(`tar -xzf ${latest} -C ${runtimeDir}`, sh);
			}
		} catch (err) {
			// We only build nightly when there are changes, so there will be days without nightly builds.
			if (!(err instanceof Error) || !err.message.includes("404")) {
				vscode.window.showErrorMessage(`Failed to download odoo-lsp binary: ${err}`);
			}
			await rm(latest);
		}
	} else if (!existsSync(odooLspBin)) {
		if (isWindows) {
			await $(`Expand-Archive -Path ${latest} -DestinationPath ${runtimeDir}`, powershell);
		} else {
			await $(`tar -xzf ${latest} -C ${runtimeDir}`, sh);
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
	return await $(`${opener} ${url}`);
}

const makeExtensionState = (context: vscode.ExtensionContext) => makeStates(context, {
	noXmlReminders: Boolean,
	noXPathReminders: Boolean,
});

export type State = ReturnType<typeof makeExtensionState>;

export async function activate(context: vscode.ExtensionContext) {
	const traceOutputChannel = vscode.window.createOutputChannel("Odoo LSP Extension");
	const extensionState = makeExtensionState(context);

	await registerXmlFileAssociations(context, traceOutputChannel, extensionState);
	await registerXPathSemanticTokensProvider(context, traceOutputChannel, extensionState);

	let command = process.env.SERVER_PATH || "odoo-lsp";
	if (!(await which(command))) {
		command = (await downloadLspBinary(context)) || command;
	}
	traceOutputChannel.appendLine(`odoo-lsp executable: ${command}`);
	if (!(await which(command))) {
		vscode.window.showErrorMessage(`no odoo-lsp executable present: ${command}`);
		return;
	}
	const serverOptions: ServerOptions = {
		run: {
			command,
			options: {
				env: { ...process.env, RUST_LOG: process.env.RUST_LOG || "info" },
			},
		},
		debug: {
			command,
			options: {
				env: { ...process.env, RUST_LOG: process.env.RUST_LOG || "debug" },
			},
		},
	};
	const clientOptions: LanguageClientOptions = {
		documentSelector: [
			{ language: "xml", scheme: "file" },
			{ language: "python", scheme: "file" },
			{ language: "javascript", scheme: "file" },
		],
		synchronize: {
			fileEvents: vscode.workspace.createFileSystemWatcher("**/.odoo_lsp*"),
		},
		traceOutputChannel,
	};

	context.subscriptions.push(
		vscode.commands.registerCommand("odoo-lsp.tsconfig", async () => {
			const activeWindow = vscode.window.activeTextEditor?.document.uri.fsPath;
			let folder: vscode.WorkspaceFolder | undefined;
			if (activeWindow) {
				folder = vscode.workspace.workspaceFolders?.find((ws) => activeWindow.includes(ws.uri.fsPath));
			}

			if (!folder) folder = await vscode.window.showWorkspaceFolderPick();
			if (!folder) return;

			const selection =
				(await vscode.window.showOpenDialog({
					canSelectFiles: false,
					canSelectFolders: true,
					canSelectMany: true,
					title: "Select addons roots",
					defaultUri: folder.uri,
				})) ?? [];

			const paths = selection.map((sel) => `--addons-path ${sel.fsPath}`).join(" ");
			const { stdout } = await $(`${command} tsconfig ${paths}`, {
				cwd: folder.uri.fsPath,
			});

			const doc = await vscode.workspace.openTextDocument({
				language: "json",
				content: stdout as string,
			});
			await vscode.window.showTextDocument(doc);
		}),
	);

	context.subscriptions.push(
		vscode.commands.registerCommand("odoo-lsp.statistics", async () => {
			const response = await client.sendRequest("odoo-lsp/statistics");
			const doc = await vscode.workspace.openTextDocument({
				language: "json",
				content: JSON.stringify(response, undefined, 2),
			});
			await vscode.window.showTextDocument(doc);
		}),
	);

	context.subscriptions.push(
		vscode.commands.registerCommand("odoo-lsp.restart-lsp", async () => {
			await client.restart();
			traceOutputChannel.appendLine("Odoo LSP restarted");
		}),
	);

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
