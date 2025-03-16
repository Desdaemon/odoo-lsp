/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import { mkdir, rm } from "node:fs/promises";
import { type Stats, existsSync } from "node:fs";
import { spawn } from "node:child_process";
import { get } from "node:https";
import {
	LanguageClient,
	LanguageClientOptions,
	RevealOutputChannelOn,
	ServerOptions,
} from "vscode-languageclient/node";
import { registerXmlFileAssociations, registerXPathSemanticTokensProvider } from "./xml";
import {
	compareDate,
	downloadFile,
	guessRustTarget,
	isWindows,
	makeStates,
	openLink,
	parseNightly,
	tryStatSync,
	which,
} from "./utils";
import * as vscode from "vscode";
import { $ } from "execa";

let client: LanguageClient;
let extensionState: State;

const repo = "https://github.com/Desdaemon/odoo-lsp";

async function downloadLspBinary(context: vscode.ExtensionContext) {
	const archiveExtension = isWindows ? ".zip" : ".tgz";
	const runtimeDir = context.globalStorageUri.fsPath;
	await mkdir(runtimeDir, { recursive: true });
	const preferNightly = !!vscode.workspace.getConfiguration("odoo-lsp.binary").get("preferNightly");
	const overrideVersion: string | undefined = vscode.workspace
		.getConfiguration("odoo-lsp.binary")
		.get("overrideVersion");

	let release = overrideVersion;
	if (!preferNightly && !overrideVersion) {
		release = context.extension.packageJSON._release || `v${context.extension.packageJSON.version}`;
	} else if (preferNightly) {
		release = await latestReleaseInfo(true, release);
	}
	if (typeof release !== "string" || !release) {
		vscode.window.showErrorMessage(`Bug: invalid release "${release}"`);
		return;
	}

	const archiveName = release.startsWith("nightly-") ? "nightly" : release;
	const latest = `${runtimeDir}/${archiveName}${archiveExtension}`;
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

	const hasNewerNightly = (() => {
		if (!release.startsWith("nightly-")) return false;
		const releaseDate = parseNightly(release);
		const latestStat = tryStatSync(latest);
		if (!latestStat) return true;
		return compareDate(latestStat.ctime, releaseDate) < 0;
	})();

	const powershell = { shell: "powershell.exe" };
	const sh = { shell: "sh" };
	if (!existsSync(latest) || hasNewerNightly) {
		await vscode.window.withProgress(
			{ location: vscode.ProgressLocation.Notification, title: `Downloading odoo-lsp@${release}...` },
			async () => {
				try {
					await downloadFile(link, latest);
					await downloadFile(shaLink, shaOutput);
					if (isWindows) {
						const { stdout } = await $(powershell)`(Get-Filehash ${latest} -Algorithm SHA256).Hash -eq (Get-Content ${shaOutput})`;
						if (stdout.trim() !== "True") throw new Error("Checksum verification failed");
						await $(powershell)`Expand-Archive -Path ${latest} -DestinationPath ${runtimeDir}`;
					} else {
						await $(sh)`if [ "$(shasum -a 256 ${latest} | cut -d ' ' -f 1)" != "$(cat ${shaOutput})" ]; then exit 1; fi`
						await $`tar -xzf ${latest} -C ${runtimeDir}`;
					}
				} catch (err) {
					// We only build nightly when there are changes, so there will be days without nightly builds.
					if (hasNewerNightly) vscode.window.showErrorMessage(`Failed to download odoo-lsp binary: ${err}`);
					await rm(latest, { force: true });
				}
			},
		);
	} else if (!existsSync(odooLspBin)) {
		if (isWindows) {
			await $(powershell)`Expand-Archive -Path ${latest} -DestinationPath ${runtimeDir}`;
		} else {
			await $`tar -xzf ${latest} -C ${runtimeDir}`;
		}
	}

	if (extensionState.nightlyExtensionUpdates !== "never" && preferNightly && hasNewerNightly) {
		updateExtension(context, release);
	}

	if (existsSync(odooLspBin)) return odooLspBin;
}

async function latestReleaseInfo(includeStable: boolean, fallback?: string) {
	return (
		(await new Promise<string | undefined>((resolve) =>
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
							const releases: { tag_name: string; name: string; published_at: string }[] = JSON.parse(
								Buffer.concat(chunks).toString(),
							);
							const today = new Date();
							releases.sort((a, z) => z.published_at.localeCompare(a.published_at));
							const latest = releases.find(
								(r) => (includeStable || r.name === "nightly") && compareDate(today, new Date(r.published_at)) >= 0,
							);
							resolve(latest?.tag_name || fallback);
						} catch (err) {
							vscode.window.showErrorMessage(`Unable to fetch nightly release: ${err}`);
							resolve(fallback);
						}
					});
				},
			),
		)) || fallback
	);
}

function updateExtension(context: vscode.ExtensionContext, release: string) {
	const runtimeDir = context.globalStorageUri.fsPath;
	const vsixLink = `${repo}/releases/download/${release}/odoo-lsp-${context.extension.packageJSON.version}.vsix`;
	const vsixOutput = `${runtimeDir}/odoo-lsp.vsix`;
	downloadFile(vsixLink, vsixOutput).then(
		async () => {
			const resp =
				extensionState.nightlyExtensionUpdates === "always"
					? "Yes"
					: await vscode.window.showInformationMessage(
						"A new nightly update for the extension is available. Install?",
						"Yes",
						"No",
						"Always",
						"Never show again",
					);
			if (resp === "Always") extensionState.nightlyExtensionUpdates = "always";
			else if (resp === "Never show again") extensionState.nightlyExtensionUpdates = "never";

			if (resp === "Yes" || resp === "Always") {
				try {
					await vscode.commands.executeCommand("workbench.extensions.installExtension", vscode.Uri.file(vsixOutput));
				} catch (err) {
					vscode.window.showErrorMessage(`Failed to update extension: ${err}`);
					return;
				}
				vscode.window
					.showInformationMessage(`Extension updated to ${release}. Reload to apply changes.`, "Reload now", "Later")
					.then((resp) => {
						if (resp === "Reload now") vscode.commands.executeCommand("workbench.action.reloadWindow");
					});
			}
		},
		(reason) => {
			vscode.window.showErrorMessage(`Failed to download extension update: ${reason}`);
		},
	);
}

const makeExtensionState = (context: vscode.ExtensionContext) =>
	makeStates(context, {
		noXmlReminders: Boolean,
		noXPathReminders: Boolean,
		nightlyExtensionUpdates: String as () => "always" | "never",
	});

export type State = ReturnType<typeof makeExtensionState>;

export async function activate(context: vscode.ExtensionContext) {
	const traceOutputChannel = vscode.window.createOutputChannel("Odoo LSP Extension", { log: true });
	context.subscriptions.push(traceOutputChannel);
	extensionState = makeExtensionState(context);

	await registerXmlFileAssociations(context, traceOutputChannel, extensionState);
	await registerXPathSemanticTokensProvider(context, traceOutputChannel, extensionState);

	let command = process.env.SERVER_PATH || "odoo-lsp";
	if (!(await which(command))) {
		command = (await downloadLspBinary(context)) || command;
	}
	traceOutputChannel.info(`odoo-lsp executable: ${command}`);
	if (!(await which(command))) {
		vscode.window.showErrorMessage(`no odoo-lsp executable present: ${command}`);
		return;
	}

	// Update the extension when using local binary
	const preferNightly = !!vscode.workspace.getConfiguration("odoo-lsp.binary").get("preferNightly");
	const runtimeDir = context.globalStorageUri.fsPath;
	let latestRelease: string | undefined;
	let vsixStat: Stats | null;
	if (
		preferNightly &&
		command === "odoo-lsp" &&
		(latestRelease = await latestReleaseInfo(false)) &&
		latestRelease.startsWith("nightly-") &&
		(!(vsixStat = tryStatSync(`${runtimeDir}/odoo-lsp.vsix`)) ||
			compareDate(vsixStat.ctime, parseNightly(latestRelease)) < 0)
	) {
		updateExtension(context, latestRelease);
	}

	const logLevel = vscode.workspace.getConfiguration("odoo-lsp.trace").get("binary");
	const RUST_LOG_STYLE = "never";
	const NO_COLOR = "1";
	const args = ["--log-format", "json"];
	const serverOptions: ServerOptions = {
		run: {
			command,
			args,
			options: {
				env: {
					...process.env,
					RUST_LOG: process.env.RUST_LOG || `info,odoo_lsp=${logLevel}`,
					RUST_LOG_STYLE,
					NO_COLOR,
				},
			},
		},
		debug: {
			command,
			args,
			options: {
				env: {
					...process.env,
					RUST_LOG: process.env.RUST_LOG || `debug,odoo_lsp=${logLevel}`,
					RUST_LOG_STYLE,
					NO_COLOR,
				},
			},
		},
	};

	const binaryOutputChannel = vscode.window.createOutputChannel("Odoo LSP", { log: true });
	const splitPattern = /\n/gm;

	const oldAppend = binaryOutputChannel.append.bind(binaryOutputChannel);
	binaryOutputChannel.append = function(this: vscode.LogOutputChannel, lines: string) {
		if (!lines) return;

		for (const line of lines.split(splitPattern)) {
			try {
				let { level, fields, target, spans } = JSON.parse(line) || {};

				let message = '';
				if (fields?.message) {
					message = fields.message;
					delete fields.message;
				}
				const formattedFields = Array.from(Object.entries(fields || {})).map(([key, val]) => `${key}=${val}`);
				const components = [message, ...formattedFields].map(e => e.trim()).filter(Boolean).join(' ');
				if (spans?.length) {
					const formatted = spans.map(span => span.name).join('/');
					if (formatted) target = `${target}:${formatted}`;
				}
				
				switch (level) {
				case 'INFO': this.info(`   [${target}] ${components}`); continue;
				case 'WARN': this.warn(`[${target}] ${components}`); continue;
				case 'ERROR': this.error(`  [${target}] ${components}`); continue;
				case 'DEBUG': this.debug(`  [${target}] ${components}`); continue;
				case 'TRACE': this.trace(`  [${target}] ${components}`); continue;
				}
			} catch {}
			if (line.trim()) oldAppend(line);
		}
	}.bind(binaryOutputChannel);

	const clientOptions = {
		documentSelector: [
			{ language: "xml", scheme: "file" },
			{ language: "python", scheme: "file" },
			{ language: "javascript", scheme: "file" },
		],
		outputChannel: binaryOutputChannel,
		traceOutputChannel,
		revealOutputChannelOn: process.env.RUST_LOG ? RevealOutputChannelOn.Info : undefined,
	} satisfies LanguageClientOptions;

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
		vscode.commands.registerCommand("odoo-lsp.restart-lsp", async () => {
			await client.restart();
			traceOutputChannel.info("Odoo LSP restarted");
		}),
	);

	context.subscriptions.push(
		vscode.commands.registerCommand("odoo-lsp.debug.intern", async () => {
			const response: Record<string, [string, number][]> | null = await client.sendRequest("odoo-lsp/debug/usage");
			if (!response) return;
			const formatted = Object.fromEntries(
				Object.entries(response).map(([key, lines]) => [key, Object.fromEntries(lines)]),
			);
			const doc = await vscode.workspace.openTextDocument({
				language: "json",
				content: JSON.stringify(formatted, undefined, 2),
			});
			await vscode.window.showTextDocument(doc);
		}),
	);

	client = new LanguageClient("odoo-lsp", "Odoo LSP", serverOptions, clientOptions);
	await client.start();
	traceOutputChannel.info("Odoo LSP started");

	return { client, serverOptions };
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
