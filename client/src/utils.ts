import { statSync, type ObjectEncodingOptions } from "node:fs";
import { exec, type ExecOptions } from "node:child_process";
import type { ExtensionContext } from "vscode";

export const isWindows = process.platform === "win32";
export const shell = isWindows ? "powershell.exe" : "sh";

export function guessRustTarget() {
	const platform = process.platform;
	const arch = process.arch;
	if (platform === "win32") {
		if (arch === "x64") return "x86_64-pc-windows-msvc";
		return "i686-pc-windows-msvc";
	}
	if (platform === "darwin") {
		if (arch === "x64") return "x86_64-apple-darwin";
		if (arch === "arm64") return "aarch64-apple-darwin";
	} else if (platform === "linux") {
		if (arch === "x64") return "x86_64-unknown-linux-gnu";
		return "i686-unknown-linux-gnu";
	}
}

export function $(command: string, options?: ObjectEncodingOptions & ExecOptions) {
	return new Promise<{ stdout: string | Buffer; stderr: string | Buffer }>((resolve, reject) => {
		exec(command, options, (err, stdout, stderr) => {
			if (err) reject(err);
			else resolve({ stdout, stderr });
		});
	});
}

export function makeStates<T extends Record<string | number, (..._: unknown[]) => unknown>>(
	context: ExtensionContext,
	schema: T,
) {
	const out = {} as { [K in keyof T]: ReturnType<T[K]> | null };
	for (const key in schema) {
		Object.defineProperty(out, key, {
			get() {
				return context.globalState.get(`odoo-lsp.${key}`) ?? null;
			},
			set(value: ReturnType<T[typeof key]>) {
				context.globalState.update(`odoo-lsp.${key}`, value);
			},
		});
	}
	return out;
}

export async function downloadFile(src: string, dest: string) {
	if (isWindows) {
		await $(`Invoke-WebRequest -Uri ${src} -OutFile ${dest}`, { shell });
	} else {
		await $(`curl -Lo ${dest} ${src}`, { shell });
	}
}

export function tryStatSync(path: string) {
	try {
		return statSync(path);
	} catch {
		return null;
	}
}

export function compareDate(lhs: Date, rhs: Date) {
	const left = lhs.setHours(0, 0, 0, 0);
	const right = rhs.setHours(0, 0, 0, 0);
	return left - right;
}

export function which(bin: string) {
	const checker = isWindows ? "Get-Command" : "which";
	return new Promise<boolean>((resolve) => {
		exec(`${checker} ${bin}`, { shell }, (err) => resolve(err === null));
	});
}

export async function openLink(url: string) {
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

export function parseNightly(releaseName: string) {
	if (!releaseName.startsWith("nightly-")) {
		throw new Error(`bug: releaseName=${releaseName} is not a nightly release`);
	}
	// nightly-YYYYMMDD
	const dateString = releaseName.slice("nightly-".length);
	const yearString = dateString.slice(0, 4);
	const monthString = dateString.slice(4, 6);
	const dayString = dateString.slice(6, 8);
	return new Date(Date.UTC(+yearString, +monthString - 1, +dayString + 1));
}
