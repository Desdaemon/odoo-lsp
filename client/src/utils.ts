import { ObjectEncodingOptions } from "node:fs";
import { exec, ExecOptions } from "node:child_process";
import type { ExtensionContext } from "vscode";

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

export function execAsync(command: string, options?: ObjectEncodingOptions & ExecOptions) {
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
	const out = {} as { [K in keyof T]: ReturnType<T[K]> };
	for (const key in schema) {
		Object.defineProperty(out, key, {
			get() {
				return context.globalState.get(`odoo-lsp.${key}`) ?? null;
			},
			set(value: ReturnType<T[typeof key]>) {
				context.globalState.update(`odoo-lsp.${key}`, value);
			},
		})
	}
	return out;
}
