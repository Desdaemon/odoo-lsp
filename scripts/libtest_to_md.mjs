#!/usr/bin/env node
import { createReadStream } from "fs";
import { createInterface } from "readline";

const inputFile = process.argv[2];
const rl = createInterface({ input: inputFile ? createReadStream(inputFile) : process.stdin, crlfDelay: Infinity });

function suiteKeyFromNextest(nextest) {
	const crate = nextest?.crate ?? "unknown";
	const binary = nextest?.test_binary ?? "unknown";
	return `${crate}::${binary}`;
}

function suiteKeyFromTestName(name) {
	const separator = name.indexOf("$");
	return separator === -1 ? null : name.slice(0, separator);
}

function getOrCreateSuite(suites, suiteMap, key, nextest = {}) {
	let suite = suiteMap.get(key);
	if (suite) {
		if (suite.crate === "unknown" && nextest.crate) suite.crate = nextest.crate;
		if (suite.binary === "unknown" && nextest.test_binary) suite.binary = nextest.test_binary;
		if (suite.kind === "unknown" && nextest.kind) suite.kind = nextest.kind;
		return suite;
	}

	suite = {
		key,
		crate: nextest.crate ?? "unknown",
		binary: nextest.test_binary ?? "unknown",
		kind: nextest.kind ?? "unknown",
		testCount: 0,
		tests: [],
		summary: null,
	};
	suites.push(suite);
	suiteMap.set(key, suite);
	return suite;
}

function stripAnsi(text) {
	return text.replaceAll(/\u001b\[[0-9;]*m/g, "");
}

function pushCodeBlock(lines, language, content) {
	lines.push(`\`\`\`${language}`);
	lines.push(content);
	lines.push("```");
}

function pushFormattedStdout(lines, stdout) {
	const cleaned = stripAnsi(stdout).trimEnd();
	if (!cleaned) return;

	const stdoutLines = cleaned.split("\n");
	const diffHeader = "Diff < left / right > :";
	const diffIndex = stdoutLines.findIndex((line) => line.trim() === diffHeader);

	if (diffIndex === -1) {
		pushCodeBlock(lines, "text", cleaned);
		return;
	}

	const prelude = stdoutLines.slice(0, diffIndex).join("\n").trimEnd();
	const diffBody = stdoutLines.slice(diffIndex + 1).join("\n").trimEnd();

	if (prelude) {
		pushCodeBlock(lines, "text", prelude);
	}
	if (diffBody) {
		pushCodeBlock(lines, "diff", `--- left\n+++ right\n${diffBody}`);
	}
}

const suites = [];
const suiteMap = new Map();
const testResults = new Map();

for await (const line of rl) {
	if (!line.trim()) continue;
	let obj;
	try {
		obj = JSON.parse(line);
	} catch {
		continue;
	}

	if (obj.type === "suite" && obj.event === "started") {
		const key = suiteKeyFromNextest(obj.nextest);
		const suite = getOrCreateSuite(suites, suiteMap, key, obj.nextest);
		suite.testCount = obj.test_count ?? suite.testCount;
	} else if (obj.type === "suite" && (obj.event === "ok" || obj.event === "failed")) {
		const key = suiteKeyFromNextest(obj.nextest);
		const suite = getOrCreateSuite(suites, suiteMap, key, obj.nextest);
		suite.summary = {
			result: obj.event,
			passed: obj.passed ?? 0,
			failed: obj.failed ?? 0,
			ignored: obj.ignored ?? 0,
			execTime: obj.exec_time ?? 0,
		};
	} else if (obj.type === "test" && obj.event === "started") {
		const suiteKey = suiteKeyFromTestName(obj.name);
		if (suiteKey) getOrCreateSuite(suites, suiteMap, suiteKey);
		testResults.set(obj.name, {
			name: obj.name,
			status: "running",
			suiteKey,
			execTime: 0,
			stdout: null,
		});
	} else if (obj.type === "test" && (obj.event === "ok" || obj.event === "failed")) {
		const suiteKey = suiteKeyFromTestName(obj.name);
		if (suiteKey) getOrCreateSuite(suites, suiteMap, suiteKey);
		const entry = {
			name: obj.name,
			status: obj.event,
			suiteKey,
			execTime: obj.exec_time ?? 0,
			stdout: obj.stdout ?? null,
		};
		testResults.set(obj.name, entry);
	}
}

for (const suite of suites) {
	suite.tests = [];
}
for (const [, entry] of testResults) {
	if (entry.status === "running") continue;
	const suite = entry.suiteKey ? suiteMap.get(entry.suiteKey) : null;
	if (suite) suite.tests.push(entry);
}

// Build markdown
const lines = [];

const totalPassed = suites.reduce((s, x) => s + (x.summary?.passed ?? 0), 0);
const totalFailed = suites.reduce((s, x) => s + (x.summary?.failed ?? 0), 0);
const totalTests = totalPassed + totalFailed;
const overallResult = totalFailed === 0 ? "✅ PASSED" : "❌ FAILED";

lines.push(`# Test Report`);
lines.push(``);
lines.push(`**Result:** ${overallResult}  `);
lines.push(`**Total:** ${totalTests} tests — ${totalPassed} passed, ${totalFailed} failed`);
lines.push(``);

for (const suite of suites) {
	const s = suite.summary;
	if (!s) continue;
	const icon = s.result === "ok" ? "✅" : "❌";
	lines.push(`## ${icon} ${suite.crate} (${suite.kind})`);
	lines.push(``);
	lines.push(`| | |`);
	lines.push(`|---|---|`);
	lines.push(`| Passed | ${s.passed} |`);
	lines.push(`| Failed | ${s.failed} |`);
	if (s.ignored) lines.push(`| Ignored | ${s.ignored} |`);
	lines.push(`| Duration | ${s.execTime.toFixed(3)}s |`);
	lines.push(``);

	const failed = suite.tests.filter((t) => t.status === "failed");
	if (failed.length > 0) {
		lines.push(`### Failed Tests`);
		lines.push(``);
		for (const t of failed) {
			// strip suite prefix for display
			const prefix = `${suite.crate}::${suite.binary}$`;
			const shortName = t.name.startsWith(prefix) ? t.name.slice(prefix.length) : t.name;
			lines.push(`#### \`${shortName}\``);
			lines.push(``);
			if (t.stdout) {
				pushFormattedStdout(lines, t.stdout);
			}
			lines.push(``);
		}
	}
}

process.stdout.write(lines.join("\n") + "\n");
