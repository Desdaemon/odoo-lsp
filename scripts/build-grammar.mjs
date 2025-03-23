#!/usr/bin/env node
import { load } from "js-yaml";
import { readFile, writeFile } from "fs";

const fileMap = {
	"syntaxes/gen/odoo-xml.yml": "syntaxes/odoo-xml.tmLanguage.json",
	"syntaxes/gen/odoo-owl.yml": "syntaxes/odoo-owl.tmLanguage.json",
	"syntaxes/gen/odoo-python.yml": "syntaxes/odoo-python.tmLanguage.json",
};

const onigurumaComment = /\(\?#[^)]*\)/g;
for (const input in fileMap) {
	readFile(input, "utf8", (readErr, data) => {
		if (readErr) return console.error({ readErr });
		const parsed = load(data);
		const trimmed = JSON.stringify(parsed).replace(onigurumaComment, "");
		writeFile(fileMap[input], trimmed, (writeErr) => {
			if (writeErr) console.error({ writeErr });
		});
	});
}
