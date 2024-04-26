#!/usr/bin/env zx
import { Octokit } from "@octokit/rest";

const cutOff = 10;
const owner = "Desdaemon";
const repo = "odoo-lsp";
let auth = await $`gh auth token`;
auth = auth.stdout.trim();

const gh = new Octokit({ request: { fetch }, auth });
const releases = await gh.repos.listReleases({ owner, repo });
const oldReleases = releases.data
	.filter((rel) => rel.prerelease && rel.tag_name.startsWith("nightly"))
	.sort((a, z) => z.created_at.localeCompare(a.created_at))
	.slice(cutOff);
for (const rel of oldReleases) {
	console.log(rel.tag_name);
	for (const asset of rel.assets) {
		console.log(".. delete ", asset.name);
		await gh.repos.deleteReleaseAsset({ owner, repo, asset_id: asset.id });
	}
}
