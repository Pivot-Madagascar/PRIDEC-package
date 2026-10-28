# Contributing and Releasing

This package uses [release-please](https://github.com/googleapis/release-please) to manage versions. It reads the commit messages on `main`, works out the next version number, updates `DESCRIPTION` and `NEWS.md`, and tags a release. This only works if commit messages follow the [Conventional Commits](https://www.conventionalcommits.org/) format, so the steps below matter.

It follows Pivot's standard application development git flow described [here](https://github.com/Pivot-Madagascar/pivot-wiki/wiki/Application-Development-Git-Workflow.)

## Overview

```
Issue  ->  Branch  ->  Pull request  ->  Squash merge to dev  ->  Merge to main  ->  Release PR  ->  Merge Release PR  ->  New release
```

Merging a normal PR does **not** publish a release by itself. It adds changes to an automatically maintained "Release PR". The release happens when someone merges that Release PR.

## 1. Start from an issue

- Open a GitHub issue describing the bug, feature or task. It should be named using kebab case following <issue  number>-description-of-issue.
- Decide what kind of change it is (see the table in step 3). This determines the PR title prefix.

## 2. Make a branch and open a PR

- Branch from `dev`, for example `42-inla-sampling` or `17-get-dhis-data`.
- Open a PR into `dev` and link the issue in the description with `Closes #42`.
- Run `devtools::check()` and `devtools::test()` before requesting review.

## 3. Write the PR title as a conventional commit

On squash merge, GitHub uses the **PR title** as the commit message, and release-please reads that. The title must look like this:

```
<type>: <short description>
```

| Type | Use for | Version bump | NEWS.md section |
|------|---------|--------------|-----------------|
| `feat` | New functionality | minor (0.1.0 -> 0.2.0) | Features |
| `fix` | Bug fix | patch (0.1.0 -> 0.1.1) | Bug Fixes |
| `refactor`, `chore`, `docs`, `test`, `build` | Internal changes | none on their own | Internal |
| `feat!` / `fix!` or a `BREAKING CHANGE:` footer | Breaking change | major | (listed under its type) |

Examples:

- `feat: add get_dhis_data function`
- `fix: change INLA sampling default to help with fitting`
- `docs: update forecast vignette`

A title without a recognised prefix (for example "update stuff") is ignored by release-please and will not appear in `NEWS.md` or trigger a version bump.

### If one PR contains both a feat and a fix

A title can only have one type. To record several changes, add an override block at the bottom of the **PR description** before merging:

```
BEGIN_COMMIT_OVERRIDE
feat: add get_dhis_data function
fix: correct INLA sampling default
END_COMMIT_OVERRIDE
```

Release-please then creates one `NEWS.md` entry per line instead of using the title. The highest-ranking type sets the bump (here, `feat` gives a minor bump). If you forget, you can edit the PR description after merging and release-please will pick it up on its next run.

Better still, keep PRs small and focused on one type of change.

## 4. Squash merge into dev

- Always use **Squash and merge** into dev, not a regular merge or rebase, so the PR title is the single commit message.
- Check the commit title in the merge dialog before confirming. It should be the conventional-commit title.

## 5. Release to production via a PR to main

The `dev` branch should be merged into `main` fairly regularly, either when a PR is accepted into dev, or at least weekly. This should be a normal merge that will contain all of the squashed-merge commit messages on `dev`.

After each push to `main`, the release-please GitHub Action (`.github/workflows/release-please.yml`) opens or updates a PR titled `chore(release): release vX.Y.Z`. It contains:

- the new `Version:` in `DESCRIPTION`
- new entries in `NEWS.md`
- the updated `.release-please-manifest.json`

More merged PRs keep accumulating in this same Release PR. Do not edit it by hand.

## 6. Merge the Release PR to publish

When you are ready to release:

1. Review the version number and `NEWS.md` entries in the Release PR.
2. Merge it (this one is not a feature PR, so merge it as is).
3. Release-please tags `vX.Y.Z` and creates a GitHub release with the notes.

If nothing releasable (`feat` or `fix`) has been merged since the last release, no Release PR is opened.

## Quick checklist

- [ ] Issue exists and is linked from the PR (`Closes #N`)
- [ ] PR title starts with `feat:`, `fix:`, etc.
- [ ] Mixed PR? Add a `BEGIN_COMMIT_OVERRIDE` block to the description
- [ ] Merged with **Squash and merge**
- [ ] Merge the Release PR when ready to publish
