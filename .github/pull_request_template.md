<!--
Thanks for contributing to MPC_designations. Please complete the sections
below. For bug fixes, link the issue. For new-language ports, reference the
per-language checklist from CLAUDE_NOTES.md.
-->

## Summary

<!-- One or two sentences on what this PR changes and why. -->

## Type of change

- [ ] Bug fix (conversion result, error handling, or parity between languages)
- [ ] New language implementation
- [ ] Enhancement to an existing implementation
- [ ] Documentation / metadata only
- [ ] Test data or tooling

## Affected languages

<!-- List the language directories touched. "n/a" for docs-only changes. -->

## Tests

- [ ] `make test-all` passes locally (or the relevant per-language subset)
- [ ] No regression in error-case test suite
- [ ] No regression in CSV conversion test (`prov_unpack_to_pack` 2,110,813 and `numbered_pack` 899,365 entries)
- [ ] No regression in helper-function test suite
- [ ] New tests added (if behavior changed)

## For new-language PRs, confirm

- [ ] Full per-language checklist from `CLAUDE_NOTES.md` is complete
- [ ] Entry added to top-level `README.md` implementations table
- [ ] Entry added to `CHANGELOG.md`

## Related issues

<!-- Fixes #123, Refs #456, etc. -->

## Notes for the reviewer

<!-- Anything surprising, spec-ambiguous, or worth calling out. -->
