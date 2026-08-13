# Releasing hanalyze to Hackage

## Steps

1. Bump `version:` in `hanalyze/hanalyze.cabal` (and sibling packages) and move the `[Unreleased]` notes in
   `CHANGELOG.md` under the new version.
2. **Bump the pinned URLs in `README.md`** (see below).
3. Commit → push `master` → tag `vX.Y.Z.W` and push the tag
   (before uploading, so Hackage renders the README images immediately).
4. `cabal sdist`, then upload as candidate first:

   ```bash
   cabal upload --token "$(cat ~/.hackage-token)" dist-newstyle/sdist/hanalyze-<ver>.tar.gz
   # review the candidate page (haddock, README figures, metadata), then:
   cabal upload --publish --token "$(cat ~/.hackage-token)" dist-newstyle/sdist/hanalyze-<ver>.tar.gz
   ```

## Per-release: bump the pinned URLs in README.md

README figures and doc links are absolute URLs pinned to the release tag
(raw.githubusercontent / github.com blob), because Hackage cannot resolve
relative paths. On every release, replace the old tag with the new one:

```bash
sed -i 's|/hanalyze/v0\.2\.0\.0/|/hanalyze/vX.Y.Z.W/|g' README.md
```

then verify every URL still resolves (must print nothing but "done"):

```bash
grep -o 'https://[^")<> ]*' README.md | sed 's/#.*$//;s/[.,]$//' | sort -u | \
  while read u; do [ "$(curl -s -o /dev/null -w '%{http_code}' "$u")" != 200 ] && echo "FAIL $u"; done; echo done
```

## Caveats

- The repo is now multi-package (hanalyze + hanalyze-{core,frame,bayes,
  models,design,viz,plot,cli,demos}); bump `version:` in each package to be
  uploaded (which packages go to Hackage is decided per release).
- `Hanalyze.Plot` lives in the separate `hanalyze-plot` package (the old
  `plot-integration` flag was removed with the multi-package split). It
  depends on the sibling hgg packages (`hgg-core`/`hgg-svg`/`hgg-3d`/
  `hgg-custom`, bounds `>= 0.2 && < 0.3`); until hgg 0.2 is on
  Hackage, point a checkout via `cabal.project.plot.local` and build with
  `cabal build --project-file=cabal.project.plot hanalyze-plot` before
  releasing to catch hgg API drift.
- Note: the hanalyze-0.2.0.0 tarball as published still contains the
  pre-rename `Hgg.Plot.*` imports in the flag-gated Hanalyze.Plot modules,
  so `+plot-integration` does not compile against hgg 0.1.0.0 from Hackage
  (fixed in-repo 2026-07-19; ships with the next release).
