# Releasing hanalyze to Hackage

## Steps

1. Bump `version:` in `hanalyze.cabal` and move the `[Unreleased]` notes in
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

- The `plot-integration` flag (Hanalyze.Plot) depends on the sibling hgg
  packages (`hgg-core`/`hgg-svg`/`hgg-3d`/`hgg-custom ^>= 0.1`); it is
  manual + default-off so the standalone build stays plot-free. Build it
  locally before releasing to catch hgg API drift
  (`cabal build hanalyze -f plot-integration`).
- Note: the hanalyze-0.2.0.0 tarball as published still contains the
  pre-rename `Hgg.Plot.*` imports in the flag-gated Hanalyze.Plot modules,
  so `+plot-integration` does not compile against hgg 0.1.0.0 from Hackage
  (fixed in-repo 2026-07-19; ships with the next release).
