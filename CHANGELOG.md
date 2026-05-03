# ChangeLog for wai-app-file-cgi

## 3.2.0

- `FileRoute`s now allow routing a requested path directly to a file
  instead of just path prefixes to directories. For this, paths that
  don't have a trailing path separator are interpreted as paths to
  a file.

  This change may **break** existing `FileRoute`s if they lack the
  trailing path separator. Before upgrading, make sure that all
  `FileRoute`s (that work correctly with `wai-app-file-cgi < 3.2`)
  have trailing path separators in the `fileSrc` and `fileDst`
  record fields.

  See also [#23](https://github.com/kazu-yamamoto/wai-app-file-cgi/pull/23).
