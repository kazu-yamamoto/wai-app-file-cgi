# ChangeLog for wai-app-file-cgi

## 3.2.2

- Security: request paths containing `..` segments are rejected
  with 400 in `fileApp` and `cgiApp`. Previously, files outside
  `fileDst` could be served.
- Security: languages in Accept-Language are used as file suffixes
  only if they consist of alphanumerics and `-`. Languages with
  `q=0` and `*` are ignored.
- `cgiApp`:
    - Fixing a deadlock when a CGI script writes a large output
      while reading a large request body.
    - Any methods are passed to CGI scripts. 405 is not returned
      anymore.
    - Request headers are passed as `HTTP_*` except `Proxy`
      (httpoxy), `Authorization` and `Proxy-Authorization`.
      `SCRIPT_FILENAME`, `REMOTE_PORT` and `HTTPS` are also passed.
    - Responses with `Location:` but without `Content-Type:` are
      returned as 302 instead of 500.
    - The status code is not duplicated in the reason phrase.
    - The index CGI is used if the program name is empty
      (e.g. `/cgi-bin`).
- `fileApp`:
    - MIME types are determined from the file name only, trying the
      longest extension first, case-insensitively.
    - The query string is kept when redirecting to a directory.
- `revProxyApp`:
    - 502 is returned if the upstream is not available.
    - Hop-by-hop headers and headers listed in `Connection:` are
      removed.
    - `X-Forwarded-For:` and `Via:` are appended to existing ones.
- `hostPort` handles IPv6 literals such as `[::1]:8080` and uses 443
  as the default port for secure connections.

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
