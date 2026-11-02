# ChangeLog for wai-app-file-cgi

## Unreleased

- **Breaking**: `rawPathInfo` is percent-decoded in `fileApp` and
  `cgiApp`, including `PATH_INFO` and `SCRIPT_NAME`. Callers that
  decode it beforehand (e.g. mighttpd2 up to now) must pass the
  original request; otherwise paths containing `%` are decoded
  twice.
- **Breaking**: paths that do not match the source of a route are
  rejected with 404 in all applications. A source with a trailing
  slash also matches the path without it.
- **Breaking**: paths containing a segment starting with `.` (e.g.
  `.git`), NUL, or encoded `..` are rejected with 400 in `fileApp`
  and `cgiApp`. `/.well-known/` is allowed.

## 3.2.3

- Security: request headers containing `_` are not passed to CGI
  since they would collide with ones containing `-`
  (e.g. `X_Forwarded_For` vs `X-Forwarded-For`).
- Paths and CGI variables are converted with the file system
  encoding. Non-ASCII file names and header values work now.
- `cgiApp`:
    - CGI processes are reaped by this library. The program does
      not have to ignore SIGCHLD anymore. Ignoring it is still
      possible but a PID may be reused before it is terminated.
    - 404 is returned if the CGI program does not exist.
- `fileApp`:
    - `Vary: Accept-Language` is added to HTML responses.
    - `Allow:` is added to 405 responses.
- `redirectApp` keeps the query string.
- `revProxyApp` appends `Via:` to requests to the upstream.
- Dropping support for conduit < 1.3 and removing unused
  dependencies.
- The test suite works from the tarball. CGI scripts for tests
  are made executable since `cabal sdist` drops the bit.

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
