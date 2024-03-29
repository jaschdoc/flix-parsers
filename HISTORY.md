### v0.1.0

- Initial release.

### v0.1.1

- Fixes compatibility in test suite with Flix 0.34.0 (version 0.1.0 was compatible with a newer version of Flix which is not yet released).

### v0.1.2

*This release is mostly just to fix compatibility with the Flix community build. See [https://github.com/flix/flix/pull/5235](https://github.com/flix/flix/pull/5235).*

- All functions do not carry the last parameter, and instead return a lambda to make the API more readable.
- Includes basic parser combinators so one should be able to write a few useful grammars with the library.

### v0.1.3

*This release is just to fix compatibility with an upcoming version of the Flix packager. See [https://github.com/flix/flix/issues/5134](https://github.com/flix/flix/issues/5134).*

### v0.1.4

*This release is mostly just to fix compatibility with the latest Flix compiler.*

- Removes use of `new` keyword.
- Removes use of `foreach` loop.
- Updated namespaces to use new `mod` keyword and qualified tags.

### v0.1.5

*This is release fixes compatibility with the latest version of Flix.*

- Removes `namespace` keyword (uses `mod` instead).
- Uses `.` as namespace separator instead of `/`.
