Identifiers cannot contain `.`, so `@extern` names use `__` for it (`weighted__mean`).
Calls to a plain function are rewritten (`weighted.mean`), but a namespaced extern
`utils::read__csv` is emitted as `utils$utils::read__csv(...)`: the `__` is kept, so the call
fails at run time. Expected `utils::read.csv(...)`.

Code: transpiling of `Lang::Extern` / namespaced function calls.
