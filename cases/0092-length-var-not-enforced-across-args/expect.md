`rel(x: [#N, num], y: [#N, num])` called with `[1.0, 2.0, 3.0]` and `[1.0, 2.0]` is accepted:
`#N` should unify to a single length across the signature, so the second argument (length 2)
must be a type error against `N = 3`.

Not to be promised on the landing page until fixed (see `typr.github.io` "What R lets through").
