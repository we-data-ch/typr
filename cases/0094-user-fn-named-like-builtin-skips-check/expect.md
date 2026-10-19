`let norm <- fn(x: [3, num]): num {...}; norm([1.0, 2.0]);` type-checks. Renaming the function
(`rescale`) gives the expected error. A user definition sharing a name with an R builtin
(`norm`, from base) seems to fall back to `any` on call resolution, silently dropping the check.
