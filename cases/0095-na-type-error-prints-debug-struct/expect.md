`fn(): char { NA }` is rejected (correct) but the message reads
`Found: NA(HelpData { offset: 61, end: 64, file_name: "main.ty" })`: the `Debug` form of the
`Lang::NA` node leaks into the user-facing text. Expected `Found: NA`.
Also: a `type Factor <- Foreign<Any>` alias is shown as `Foreign` in errors, not `Factor`.
