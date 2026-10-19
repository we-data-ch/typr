# 0099 — reassigning a list field

`let d <- :{ a: 8, b: false};` then `d$b <- true;` was rejected with
"Unknown element `<- true;`": `assign` only accepted a bare variable on the
left of `<-`/`=`. The target may now be a `$` chain (`d$b`, `d$c$x`), parsed
as nested `Op::Dollar`; the type checker requires the new value to be a
subtype of the field's (widened) type, and an unknown field is reported.
