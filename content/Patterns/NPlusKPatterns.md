---
tags: patterns
---

# NPlusKPatterns

The NPlusKPatterns extension adds a new syntax to patterns. It allows patterns
of the form `(n+k)` to be matched, where `n` is the identfier to bind to, `k` is
any positive integer literal, and the plus sign represents mathematical addition
(I know, shocking). The compiler just desugars the form into a subtraction.

NPlusKPatterns are largely of historical interest now. They were originally part
of [Haskell98](Haskell98), but subsequently removed from
[Haskell2010](Haskell2010), and this extension turns them back on again. Anyone
writing new code is directed to [ViewPatterns](ViewPatterns), instead.
