---
tags: literals
---

# OverloadedStrings

OverloadedStrings allows string literals to represent types other than `[Char]`.
Instead, string literals have the type `IsString a => a` and any type can be
equipped with an instance of `IsString`.

OverloadedStrings is similar to both Haskell2010’s treatment of numeric literals
and the extension [OverloadedLists](OverloadedLists).
