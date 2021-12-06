---
tags: patterns
---

# ViewPatterns

The ViewPatterns extension adds a new syntax to existing patterns. Instead of
writing a pattern that the argument will be matched against, you write two
things in the form `(e -> p)`. Here `e` is an arbitrary expression and `p` is a
pattern. Where without a view pattern the corresponding argument would be
matched against a pattern directly, instead the expression will be evaluated
with the pattern’s argument as its arguemnt, and its result will then be matched
against the given pattern.

One interesting use is to replace [NPlusKPatterns](NPlusKPatterns) by using the `subtract`
function as a view. 
