# Plan: normalize `R/recipes.R`

Deferred from the 0.7.1 review. Everything here is a follow-up to the fixes
already shipped in 0.7.1 — read that NEWS entry first, since two of the bugs
it fixes are the motivation for this work.

## Why

`R/recipes.R` has five exported chart builders. Each one independently
reimplements the same pipeline:

1. validate `data` is a data frame
2. `enquo()` the positional aesthetics
3. `.detect_aesthetic_type()` on the color/fill quosure
4. `.warn_palette_ignored()`
5. `.default_palette()`
6. a three-way branch — `missing` / `static_color` / `variable_mapping` —
   each arm rebuilding the whole `ggplot() + geom_*()` call
7. inside the third arm, a second branch attaching `scale_*_ekio_c()` or
   `scale_*_ekio_d()`
8. optional `add_zero` rule
9. a `scale_y_continuous(expand = ...)`
10. `labs()` + `theme_ekio()`

Steps 6 and 7 are ~150 of the file's ~430 lines, and `ekio_scatterplot()`
doubles its copy into six arms because of the `size` mapping.

Both bugs fixed in 0.7.1 were caused by this duplication, not by any single
recipe being wrong:

- `ekio_barplot()` hardcoded `scale_fill_ekio_d()` in its step-7 slot while
  the other four branched. A continuous `fill` errored.
- `palette <- "contrast"` was pasted into all five step-5 slots, so the
  continuous path errored in *every* recipe — nobody noticed, because the
  bug was uniform.

The next divergence between the five copies will be just as quiet. That is
the thing to fix.

## Target shape

Extract the branch tree into one internal builder. Sketch:

```r
# Builds the ggplot + geom for one color/fill-aware recipe and attaches the
# scale the detected aesthetic type calls for. `base_aes` is the mapping
# every arm shares (x, y, size, ...); `aes_name` is "colour" or "fill".
.recipe_layer <- function(data,
                          base_aes,
                          aes_name,
                          aes_quo,
                          aes_type,
                          geom,
                          palette,
                          static_args = list(),
                          mapped_args = list()) {
  ...
}
```

- `missing` → `geom(<aes_name> = .ekio("blue", 700), !!!static_args)`
- `static_color` → `geom(<aes_name> = aes_type$value, !!!static_args)`
- `variable_mapping` → mapping gains `aes_name`, geom gets `mapped_args`,
  and the matching `scale_*_ekio_c/d(palette)` is appended

Each recipe then reduces to: validate, enquo, detect, call `.recipe_layer()`,
apply its own `add_zero` / expansion / `coord_flip()` rules, `labs()`,
`theme_ekio()`. Roughly 25 lines each instead of 60–90.

Note the `missing` and `static_color` arms differ only in which color they
pass, and the `mapped_args` exist only because the histogram's mapped arm
adds `position = "identity", alpha = 0.7` and the area plot's adds
`position`. Resolve those explicitly rather than letting them stay implicit.

## Make the recipes opinionated

The 0.7.1 fix made all five recipes support continuous mappings *uniformly*,
which was the right call for a patch release — it removed an inconsistency.
But uniform is not the same as correct, and a continuous palette rarely makes
sense on some of these charts. This refactor should decide per chart type
rather than mechanically supporting everything

- `ekio_histogram()`, `ekio_barplot()`, `ekio_lineplot()`, `ekio_areaplot()` - rarely does it make sense to map
a continuous variable to these plots. These should render a `cli_abort()` that names the
  real fix: bin the variable or wrap it in `factor()`.
- `ekio_scatterplot`, is a bit of an edge case. This should render a `cli_warn()`
that suggests a discrete variable instead of continuous one

Capture it as a per-recipe capability passed into
`.recipe_layer()` (e.g. `continuous = c("allow", "reject")`) rather than as
ad-hoc `if` statements, so the policy is visible in one place.

## One open design questions to settle in the same pass

**1. `.detect_aesthetic_type()`'s error swallowing.** Lines 24–33 wrap the
mapped-variable evaluation in `tryCatch(..., error = function(e)
is_continuous <<- FALSE)`. A typo'd column name is silently treated as
discrete, and the user gets ggplot2's later "object not found" instead of a
recipe-level message. Consider letting the error through, or catching it to
produce a better message.

## Constraints

- Purely internal. `.recipe_layer()`, `.detect_aesthetic_type()`, and
  `.default_palette()` are unexported; no NAMESPACE change should be needed
  except for the opinionated-behavior decision above.
- Follow the project convention in `CLAUDE.md`: `ggplot2::` prefixes (no
  blanket `@import`), no hardcoded hex — use `.ekio(scale, shade)`.
- `...` currently forwards to the geom in every recipe. Preserve that; it is
  documented in each `@param ...`.

## Verification

- `tests/testthat/test-recipes.R` already covers the three aesthetic types
  per recipe, the continuous/discrete scale selection, the label arguments,
  and non-data-frame rejection. It should pass **unchanged** through the
  mechanical extraction — treat any required test edit as a signal that
  behavior moved, and check it was intentional.
- Add cases for whatever the opinionated policy rejects.
- `devtools::check()` must stay at 0 errors / 0 warnings / 0 notes.
- Compare `ggplot2::ggplot_build()` output before and after on one plot per
  recipe per aesthetic type; layer data should be identical for the
  mechanical part of the change.
