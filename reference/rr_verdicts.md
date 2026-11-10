# Verdicts given on a served map

A map served with `rr_serve(verdicts = )` takes the reader's verdict on
the features of a layer whose popup has a `verdict`
([`rr_layer()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_layer.md)):
its questions in a feature's popup, a button for each answer, a box for
the name an answer points at (`other`, with the names in view offered),
and the feature's note where the verdict asks for one. Each answer is a
row added to a `.csv` file, never a row changed: when, the layer, the
feature (its value of the verdict's `field`), the question, the answer,
the other feature's name, the note, the feature's title, its place (the
feature's own for a point, else where it was clicked) and the zoom. The
note is a question of its own, "note", its text the answer. A later
answer to a question replaces the earlier, and an empty one withdraws
it. The page draws a mark on each feature judged: the first question's
answer its fill, the second's its ring. A review written down where it
is made, and read back as data: what a map shows can be checked by hand,
and the checks counted. A file of 2026.11.09, before the questions,
reads as the question "verdict" with no other names.

## Usage

``` r
rr_verdicts(path, all = FALSE)
```

## Arguments

- path:

  The verdicts' file (`.csv`).

- all:

  `FALSE`, each feature's current answer to each question (the last,
  none where it was withdrawn; a note where it has a text); `TRUE`,
  every answer given, in the order given.

## Value

A data frame: `time` (UTC, as text), `layer`, `id`, `question`, `value`,
`other`, `text`, `title`, `lon`, `lat`, `zoom`; no rows when there is no
file.

## Examples

``` r
# a map whose harbours take a verdict each:
# popup = list(click = "harbours-fill", title = "SI_HARB",
#              verdict = list(field = "SI_HARB", text = TRUE,
#                             options = list(list(value = "right", label = "right"),
#                                            list(value = "wrong", label = "wrong"))))
# rr_serve(m, verdicts = "harbour_verdicts.csv")
f <- tempfile(fileext = ".csv")
rr_verdicts(f) # none yet
#>  [1] time     layer    id       question value    other    text     title   
#>  [9] lon      lat      zoom    
#> <0 rows> (or 0-length row.names)
```
