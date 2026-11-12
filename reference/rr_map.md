# A map: layers in groups, texts, base maps and palettes

The map is a page with a sidebar of groups that fold, each layer with
its info and, when ticked, its controls and legend under it; base maps
to choose from; the whole view in the page's address, so a link gives
the same view; and on a phone the sidebar behind a menu button.
[`rr_serve()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_serve.md)
serves it.

## Usage

``` r
rr_map(
  ...,
  title = "Map",
  view = list(center = c(0, 0), zoom = 2),
  texts = list(),
  languages = "en",
  language = languages[1],
  locales = NULL,
  about = NULL,
  choose = NULL,
  sections = NULL,
  palette = NULL,
  palettes = rr_palettes(),
  basemaps = rr_basemaps()
)
```

## Arguments

- ...:

  [`rr_section()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_section.md)s;
  or, alone, the data a map starts from (a parquet file, folder or glob,
  a lazy DuckDB table, a data frame or an sf object), which the layer
  functions
  ([`rr_point()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_point.md)
  and the others) draw.

- title:

  The page's title: a key of `texts`, or the text itself.

- view:

  The first view: `center` (longitude, latitude) and `zoom`.

- texts:

  The texts of the layers and groups, and of the page in other
  languages: a named list whose elements are a text, or a list of texts
  by language (`list(en = "Ports", is = "Hafnir")`). The page's own
  texts are English; give them in another language under the same keys,
  a base map's label under `b_<id>`.

- languages:

  The languages of the page, in the order of its language buttons (none
  for one language).

- language:

  The language the page opens in.

- locales:

  A list by language of the locales its numbers are written in, the
  first a browser has (`list(is = c("is-IS", "de-DE"))`, German standing
  in where a browser lacks Icelandic).

- about:

  Keys of `texts` for the paragraphs of the About group (none, no
  group).

- choose:

  Fields whose values the page offers to show, over every layer that has
  the field (each a control above the groups, with buttons that step
  through the values): a list with `fields`, each a `field`, a `label`,
  `multi` (`TRUE`: each value a chip ticked on and off, so several show
  at once; else a list, "all" first, to show one at a time), its
  `options` (each a `value` for the control and the address, a `label`,
  `match`, the value the layers' features carry, `NULL` for none, and
  optionally `bbox`, the extent the view is fitted to, and `base`, the
  id of a base map shown while this value is the one chosen: a chart's
  scale chosen with that chart beneath) and `layers` (the ids it
  filters); or, for a range, `range` (`"number"`, `"date"` or `"time"`),
  `closed` and its `bins` (each a `lo` and a `hi` as the features carry
  them, `from` and `to` as written, `n` and optionally `bbox`); and
  optionally `extents`, each a `key` (each field's value, or a range's
  bin from 0, joined by the character 31), `n` and a `bbox`, by which
  the fields are linked and the view fitted. `rr_map()` makes it from
  the columns its `choose` names. A choice goes into the address under
  the field's name, several values separated by commas, a range as
  `from..to`, and nothing is fetched again. For a map started from data,
  its columns written bare, `choose = c(id, year)`, made from the data
  and linked: text, a factor, a logical or a whole number (an id, a
  code, a year as an integer) is a list of its values (chips for up to
  12); a fraction, a date or a time is a range with two handles, over
  each of its values where it has at most 64, else over bins between
  [`pretty()`](https://rdrr.io/r/base/pretty.html) breaks. Where the
  data's dictionary (data-dict.tidyverse.org's, in the parquet file's
  metadata) gives a column's type, it decides: `number(id)`, `enum`,
  `string` or `boolean` a list, `number`, `number(quantity)`, `date` or
  `datetime` a range. A path is split by a range's bins, so a choice
  shows whole runs.

- sections:

  For a map started from data, the sidebar's boxes the layers name
  (their `section`), each a `title` and whether it is `open`:
  `list(Reference = list(open = FALSE))`.

- palette:

  The grid's palette when the page opens: an `id` of `palettes`.

- palettes, basemaps:

  The palettes and base maps to choose from:
  [`rr_palettes()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_palettes.md),
  [`rr_basemaps()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_palettes.md).
  The grid offers the palettes for its use (`use` holding `"grid"`, or
  no `use`); a dem layer's colour offers its own list (`colour$palettes`
  of
  [`rr_layer()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_layer.md)),
  by default the seven looks for a depth. A palette given again under an
  id replaces the earlier in its place, so `c(rr_palettes(), mine)` may
  redefine one.

## Value

An `rr_map`: `config` (what the page is sent) and `layers`.

## Details

Given data first, `rr_map(data)` starts a map that the layer functions
add to
([`rr_point()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_point.md)
and the others), the data the layers' unless a layer brings its own.

## Examples

``` r
harbours <- rr_layer(
  "harbours", "vector", label = "Harbours",
  tiles = function(z, x, y) raw(0),
  style = list(list(id = "harbours-fill", type = "fill", "source-layer" = "features",
                    paint = list("fill-color" = "#d9822b")))
)
m <- rr_map(rr_section("ref", "Reference", list(harbours)), title = "Example",
            view = list(center = c(12.5, 56), zoom = 7))
names(m$config)
#> [1] "title"     "view"      "languages" "language"  "groups"    "layers"   
#> [7] "basemaps"  "palettes"  "texts"    
```
