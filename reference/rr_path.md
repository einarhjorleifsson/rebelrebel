# Paths: rows joined in time, a line per group

A layer of lines made from rows joined in order of `time`, one line for
each value of `group` (a float, an animal, a ship, or each of its legs
with `group = c(id, leg)`), as ggplot2's `geom_path()` joins rows in
their order: tracks of drifters, tagged animals, gliders and ships. A
silence longer than `max_gap_s` seconds breaks a line. Coloured by a
column, a line is cut where the column's value changes, each run in its
value's colour, and stays joined across the change.

## Usage

``` r
rr_path(
  map,
  data = NULL,
  lon = NULL,
  lat = NULL,
  group = NULL,
  time,
  max_gap_s = Inf,
  colour = NULL,
  linewidth = NULL,
  opacity = NULL,
  palette = NULL,
  limits = NULL,
  stretch = NULL,
  legend = TRUE,
  popup = TRUE,
  labels = NULL,
  codes = NULL,
  hover = NULL,
  choose = NULL,
  name = NULL,
  info = NULL,
  section = "Layers",
  visible = TRUE,
  minzoom = NULL,
  maxzoom = NULL,
  ...
)
```

## Arguments

- map:

  An
  [`rr_map()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_map.md),
  or the data, which start a map.

- data:

  The rows, if not the map's: a parquet file, folder or glob, a lazy
  DuckDB table, a data frame or an sf object (points, or lines and areas
  for
  [`rr_line()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_point.md)
  and
  [`rr_polygon()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_point.md);
  a table's shapes in a column `geometry` as DuckDB's GEOMETRY or WKB).

- lon, lat:

  The position's columns, by default `lon` and `lat`; for
  [`rr_segment()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_point.md)
  its start.

- group:

  The columns, written bare, whose values each have a line: one, or
  several, `c(id, leg)`. By default none, one line through all the rows
  (and the map's `choose` columns, so a choice shows whole lines).

- time:

  The column that orders the rows, written bare: a time, or any column
  that orders (a distance along a transect, a sequence number). A lazy
  table has no order of its own, so it is needed.

- max_gap_s:

  A silence between two rows of a line longer than this many seconds
  breaks it, as `3600` for an hour; `Inf`, the default, for none, so no
  gap is hidden.

- colour:

  The lines' colour: a colour, or one column written bare, whose runs of
  one value cut and colour the line (Okabe-Ito and the other defaults of
  [`rr_point()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_point.md)).

- linewidth:

  A line's width (an area's outline), in pixels.

- opacity:

  The marks' opacity, 0-1.

- palette:

  The colours for a column, in place of the default: a palette's name
  (from
  [`rr_palettes()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_palettes.md),
  [`palette.pals()`](https://rdrr.io/r/grDevices/palette.html) or
  [`hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html), written
  loosely: `"set1"` is Set 1), colours, colours named by value
  (`c(Fishing = "#c0392b")`), or a function of n
  ([`viridisLite::mako`](https://sjmgarnier.github.io/viridisLite/reference/viridis.html)).

- limits, stretch:

  For a numeric column: the values the palette's ends stand at (by
  default its 1st and 99th percentile), and how the colours spread
  between them, `"linear"`, `"log"` or `"equal"` (equal shares of the
  values).

- legend:

  Whether the layer has a legend.

- popup:

  The columns a click shows, written bare; `TRUE`, every column;
  `FALSE`, no popup. A click shows the row as a table, read from the
  server (a bundle carries the rows): a column's name, its unit beside
  it and its description behind it, where the data's dictionary or
  `labels` give them.

- labels:

  The columns' names as the popup shows them, in place of the data's:
  `c(speed = "Speed (knots)")`. By default a data frame's column
  attributes (`label`, `units`) or a parquet file's own dictionary (an
  entry of data-dict.tidyverse.org's format in its metadata, as
  [`rr_example()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_example.md)'s
  files hold), else the column's name.

- codes:

  The labels of a coded column's values, shown beside each code: a named
  list by column, each a named vector (`c("6" = "bottom trawl")`) or a
  table of code and label; by default the dictionary's, where it has
  them.

- hover:

  A column whose value is shown beside the pointer.

- choose:

  For the map's first layer from data: the columns whose values the
  reader picks, as for
  [`rr_map()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_map.md).

- name, info:

  The layer's name in the sidebar and the text behind its ⓘ.

- section:

  The sidebar's box the layer sits in, by its title.

- visible:

  Whether the layer is on when the page opens.

- minzoom, maxzoom:

  The zooms the layer is drawn at.

- ...:

  Not used; `color` is taken for `colour`.

## Value

An
[`rr_map()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_map.md)
with the layer added.

## Examples

``` r
# \donttest{
m <- rr_map(rr_example("dansk")) |>
  rr_path(group = vessel_id, time = time_stamp, colour = behaviour, max_gap_s = 3600)
format(m)
#> [1] "<rr_map> Map"             "  layers: paths (vector)"
# }
```
