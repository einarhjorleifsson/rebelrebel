# Points, lines, areas, segments and text

A layer of a map, one mark for each row of the data. `rr_point()` draws
a circle at each row's position (pings, stations, catches); `rr_line()`
draws rows that are lines (tows, transects, cables); `rr_segment()`
draws a line from each row's start to its end (a tow from shooting to
hauling); `rr_polygon()` draws areas, filled and outlined; `rr_text()`
writes a column's values at the rows' positions (place names, station
ids). Each is piped onto a map,
`rr_map(data) |> rr_point(colour = speed)`, or given the data first,
`data |> rr_point()`, which starts a map. The data stay in DuckDB and
reach the page as vector tiles, so a map of millions of rows stays fast
and its colours change in the page.

## Usage

``` r
rr_point(
  map,
  data = NULL,
  lon = NULL,
  lat = NULL,
  colour = NULL,
  size = NULL,
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

rr_line(
  map,
  data = NULL,
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

rr_segment(
  map,
  data = NULL,
  lon = NULL,
  lat = NULL,
  lon_end,
  lat_end,
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

rr_polygon(
  map,
  data = NULL,
  colour = NULL,
  fill = NULL,
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

rr_text(
  map,
  data = NULL,
  lon = NULL,
  lat = NULL,
  colour = NULL,
  size = NULL,
  opacity = NULL,
  label,
  palette = NULL,
  limits = NULL,
  stretch = NULL,
  legend = FALSE,
  popup = FALSE,
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
  for `rr_line()` and `rr_polygon()`; a table's shapes in a column
  `geometry` as DuckDB's GEOMETRY or WKB).

- lon, lat:

  The position's columns, by default `lon` and `lat`; for `rr_segment()`
  its start.

- colour:

  The marks' colour (an area's outline): a colour (`"#c0392b"`), or a
  column written bare (`colour = speed`), coloured by `palette`; several
  columns, `c(behaviour, speed)`, are a choice in the page, the first
  shown. A column's colours by default: Okabe-Ito for up to eight
  values, Paired for up to twelve, Glasbey's 32 beyond, the rest grey as
  "other"; viridis for an ordered factor and for numbers, dates and
  times. `color` is the same.

- size:

  A point's diameter, a text's size, in pixels.

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

- linewidth:

  A line's width (an area's outline), in pixels.

- lon_end, lat_end:

  For `rr_segment()`, the columns of its end.

- fill:

  For `rr_polygon()`, the areas' colour, as `colour` is for the other
  marks.

- label:

  For `rr_text()`, the column whose values are written, bare.

## Value

An
[`rr_map()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_map.md)
with the layer added.

## See also

[`rr_map()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_map.md);
[`rr_layer()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_layer.md)
for a layer in MapLibre's own terms.

## Examples

``` r
# \donttest{
m <- rr_example("dansk") |>
  rr_point(colour = c(behaviour, speed), popup = c(vessel_id, time_stamp, speed), minzoom = 9)
format(m)
#> [1] "<rr_map> Map"              "  layers: points (vector)"
rr_map(rr_example("trawlsurveytows")) |> rr_line(name = "Tows") |> format()
#> [1] "<rr_map> Map"            "  layers: tows (vector)"
rr_example("dansk_harbours") |> rr_polygon(fill = "#d9822b", popup = SI_HARB) |> format()
#> [1] "<rr_map> Map"             "  layers: areas (vector)"
# }
```
