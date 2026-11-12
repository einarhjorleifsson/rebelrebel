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
  whole = NULL,
  thin = TRUE,
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
  whole = NULL,
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
  whole = NULL,
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
  whole = NULL,
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
  whole = NULL,
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

  A point's diameter, a text's size, in pixels. For points, by default,
  a size that follows the zoom: the least, 2 pixels, until the zoom at
  which most points lie that far apart, then doubling with each zoom in,
  as a size on the ground does, up to the most, 8; `size = c(1, 12)`
  gives other bounds, one number a size that stays.

- opacity:

  The marks' opacity, 0-1.

- whole:

  How the rows reach the page: `TRUE`, whole, as one GeoJSON file read
  once (no further fetching as the reader zooms and pans, nothing cut at
  a tile's edge, one file in a bundle); `FALSE`, as vector tiles cut by
  DuckDB as the page asks for them, which keep a page light however many
  rows there are. By default (`NULL`) whole when its GeoJSON is at most
  5 MB (some 30,000 points; a table of more than 50,000 rows is not
  tried), else as tiles: measured on layers of real data, the two are
  equal in a browser up to about 5 MB, and a whole layer needs nothing
  more of R once read, so a served map pans and zooms while R is busy.
  Above it a whole layer is slower and heavier (about 50 MB of a
  browser's memory for each MB of GeoJSON).

- thin:

  For points: in each tile, one point for each screen pixel, `TRUE`
  (each colour, each value a `choose` list picks and each group of a
  time panel keeping its own), so a tile of millions of rows reaches the
  page as what can be seen of them; a number of pixels for coarser
  cells; `FALSE` keeps every point. At the layer's deepest zoom
  (`maxzoom`, 14 by default) and beyond, only points that would be drawn
  on one spot are dropped.

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

  The columns, written bare, whose values the reader picks to show of
  this layer alone, each under the layer's name: text, a factor, a
  logical or a whole number (an id, a code) as a list (chips for up to
  12 values, else a list to show one at a time), a fraction, a date or a
  time as a range with two handles, unless the data's dictionary
  declares otherwise (as for
  [`rr_map()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_map.md));
  ‹ and › step through either. Several are linked: each offers only the
  values the others leave. For choices that drive every layer with the
  column, give `choose` to
  [`rr_map()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_map.md).

- name, info:

  The layer's name in the sidebar and the text behind its ⓘ.

- section:

  The sidebar's box the layer sits in, by its title.

- visible:

  Whether the layer is on when the page opens.

- minzoom, maxzoom:

  The zoom the layer is drawn from, and its tiles' deepest (14 by
  default), past which the map draws that zoom's tiles larger.

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
  rr_point(colour = c(behaviour, speed), popup = c(vessel_id, time_stamp, speed))
format(m)
#> [1] "<rr_map> Map"              "  layers: points (vector)"
rr_map(rr_example("trawlsurveytows")) |> rr_line(name = "Tows") |> format()
#> [1] "<rr_map> Map"             "  layers: tows (geojson)"
rr_example("dansk_harbours") |> rr_polygon(fill = "#d9822b", popup = SI_HARB) |> format()
#> [1] "<rr_map> Map"              "  layers: areas (geojson)"
# }
```
