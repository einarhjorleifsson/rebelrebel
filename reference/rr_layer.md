# A layer of a map

One layer of an
[`rr_map()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_map.md):
what it shows, how it is drawn, and where its data come from. Five
kinds:

## Usage

``` r
rr_layer(
  id,
  kind = c("grid", "vector", "geojson", "dem", "contour"),
  label = id,
  info = NULL,
  visible = TRUE,
  tiles = NULL,
  meta = NULL,
  data = NULL,
  style = NULL,
  popup = NULL,
  legend = NULL,
  variants = NULL,
  count = NULL,
  missing = NULL,
  filters = NULL,
  metrics = NULL,
  source_layer = NULL,
  minzoom = NULL,
  maxzoom = NULL,
  opacity = NULL,
  encoding = NULL,
  tile_size = NULL,
  bounds = NULL,
  shade = NULL,
  colour = NULL,
  contours = NULL,
  state = NULL
)
```

## Arguments

- id:

  The layer's name, lower case: its checkbox, its source and its key in
  the page's address.

- kind:

  `"grid"`, `"vector"`, `"geojson"`, `"dem"` or `"contour"`.

- label, info:

  Its name in the sidebar and the text behind its ⓘ.

- visible:

  Whether it is on when the page opens; the address turns it off with
  `id=0` or on with `id=1`.

- tiles:

  The tile function. For a grid, `function(z, x, y, params)`, `params` a
  named list of the filters' values as text (absent when not given); for
  a vector layer, `function(z, x, y)`. It returns a Mapbox vector tile
  as raw bytes (DuckDB's `ST_AsMVT()`), or `raw(0)`. A dem layer's,
  `function(z, x, y)`, returns an image of elevations (PNG or WebP) as
  raw bytes, or `NULL` where there is none; a contour layer's the same.

- meta:

  For a grid, a list or a function returning one, read when the server
  starts: `options` (per filter with `meta = TRUE`, a data frame of
  `value` and, optionally, `name` and `code`), `ranges` (per range
  filter, its first and last value) and `v` (a version, put into the
  tile address so a rebuilt source is not hidden by the browser's
  cache).

- data:

  For a geojson layer: GeoJSON text, an sf object, or a function
  returning either, read when the server starts.

- style:

  A list of MapLibre layer specifications, without `source` (the layer's
  own).

- popup:

  For a vector or geojson layer, what a click shows: `click` (the style
  ids that answer), `title` (a field), `fields` (each a list of `field`,
  `label`, and optionally `values`, a value-to-text lookup whose text
  may hold `{field}` placeholders; `template`, a text with `{field}`
  placeholders shown in place of the value when there is one; and
  `format`, `"value"` or `"count"`), `maxWidth`. For a grid, `fields`
  only, the first in bold. Opt-in, a label on hover: `hover`, the style
  ids whose features show their `title` in a light label beside the
  pointer while it is over them (`TRUE` for those of `click`), and
  `hover_fields`, `TRUE` to show the `fields` there too. Over features
  of several layers, the topmost speaks, as for a click; a click hides
  the label until the pointer is over another feature. Opt-in, a verdict
  on each feature, kept by a server started with
  `rr_serve(verdicts = )`: `verdict`, a list of `field` (the field
  naming a feature: a verdict belongs to its value), `questions` (one to
  four, each an `id`, a `label`, two to six `options` and optionally
  `skip`), and `text` (`TRUE` for a note on the feature, in the reader's
  words). An option is a `value` (letters, digits, `_` or `-`), a
  `label` (a text or a key of the map's `texts`), optionally a `colour`
  for the feature's mark (the first green by default, the others red),
  and `other = TRUE` for an answer that names another feature: a box for
  its name, the names in view offered. `skip` names earlier questions
  and the answers that leave this one out (`list(name = "not")`). One
  question may be given as `options` alone. `store`, another layer's id,
  keeps the answers under that layer's and shows its answers (two layers
  of one kind of feature: a feature's answer the same whichever it is
  clicked on); the two must ask the same questions. The click's popup
  then holds the questions, a button for each answer; each answer is
  written to the server's file with the feature's place, and a mark
  drawn on the feature (the first question's colour its fill, the
  second's its ring). A later answer replaces the earlier;
  [`rr_verdicts()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_verdicts.md)
  reads them.

- legend:

  A list of swatches, each `swatch` (`fill` and `stroke`, or `dot`) and
  `label`.

- variants:

  For a vector, geojson or dem layer, styles to choose between in the
  page, such as the column the points are coloured by: `label` (the
  control's text) and `options`, each a `value`, a `label`, `paint` (by
  the layer's style ids, the paint properties it sets) and optionally
  `legend` (in place of the layer's). The first option is shown when the
  page opens; a choice goes into the address as `<id>_by`, and nothing
  is fetched again.

- count:

  The text beside its label: `text` (a key whose `{1}` is the count)
  and, for geojson, `where` (a field and the value of the features to
  count). A vector layer's count is its tile function's `count`
  attribute.

- missing:

  The text shown when the server has none of it.

- filters:

  For a grid: a list of controls, each a list with `id`, `label` and one
  of: `options` (a list of `value`, `label` and optionally `tip`) with
  an optional `default`; `meta = TRUE` (options from `meta`, with an
  optional `unnamed` text for an option with no name; several values,
  comma separated, may come from the address); `type = "range"` with
  `params` (two names, such as `c("y1", "y2")`, its bounds from `meta`);
  `type = "metric"` (the choice among `metrics`).

- metrics:

  For a grid: a list of the numeric fields it can be coloured by, each
  `field` and `label`.

- source_layer:

  For a grid, the tile layer its cells are in (`"cells"`).

- minzoom, maxzoom:

  The zooms its tiles are made for.

- opacity:

  For a grid, its opacity when the page opens, 0-100.

- encoding, tile_size, bounds:

  For a dem or contour layer: how its images hold the elevation
  (`"terrarium"` or `"mapbox"`), their side in pixels, and the extent
  that has tiles (west, south, east, north), so the page asks for no
  other. Taken, with `minzoom` and `maxzoom`, from the attributes of
  `tiles` where not given
  ([`rr_tiles_mbtiles()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_tiles_mbtiles.md)
  sets them); else `"terrarium"` and 512. The image format (`"png"` or
  `"webp"`) is the `format` attribute's.

- shade:

  For a dem layer with a `hillshade` style: the relief's controls in the
  page, and their values when it opens, a list of any of `method`
  (`"standard"`, `"basic"`, `"combined"`, `"igor"` or
  `"multidirectional"`, three lights 45 degrees apart), `direction` (the
  light's azimuth, 0-359), `altitude` (0-90), `exaggeration` (the
  intensity, 0-1), `opacity` (0-100: the alpha of the shading's colours,
  which has no opacity of its own), `shadow` and `highlight` (colours as
  `"#rrggbb"`); MapLibre's own defaults for those not given, but an
  intensity of 0.6 and a dark blue-grey shadow, `"#1b2a3a"`, as a
  bathymetry map in use settled on. `TRUE` takes every default. Each
  control applies as it moves, and goes into the page's address as
  `<id>_method`, `<id>_dir`, `<id>_alt`, `<id>_ex`, `<id>_op`,
  `<id>_shadow` and `<id>_light`. With `fit`, a function of the view's
  box as a colour's `fit` returning a list with `exaggeration` (0-1),
  the intensity can follow the view (`auto`, `TRUE` by default;
  `<id>_auto` in the address, which leaves out `<id>_ex` while it
  follows), until it is set by hand. Tiles from
  [`rr_tiles_dem()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_tiles_dem.md)
  carry a fit, taken when none is given.

- colour:

  For a dem layer with a `color-relief` style: its values in colour,
  with their controls in the page, a list of any of `of` (`"depth"`, the
  elevation's negative, or `"elevation"`; what is 0 or less is clear),
  `palettes` (the ids of the palettes its control offers, of the map's
  palettes) and `palette` (the one it opens on), `floor` and `cap` (the
  values the palette's ends stand at, 1-10,000, set on one slider of two
  handles), `scale` (`"log"`, `"linear"` or, with a fit, `"equal"`),
  `mode` (`"ramp"` or `"bands"`), `bands` (the bands' breaks, rising),
  `cuts` (the cuts offered, 0 for none) and `cut` (one of them: values
  under it are drawn as `cut_as`, `"clear"` or `"grey"`), `opacity`
  (0-100). Defaults: depth; for a depth the seven looks of
  [`rr_palettes()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_palettes.md)
  (quiet, seabed, deep, chart, grey, drained and haxby), for an
  elevation every palette of the map; the first of them; floor 10, cap
  2,000; in equal shares with a fit, else log; a ramp; bands at 10, 20,
  50, 100, 200, 500, 1,000 and 2,000; cuts 0, 10, 20 and 50, no cut,
  clear; 100. `TRUE` takes them all. Another palette, such as `"magma"`,
  is offered by naming it,
  `colour = list(palettes = c("quiet", "magma"))` (a `palette` not among
  `palettes` is put first in them), and one of your own by adding it to
  the map's,
  `rr_map(palettes = c(rr_palettes(), list(list(id = "mine", label = "mine", colors = <colours>))))`.
  Each control applies as it moves, and goes into the page's address as
  `<id>_pal`, `<id>_lo`, `<id>_hi`, `<id>_scale`, `<id>_mode`,
  `<id>_cut`, `<id>_cutas` and `<id>_cop`. With `fit`, a function of the
  view's `west`, `south`, `east` and `north` returning the floor and the
  cap for the values there (or `NULL` where there are none; or a list of
  `floor`, `cap` and `quantiles`, the values' quantiles rising, which
  also offers `scale` `"equal"`, each colour an equal share of the
  values in view between the floor and the cap), the page has a "Fit to
  view" button, answered by the server at `/fit/<id>?bbox=w,s,e,n`, and
  follows the view: it fits again each time the map settles, until a
  floor or cap is set by hand (`auto`, `TRUE` by default; `<id>_auto` in
  the address, where the floor and cap are left out while it follows).
  Tiles from
  [`rr_tiles_dem()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_tiles_dem.md)
  carry a fit, taken when none is given.

- contours:

  For a contour layer: `thresholds`, a named list by zoom of the minor
  and the major interval from that zoom on
  (`list("6" = c(500, 1000), "9" = c(50, 200))`; a major line has
  `level` 1), and `multiplier`, by which the elevations are multiplied
  before the lines are drawn (-1 for depths, 1 by default). With `fit`,
  a function as a colour's `fit` returning a list with the `quantiles`
  of the values in view, the lines can follow the view (`auto`, `TRUE`
  by default; `<id>_auto` in the address): about a dozen levels at round
  values near the view's quantiles, so the lines fall where the area is,
  drawn at a step of 1, 2 or 5 times a power of ten that makes some 40
  of the view's range; unticked, the `thresholds` and the style's
  filters. Tiles from
  [`rr_tiles_dem()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_tiles_dem.md)
  carry a fit, taken when none is given (by depth with a negative
  `multiplier`, else by elevation).

- state:

  For a vector, geojson, dem or contour layer: values in the reader's
  hands that its `style` reads, in a filter, a paint or a layout
  property, as `list("global-state", "<id>_<name>")` (MapLibre's global
  state), such as whether a style is drawn, its colour, or a shift of
  the zoom it starts at. A list of controls, each with `id` (`<name>`
  above: lower-case letters and digits) and `label`, and either
  `options` (a list of `value`, a single number, text or `TRUE`/`FALSE`,
  and `label`), a list to choose from, with `default` one of the values
  (the first when not given); or no `options` and `default` `TRUE` or
  `FALSE`, a checkbox. Each applies as it changes, nothing fetched
  again, and goes into the page's address as `<id>_<name>`. Every one
  must be read by the style.

## Value

An `rr_layer`, for
[`rr_section()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_section.md).

## Details

- `"grid"`: vector tiles of cells, coloured in the page by a numeric
  field on a log scale, with the palette, stretch, floor, cap and
  histogram there too. Its `filters` become controls whose values go
  into the tile address (one grid per map).

- `"vector"`: vector tiles of features, drawn by `style`, such as a
  table of geometries served by
  [`rr_tiles_table()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_tiles_table.md).

- `"geojson"`: features sent whole as GeoJSON, drawn by `style`.

- `"dem"`: image tiles of elevations (Terrarium or Mapbox Terrain-RGB),
  such as an MBTiles file read by
  [`rr_tiles_mbtiles()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_tiles_mbtiles.md),
  drawn by `style` in the page under the base map's labels: MapLibre's
  `hillshade` and `color-relief` layers, so the shading and the colours
  are made in the browser.

- `"contour"`: contour lines made in the browser from the same image
  tiles of elevations, by `maplibre-contour` (loaded by the page only
  for a map that has one), at the intervals of `contours`, drawn by
  `style` (a `line`, and a `symbol` for the labels, from the source
  layer `"contours"`, its features' `ele` and `level`) under the base
  map's labels.

Texts (`label`, `info`, a filter's `label`, a legend's `label`, ...) are
keys of the map's `texts`
([`rr_map()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_map.md)),
or the text itself where no key matches. MapLibre styles and expressions
are lists, so that a one-element array stays an array in JSON:
`list("get", "kind")`, `list("zoom")`; read from JSON with
`jsonlite::fromJSON(simplifyVector = FALSE)` they already are.

## See also

[`rr_map()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_map.md),
[`rr_tiles_table()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_tiles_table.md)

## Examples

``` r
# A vector layer: the harbours as areas, a click shows the harbour's id
# \donttest{
harbours <- rr_layer(
  "harbours", "vector", label = "Harbours",
  tiles = rr_tiles_table(rr_example("dansk_harbours"), properties = "SI_HARB"),
  style = list(list(id = "harbours-fill", type = "fill", "source-layer" = "features",
                    paint = list("fill-color" = "#d9822b"))),
  popup = list(click = "harbours-fill", title = "SI_HARB")
)
# }

# A grid: its tile function is given the filters' values in `params`. This one returns an
# empty tile; a real one returns DuckDB's ST_AsMVT() of the cells (see the "Maps in RStudio"
# article).
cells <- rr_layer(
  "cells", "grid", label = "Pings by cell",
  tiles = function(z, x, y, params) raw(0),
  filters = list(
    list(id = "behaviour", label = "Behaviour",
         options = list(list(value = "", label = "all"),
                        list(value = "Fishing", label = "fishing"))),
    list(id = "metric", type = "metric", label = "Metric")
  ),
  metrics = list(list(field = "n", label = "pings"))
)

# A geojson layer: features sent whole, here one point
point <- rr_layer(
  "point", "geojson", label = "A point",
  data = '{"type": "FeatureCollection", "features": [{"type": "Feature", "properties": {},
           "geometry": {"type": "Point", "coordinates": [12.6, 56.0]}}]}',
  style = list(list(id = "point-dot", type = "circle", paint = list("circle-color" = "#c0392b")))
)
names(cells$config)
#> [1] "id"      "kind"    "label"   "visible" "filters" "metrics"
```
