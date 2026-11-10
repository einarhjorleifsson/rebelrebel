# Write a map as a folder that opens with no R and no server

`rr_bundle()` writes an
[`rr_map()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_map.md)
as a folder whose `index.html` opens in a browser with a double click,
on any system, with no R and no server: the page with the map's config,
and beside it the layers' tiles and data as script files (`tiles/`,
`data/`), which a page opened as a file (`file://`) may load where it
may not fetch them. It needs the internet only for the base maps:
MapLibre, maplibre-contour and uPlot are in its `vendor` folder.

## Usage

``` r
rr_bundle(
  map,
  dir,
  maxzoom = NULL,
  fit = list(probs = c(0.02, 0.98), n = 25, nodata = 0, slope = list(k = 2, p = 0.9, min
    = 0.3, max = 1)),
  grids = list(),
  overwrite = FALSE
)
```

## Arguments

- map:

  An
  [`rr_map()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_map.md).

- dir:

  The folder to write. If it already holds a bundle (`index.html`,
  `tiles/`, `data/`, `grids/`, `time/` or `vendor/`), `overwrite`
  replaces those and leaves the rest of the folder as it is.

- maxzoom:

  The deepest zoom of the tiles written (dem, contour and vector
  layers), at most each layer's own; `NULL` for each layer's own.

- fit:

  A view's fit in the browser: `probs`, the percentiles (0-1) of the
  floor and the cap, with `n` quantiles from one to the other (for
  colours in equal shares and contour levels); `nodata`, an elevation
  that means no value (`NULL` for none); `slope`, a shading's intensity:
  `k` over the `p` percentile of the slope in degrees, kept between
  `min` and `max`. Any left out keeps its default: those of a depth map
  whose tiles hold 0 on land.

- grids:

  A grid layer's rows for the browser, by the layer's id, each a list
  of:

  - `rows`: a data frame or a lazy table
    ([`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html) on
    DuckDB), a row for each cell and combination of the filters' values,
    with the cell's south-west corner as whole numbers `x` and `y` of
    `unit` degrees;

  - `unit`: the degrees of one step of `x` and `y`;

  - `levels`: the cell sizes written, in steps of `unit`, rising, each a
    multiple of the rows' own cell;

  - `columns`: a named character vector, a filter's id to the column it
    selects on (a range filter's column holds the values its two
    parameters bound);

  - `sums`: the columns added up within a cell;

  - `fields`: a named list, each of the tile's fields a sum's column, or
    `c(<column>, "km2")` for that sum per square kilometre of the cell;

  - `x`, `y`: the corner's columns, `"x"` and `"y"` unless given;

  - `px`: the filter that scales the cells to that many screen pixels,
    if any.

- overwrite:

  Whether to replace a bundle already in `dir`.

## Value

`dir`, invisibly, with the number of tiles written (`tiles`) and the
bundle's size in bytes (`bytes`) as attributes.

## Details

What a bundle carries:

- a geojson layer, its data;

- a dem or contour layer, its tiles from its `minzoom` to its `maxzoom`
  inside its `bounds`, written once for the layers that read one file
  (the `source` of
  [`rr_tiles_mbtiles()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_tiles_mbtiles.md)).
  Past the deepest zoom written, the map draws that zoom's tiles larger.
  A view's fit (a colour's, the contours' or a shading's `fit`) is made
  in the browser from the layer's tiles in view, by the rules in `fit`,
  not by the layer's R function, which needs a server;

- a vector layer from
  [`rr_tiles_table()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_tiles_table.md),
  the tiles that hold a feature, to the deepest zoom written;

- a time panel
  ([`rr_time()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_time.md)),
  each vessel's spans, the whole and each month, a file each (`time/`);

- a grid layer named in `grids`, its rows: summed by cell at a few cell
  sizes and written in blocks of cells (`grids/`), from which the page
  makes each tile, its filters applied and its cells summed in the
  browser, with the fields its server would send. Its cells hold one to
  two screen pixels (the filter `px` times that), from the cell sizes
  written. A filter the rows have no column for is left out, with a
  message.

Another vector layer, or a grid layer not in `grids`, is left out, with
a message: their tiles are made by a server.

## See also

[`rr_serve()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_serve.md),
which serves the map from this R session instead.

## Examples

``` r
places <- sf::st_as_sf(
  data.frame(name = c("Skagen", "Hirtshals"), lon = c(10.59, 9.96), lat = c(57.72, 57.59)),
  coords = c("lon", "lat"), crs = 4326
)
layer <- rr_layer(
  "places", "geojson", label = "Places", data = places,
  style = list(list(id = "places-dot", type = "circle", paint = list("circle-color" = "#d9822b"))),
  popup = list(click = "places-dot", title = "name")
)
m <- rr_map(rr_section("ref", "Reference", list(layer)),
            title = "Two harbours", view = list(center = c(10.3, 57.6), zoom = 8))
d <- rr_bundle(m, file.path(tempdir(), "two_harbours"), overwrite = TRUE)
list.files(d, recursive = TRUE)
#>  [1] "data/places.js"                      "index.html"                         
#>  [3] "vendor/LICENSE-maplibre-contour.txt" "vendor/LICENSE-maplibre-gl.txt"     
#>  [5] "vendor/LICENSE-uplot.txt"            "vendor/maplibre-contour.min.js"     
#>  [7] "vendor/maplibre-gl.css"              "vendor/maplibre-gl.js"              
#>  [9] "vendor/uPlot.iife.min.js"            "vendor/uPlot.min.css"               
# utils::browseURL(file.path(d, "index.html"))  # opens it in a browser
```
