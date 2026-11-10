# Tiles of elevations from a grid

A tile function for a dem or contour
[`rr_layer()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_layer.md)
made from a regular grid of elevations, such as a clip of GEBCO or of
NOAA's ETOPO (which
[`marmap::getNOAA.bathy()`](https://rdrr.io/pkg/marmap/man/getNOAA.bathy.html)
reads), with no MBTiles file and no tool outside R. The grid is copied
once into a temporary DuckDB table. Each tile the page asks for is made
from the cells under it: their elevations interpolated (bilinear) at the
tile's pixels and written as a PNG image in the Terrarium encoding, from
which the page draws the relief, the colours and the contours. Where a
pixel spans several cells (the map zoomed out), the cells are first
averaged in square blocks, two, four, eight ... cells a side.

## Usage

``` r
rr_tiles_dem(
  x,
  lon = "lon",
  lat = "lat",
  elevation = "elevation",
  tile_size = 512L,
  minzoom = 0L,
  maxzoom = NULL,
  nodata = 0
)
```

## Arguments

- x:

  A data frame or lazy DuckDB table of a regular grid in longitude and
  latitude (WGS 84), a row per cell: its centre and its elevation. A
  cell may be missing, a row may not repeat a cell.

- lon, lat, elevation:

  Its columns: the centre's longitude and latitude, and the elevation in
  metres, negative below sea level (a depth is the elevation's negative:
  make the column first).

- tile_size:

  The tiles' side in pixels, 256 or 512.

- minzoom, maxzoom:

  The zooms tiles are made for; the map draws deeper zooms from the
  deepest's tiles, enlarged. By default from 0 to the first zoom at
  which a pixel is no wider than a cell.

- nodata:

  The elevation written where the grid has none: beyond its edge, and
  where the cell nearest a pixel is missing. By default 0, sea level,
  which the colours of a depth leave clear.

## Value

A function `function(z, x, y)` (`y` from the north, as the page asks)
returning a tile's bytes, a PNG; a tile wholly outside the grid is level
at `nodata`, for maplibre-contour draws no lines in a tile one of whose
eight neighbours is missing. With the attributes
[`rr_layer()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_layer.md)
takes for a dem or contour layer (`format`, `encoding`, `tile_size`,
`bounds`, `minzoom` and `maxzoom`), `source`, by which
[`rr_bundle()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_bundle.md)
writes the tiles of layers on one grid once, `table`, the temporary
table's name, `count`, the number of cells, and `fit`, a function of a
view's `west`, `south`, `east` and `north` (and `of`, `"depth"` or
`"elevation"`), which a layer on these tiles takes when it is given no
fit of its own: the floor and the cap at the 2nd and 98th percentiles of
the values in view, 25 quantiles between them, and the shading's
intensity, 2 over the 90th percentile of the slope in degrees, between
0.3 and 1 (the rules
[`rr_bundle()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_bundle.md)
applies in the browser). So the colours, the contours and the shading
follow the view with no more code.

## See also

[`rr_tiles_mbtiles()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_tiles_mbtiles.md),
for tiles already made;
[`rr_layer()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_layer.md)

## Examples

``` r
# a small hill, 21 by 21 cells of 0.01 degrees
g <- expand.grid(lon = seq(10, 10.2, by = 0.01), lat = seq(56, 56.2, by = 0.01))
g$elevation <- 100 - 5000 * ((g$lon - 10.1)^2 + (g$lat - 56.1)^2)
if (requireNamespace("png", quietly = TRUE)) {
  tiles <- rr_tiles_dem(g)
  attributes(tiles)[c("bounds", "minzoom", "maxzoom", "count")]
  png_bytes <- tiles(10, 540, 318) # the tile over the hill at zoom 10
  rgb <- png::readPNG(png_bytes) * 255
  # the Terrarium encoding: (red * 256 + green + blue / 256) - 32768
  range(rgb[, , 1] * 256 + rgb[, , 2] + rgb[, , 3] / 256 - 32768)
}
#> [1]  0.00000 99.97656
```
