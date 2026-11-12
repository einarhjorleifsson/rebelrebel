# Tiles of elevations from a grid

A tile function for a dem or contour
[`rr_layer()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_layer.md)
made from a grid of elevations: a GeoTIFF (a Cloud Optimized GeoTIFF
when it is large) or any grid GDAL reads, such as GEBCO's netCDF, in any
projection; a terra `SpatRaster`; or a data frame of a grid's cells.
Each tile the page asks for is warped from the grid by GDAL (through
terra), the grid resampled once: averaged where a pixel is wider than a
cell (the map zoomed out), interpolated (bilinear) where it is narrower.
The tile is written as a PNG image in the Terrarium encoding, from which
the page draws the relief, the colours and the contours, and fits them
to the view.

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

  The grid: a path of a raster file, a terra `SpatRaster`, or a data
  frame (or a lazy table of at most 10 million rows, read into R) of a
  regular grid in longitude and latitude (WGS 84), a row per cell: its
  centre and its elevation. A cell may be missing, a row may not repeat
  a cell.

- lon, lat, elevation:

  For a data frame, its columns: the centre's longitude and latitude,
  and the elevation in metres, negative below sea level (a depth is the
  elevation's negative: make the column first). For a raster of several
  layers, `elevation` names the one drawn; a raster's values are metres,
  negative below sea level.

- tile_size:

  The tiles' side in pixels, 256 or 512.

- minzoom, maxzoom:

  The zooms tiles are made for; the map draws deeper zooms from the
  deepest's tiles, enlarged. By default from 0 to the first zoom at
  which a pixel is no wider than a cell, but no deeper than 8: a pixel
  of about 300 m at the equator and 130 m at 65 degrees north (512-pixel
  tiles), an overview rather than every detail of a fine grid. Give
  `maxzoom` to go deeper; each zoom takes about four times the bytes of
  the one before.

- nodata:

  The elevation written where the grid has none: beyond its edge, and
  where a cell a pixel is drawn from is missing. By default 0, sea
  level, which the colours of a depth leave clear.

## Value

A function `function(z, x, y)` (`y` from the north, as the page asks)
returning a tile's bytes, a PNG; a tile wholly outside the grid is level
at `nodata`, for maplibre-contour draws no lines in a tile one of whose
eight neighbours is missing. With the attributes
[`rr_layer()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_layer.md)
takes for a dem or contour layer (`format`, `encoding`, `tile_size`,
`bounds`, `minzoom` and `maxzoom`); `source`, by which
[`rr_bundle()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_bundle.md)
writes the tiles of layers on one grid once; `count`, the grid's number
of cells, missing ones included; and `fit`, `"page"`: a layer on these
tiles fits its colours, contours and shading to the view in the page,
from the tiles in view, with no call to R (the rules of
[`rr_bundle()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_bundle.md)'s
`fit`, by default the floor and the cap at the 2nd and 98th percentiles
of the values in view, 25 quantiles between them, and the shading's
intensity 2 over the 90th percentile of the slope in degrees, between
0.3 and 1).

## Details

A grid is read where it is: only the part under a tile, and from a
GeoTIFF's overviews where a pixel spans eight cells or more (the full
grid nearer the deepest zoom, as overviews average twice). A large grid
that is no Cloud Optimized GeoTIFF is read whole for a tile zoomed far
out: `gdal_translate -of COG` makes one, losing nothing.

## See also

[`rr_tiles_mbtiles()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_tiles_mbtiles.md),
for tiles already made;
[`rr_layer()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_layer.md)

## Examples

``` r
# a small hill, 21 by 21 cells of 0.01 degrees
g <- expand.grid(lon = seq(10, 10.2, by = 0.01), lat = seq(56, 56.2, by = 0.01))
g$elevation <- 100 - 5000 * ((g$lon - 10.1)^2 + (g$lat - 56.1)^2)
if (requireNamespace("png", quietly = TRUE) && requireNamespace("terra", quietly = TRUE)) {
  tiles <- rr_tiles_dem(g)
  attributes(tiles)[c("bounds", "minzoom", "maxzoom", "count")]
  png_bytes <- tiles(10, 540, 318) # the tile over the hill at zoom 10
  rgb <- png::readPNG(png_bytes) * 255
  # the Terrarium encoding: (red * 256 + green + blue / 256) - 32768
  range(rgb[, , 1] * 256 + rgb[, , 2] + rgb[, , 3] / 256 - 32768)

  # the same grid as a GeoTIFF, as most grids come
  f <- tempfile(fileext = ".tif")
  terra::writeRaster(terra::rast(g, type = "xyz", crs = "EPSG:4326"), f)
  attributes(rr_tiles_dem(f))[c("bounds", "maxzoom")]
}
#> $bounds
#> [1]  9.995 55.995 10.205 56.205
#> 
#> $maxzoom
#> [1] 7
#> 
```
