# A table of geometries as vector tiles

Makes a tile function for a `"vector"`
[`rr_layer()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_layer.md)
from a table of points, lines or areas: the table is copied once into a
temporary DuckDB table in web mercator, with each feature's bounds, and
each tile is cut from it by DuckDB (`ST_AsMVT()`). Every feature carries
`kind` (`"point"`, `"line"` or `"area"`) and the `properties`.

## Usage

``` r
rr_tiles_table(
  x,
  geometry = "geometry",
  lon = NULL,
  lat = NULL,
  properties = NULL,
  layer = "features",
  points = NULL,
  thin = NULL
)
```

## Arguments

- x:

  A lazy DuckDB table or a data frame: a geometry column (DuckDB's
  GEOMETRY, or WKB), or `lon` and `lat`.

- geometry:

  The geometry column; or `lon`, `lat` for points from two columns.

- lon, lat:

  The position columns, for points.

- properties:

  The columns each feature carries. A vector tile holds text, numbers
  and logicals only, so a factor, date or time is carried as text, a
  small integer as an integer and a decimal as a double.

- layer:

  The tile layer of the features.

- points:

  The tile layer of a point on every feature (`ST_PointOnSurface()`),
  for the zooms where an area is too small to see; `NULL` for none.

- thin:

  Points thinned in each tile, where a screen pixel holds many: `NULL`
  for none, or a list of `px`, the side in screen pixels of the cells
  that each keep one point (a tile drawn 512 pixels wide); `by`, columns
  whose values each keep their own point in a cell (a colour's classes,
  a column chosen in the page), carried in the tiles only if
  `properties` names them; and `until`, the zoom from which only the
  points on one spot of a tile's own grid (4,096 a side, an eighth of a
  pixel) are dropped, the source's deepest, so that every point that can
  be told apart can be clicked there and beyond. The point kept is a
  cell's first in the table's order, the same every time. Lines and
  areas are kept whole.

## Value

A function of `z`, `x`, `y` returning a tile (raw bytes), with the
number of features as its `count` attribute, the temporary table's name
as its `table` attribute, and as `tiles_at` a function of a zoom giving
the tiles that hold a feature (`x`, `y`), by which
[`rr_bundle()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_bundle.md)
writes only those.

## Examples

``` r
# \donttest{
tiles <- rr_tiles_table(rr_example("dansk_harbours"),
  properties = "SI_HARB", points = "harbour_points")
attr(tiles, "count")
#> [1] 942
length(tiles(7, 68, 39))
#> [1] 1730
# }
```
