# rebelrebel

Fast interactive maps of parquet files and DuckDB tables, drawn in the browser by MapLibre GL JS, with the marine
environment first: vessel tracks, survey stations, gridded effort, depth, relief and contours. The data stay in DuckDB
and reach the page as vector tiles. A map is served from R on your machine, or written as a folder that opens from its
`index.html` with no R.

The package began on 2026-11-10 as the map viewer of [ramb](https://github.com/einarhjorleifsson/ramb), moved here
with its names changed (`rb_map_*()` to `rr_*()`).

The example data (`inst/extdata/*.parquet`) are Git LFS objects, so install from a clone that fetches them: with Git
and Git LFS on the machine,

```r
# install.packages("remotes")
remotes::install_git("https://github.com/einarhjorleifsson/rebelrebel", git = "external")
```

`remotes::install_github()` would install the LFS pointers instead of the data; `rr_example()` then stops and says so.

```r
library(rebelrebel)
harbours <- rr_layer(
  "harbours", "vector", label = "Harbours",
  tiles = rr_tiles_table(rr_example("dansk_harbours"), properties = "SI_HARB"),
  style = list(list(id = "harbours-fill", type = "fill", "source-layer" = "features",
                    paint = list("fill-color" = "#d9822b")))
)
m <- rr_map(rr_section("ref", "Reference", list(harbours)),
            view = list(center = c(10.5, 56.5), zoom = 6))
rr_serve(m)
```
