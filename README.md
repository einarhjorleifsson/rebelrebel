# rebelrebel

Fast interactive maps in R, drawn in the browser by MapLibre GL JS, with the sea in mind: tracks, stations, gridded
values, areas and labels over the sea floor. In the company of leaflet, mapview and tmap, rebelrebel keeps the data
in DuckDB: a parquet file of millions of rows reaches the page as vector tiles, so the map stays fast, and its colours,
legend and choices change in the page without asking R again.

A map is built with the pipe, a layer at a time, in the words of ggplot2:

```r
library(rebelrebel)

rr_map(rr_example("trawlsurvey"), title = "A survey") |>
  rr_path(group = vid, time = time, colour = behaviour) |>
  rr_point(colour = speed, popup = c(vid, time, speed), minzoom = 8, visible = FALSE)
```

Printed at the console, the map opens in the viewer, served from your machine. `rr_bundle()` writes it as a folder
that opens from its `index.html`, with no R and no network but the base map's.

## What there is

- **Layers:** `rr_point()`, `rr_path()` (rows joined in time, a line per group), `rr_line()`, `rr_segment()` (a
  line from each row's start to its end), `rr_polygon()`, `rr_text()`; and `rr_layer()` for a layer in MapLibre's
  own terms.
- **Data:** a parquet file, folder or glob, a lazy DuckDB table, a data frame or an sf object. A bare name is a
  column, a string a value: `colour = speed` or `colour = "#c0392b"`.
- **Colours from the data:** Okabe-Ito for up to eight values, Paired for up to twelve, Glasbey's 32 beyond; viridis
  for numbers, dates and ordered values; any palette by name, by colours or by value with `palette`.
- **The sea floor:** base maps that show it (EMODnet's, light and dark, Esri's ocean), and depth, relief and
  contours from your own grid (`rr_tiles_dem()`, `rr_tiles_mbtiles()`).
- **In the page:** popups, hover labels, a choice of colouring, choices of what to show, a time panel under the map
  (`rr_time()`), notes and verdicts written by readers, and the whole view in the page's address.

## Installation

The example data are Git LFS objects, so install from a clone that fetches them, with Git and Git LFS on the machine:

```r
# install.packages("remotes")
remotes::install_git("https://github.com/einarhjorleifsson/rebelrebel", git = "external")
```

`remotes::install_github()` would install the LFS pointers instead of the data; `rr_example()` then stops and says
so.

## Status

Young, and changing: the layer functions are new, and a one-call `rr_view()`, cells (`rr_grid()`), depth
(`rr_relief()`, `rr_contour()`) and figures saved at a chosen size and resolution (`rr_save()`) are to come. The
package began as the map viewer of the R package [ramb](https://github.com/einarhjorleifsson/ramb).
