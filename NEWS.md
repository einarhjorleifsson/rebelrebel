# rebelrebel 2026.11.10.4

* `choose`'s lists are linked: each offers only the values the other lists' choices leave, from the combinations the
  data hold, with their rows counted again; a choice the others no longer leave goes back to all, the last list first
  (a trip gives way to its vessel), and ‹ and › step through the values left. So `choose = c(vid, tid)` needs no
  nesting declared: a vessel chosen leaves its trips.

# rebelrebel 2026.11.10.3

* A click shows the feature's row as a table: the tiles carry the row's number and title only, and the server answers
  `/row/<layer>/<n>` with the columns `popup` names (2-3 ms a click); a bundle writes a layer's rows beside its tiles,
  up to 2 million values. Whole numbers as they are, fractions to six significant digits in the page's language, yes
  and no, a dash where missing; a web address a link, a picture's address the picture, a video's a link.
* `labels` and `codes` in every layer function: a column's name, unit and description, and a coded column's labels,
  by default from the data (a data frame's `label` and `units` attributes, or a parquet file's own dictionary in
  data-dict.tidyverse.org's format).
* `rr_path()` takes a number as its order (a sequence, a distance along a transect) as well as a time.

# rebelrebel 2026.11.10.2

* The layer functions, built with the pipe: `rr_point()`, `rr_path()` (rows joined in time, a line per `group`, one
  column or several), `rr_line()`, `rr_segment()` (a line from each row's start to its end), `rr_polygon()` and
  `rr_text()`, one internal function for all. They take their arguments in one order and words (`colour`, `fill`,
  `size`, `linewidth`, `opacity`, `label`; `palette`, `limits`, `stretch`, `legend`; `popup`, `hover`, `choose`;
  `name`, `info`, `section`, `visible`; `minzoom`, `maxzoom`). A bare name is a column, a string a value.
* `rr_map(data)` starts a map the layer functions add to; `choose` names the columns the reader picks, `sections` the
  sidebar's boxes. A layer function given data first starts a map. Data are a parquet file, folder or glob, a lazy
  DuckDB table, a data frame or an sf object.
* Colours from the data: Okabe-Ito for up to eight values, Paired for up to twelve, Glasbey's 32 beyond, the rest
  grey as "other"; viridis for ordered factors, numbers, dates and times (over the 1st to 99th percentile, or
  `limits`, stretched linearly, on a log scale or in equal shares). `palette` overrides them: a name written loosely,
  colours, colours by value, or a function of n.
* The time panel is generic: `rr_time()` takes its columns (`col`, no default) and its group (`by`) from the call or
  from the map (a path's `group` and `time`); interval tables are read by `start` and `end`, or as named; the page
  names the group's list by its column and counts rows. Points carry their time and group for it.
* The quick look at vessels' pings and tracks is no longer here: it is ramb's, as wrappers over these functions.

# rebelrebel 2026.11.10.1

* The base maps are those that show the sea floor: EMODnet's sea floor (its 2020 base
  layer), the first and the one a map opens on; the same darkened (`emodnet_dark`); EMODnet's lighter atlas (v12); and
  Esri's ocean, which reaches beyond Europe's seas; and none. Carto's Positron, Voyager and Dark Matter (a flat sea)
  and Esri's satellite tiles are no longer offered. Each picture has a Carto style's place and sea names drawn over it
  (Positron's, or Dark Matter's on the dark one), which also give the layers' own labels their fonts: a base map's
  `labels`, `label_layers`, `paint` and `background` in `rr_basemaps()`. The page opens on a picture base map.

# rebelrebel 2026.11.10

* The package starts as ramb's map viewer, moved here unchanged but for its names:
  `rb_map()` is `rr_map()`, `rb_map_layer()` `rr_layer()`, `rb_map_group()` `rr_section()`, `rb_map_serve()` and
  `rb_map_stop()` `rr_serve()` and `rr_stop()`, `rb_map_bundle()` `rr_bundle()`, the tile makers `rr_tiles_table()`,
  `rr_tiles_dem()` and `rr_tiles_mbtiles()`, `rr_palettes()`, `rr_basemaps()`, `rr_notes()`, `rr_notes_add()`,
  `rr_verdicts()` and `rr_time()`; ramb's `rb_read_example()` is `rr_example()`, with five of ramb's datasets
  (`dansk`, `dansk_harbours`, `dansk_relief`, `trawlsurvey`, `trawlsurveytows`). `rb_map_pings()` and
  `rb_map_track()` are internal until `rr_point()` and `rr_path()` take their place.
* The page's libraries are in the package (`inst/engine/vendor`): MapLibre GL JS 5.24.0, maplibre-contour 0.1.1 and
  uPlot 1.6.30, served by `rr_serve()` at `/vendor/` and copied into a bundle's `vendor` folder, so a map draws with no
  network but the base map's.
