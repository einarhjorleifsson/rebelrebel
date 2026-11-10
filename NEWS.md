# rebelrebel 2026.11.10.8

* An id is a list, not a range: in `choose`, a whole number (stored as an integer) is a list, as text is, and a
  fraction, a date or a time a range. Where the data's dictionary (data-dict.tidyverse.org's, in the parquet file's
  metadata) gives a column's type, it decides: `number(id)`, `enum`, `string` or `boolean` a list, `number`,
  `number(quantity)`, `date` or `datetime` a range. A number declared an id is coloured as categories too, and written
  whole (`1131`, not `1131.0`).
* The examples' ids are stored as integers: `trawlsurvey`'s `vid` and `trawlsurveytows`' `id.lgs`, every row and the
  files' metadata kept (`data-raw/integer_ids.R`).
* `rr_map("file.parquet")` reads the file: a path given to the map itself was not read before.
* A dictionary's YAML keeps `n`, `y`, `no`, `yes`, `on` and `off` as written: a column called `n` stopped the reader.

# rebelrebel 2026.11.10.7

* `choose` takes ranges: a number, a date or a time is a slider with two handles, over each of its values where it has
  at most 64 (years, days), else over bins between `pretty()` breaks; text, a factor or a logical stays a list, as
  colour reads them. Ranges and lists are linked by the same counts (a vessel chosen, the days' range spans its
  days), ‹ and › step one value at a time over the values with rows or move the window by its width, and the address
  holds `day=2019-03-01..2019-03-03`. A numeric id is a list once it is text: `as.character(as.integer(id))`.
* A path is split by a range's bins, so a range chosen shows whole runs of it; the view fits the combinations every
  choice leaves, and stays where it is when a range is slid.

# rebelrebel 2026.11.10.6

* Points are thinned in each tile by default (`rr_point(thin = TRUE)`): one a screen pixel, each colour (a number's in
  eight classes), each value a `choose` list picks and each group of a time panel keeping its own, so a tile of
  millions of rows reaches the page as what can be seen of it. From the layer's deepest zoom (`maxzoom`, 14 by
  default) only points that would be drawn on one spot are dropped, so every point that can be told apart can be
  clicked. On a year of national AIS pings (32.8 million), the busiest tile at zoom 6 went from 245 MB and 8.9 s to
  8.7 MB and 0.9 s; at zoom 14, from 51 MB to 3.9 MB. `thin = FALSE` keeps every point; a number of pixels thins more.
* A point's size follows the zoom by default: 2 pixels across until the zoom at which most points lie that far apart,
  then doubling with each zoom in, as a size on the ground does, up to 8. `size = c(1, 12)` sets other bounds, one
  number a size that stays. The outline shows once a point is 3 pixels across.
* `rr_tiles_table(thin = )`, the same for any table of points; a thinned tile's features in the table's order, so its
  bytes are the same every time.

# rebelrebel 2026.11.10.5

* `choose` on a layer function is that layer's own: its lists under its name in the sidebar, over it alone, linked
  as the map's are, and in the page's address under the layer's id and the field (`pings_vid=1131`). A path is split
  by its own `choose` columns too, so a choice shows whole lines. Before, a layer function that started a map gave its
  `choose` to the map; the map's lists are now given to `rr_map()` alone.
* Lists over shapes (`rr_line()`, `rr_polygon()`) take each value's extent from its shapes.

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
