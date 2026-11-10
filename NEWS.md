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
