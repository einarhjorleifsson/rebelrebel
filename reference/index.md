# Package index

## Start a map

A map from data, which the layer functions add to with the pipe.

- [`rr_map()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_map.md)
  : A map: layers in groups, texts, base maps and palettes

## Layers

A mark each, one row of the data to one mark: points, paths (rows joined
in time), lines, segments, areas and text.

- [`rr_point()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_point.md)
  [`rr_line()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_point.md)
  [`rr_segment()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_point.md)
  [`rr_polygon()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_point.md)
  [`rr_text()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_point.md)
  : Points, lines, areas, segments and text
- [`rr_path()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_path.md)
  : Paths: rows joined in time, a line per group

## Time

A panel under the map: a group’s rows over time, linked to the map.

- [`rr_time()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_time.md)
  : A time panel under the map: one group's rows over time, linked to
  the map

## In MapLibre’s own terms

A layer, its section and its tiles made by hand, for what the layer
functions do not draw.

- [`rr_layer()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_layer.md)
  : A layer of a map
- [`rr_section()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_section.md)
  : A group of layers in a map's sidebar
- [`rr_tiles_table()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_tiles_table.md)
  : A table of geometries as vector tiles
- [`rr_tiles_dem()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_tiles_dem.md)
  : Tiles of elevations from a grid
- [`rr_tiles_mbtiles()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_tiles_mbtiles.md)
  : Tiles from an MBTiles file

## Colours and base maps

- [`rr_palettes()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_palettes.md)
  [`rr_basemaps()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_palettes.md)
  : The palettes and base maps of a map

## Show and share

A map served from R on this machine, or written as a folder that opens
with no R.

- [`print(`*`<rr_map>`*`)`](https://einarhjorleifsson.github.io/rebelrebel/reference/print.rr_map.md)
  [`format(`*`<rr_map>`*`)`](https://einarhjorleifsson.github.io/rebelrebel/reference/print.rr_map.md)
  : Show a map
- [`rr_serve()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_serve.md)
  [`rr_stop()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_serve.md)
  : Serve a map
- [`rr_bundle()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_bundle.md)
  : Write a map as a folder that opens with no R and no server

## What readers write

Notes and verdicts written on a served map, kept in a file beside it.

- [`rr_notes()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_notes.md)
  [`rr_notes_add()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_notes.md)
  : Notes pinned on a served map
- [`rr_verdicts()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_verdicts.md)
  : Verdicts given on a served map

## Example data

- [`rr_example()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_example.md)
  : Read an example dataset shipped with rebelrebel
- [`dansk`](https://einarhjorleifsson.github.io/rebelrebel/reference/dansk.md)
  : Danish vessel tracks (AIS)
- [`dansk_harbours`](https://einarhjorleifsson.github.io/rebelrebel/reference/dansk_harbours.md)
  : Harbour polygons for the Danish tracks
- [`dansk_relief`](https://einarhjorleifsson.github.io/rebelrebel/reference/dansk_relief.md)
  : Seabed and land elevation around the Danish tracks
- [`trawlsurvey`](https://einarhjorleifsson.github.io/rebelrebel/reference/trawlsurvey.md)
  : Icelandic groundfish survey, March 2019, vessel tracks
- [`trawlsurveytows`](https://einarhjorleifsson.github.io/rebelrebel/reference/trawlsurveytows.md)
  : Icelandic groundfish survey, March 2019, tows
