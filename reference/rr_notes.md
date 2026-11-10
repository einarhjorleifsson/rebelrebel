# Notes pinned on a served map

A map served with `rr_serve(notes = )` takes notes: in its Notes group
the reader presses "Add a note", clicks the map and writes, and the
server writes the note to the file, a GeoJSON FeatureCollection with a
point for each note. A note keeps its `id`, its `time` (UTC), its
`author` (`"reader"` from the page), its `text`, the map's `zoom` and
the page's `address` (its query and position: the view and layers as the
reader saw them), and `under`, the features under it: each "layer:
title", from the layers whose popup has a `title`. A reply joins a
note's thread (`reply_to`, the first note's `id`) at its place. The page
shows a pin for each thread, red while its last word is the reader's and
blue once answered, with the thread and a box to reply in on a click,
and asks for the notes again when it regains the focus and every 30 s.

## Usage

``` r
rr_notes(path)

rr_notes_add(
  path,
  text,
  lon = NULL,
  lat = NULL,
  author = "analyst",
  reply_to = NULL
)
```

## Arguments

- path:

  The notes' file (`.geojson`).

- text:

  The note's text.

- lon, lat:

  Where it is pinned. A reply's place is its thread's.

- author:

  Who wrote it.

- reply_to:

  The `id` of the note it answers; a reply to a reply joins the same
  thread.

## Value

`rr_notes()`: a data frame, a row for each note in the order written:
`id`, `thread` (the `id` of its thread's first note), `reply_to`, `time`
(POSIXct, UTC), `author`, `text`, `lon`, `lat`, `zoom`, `under` (the
features under it, separated by "; ") and `address`; no rows when there
is no file. `rr_notes_add()`: the new note's `id`, invisibly.

## Details

`rr_notes()` reads the notes, `rr_notes_add()` adds a note or a reply
from R: a dialogue about places on a map, between a reader in the page
and someone at R (or an assistant reading the file). Both may run while
the map is served, in this R session or another: each write replaces the
file whole, by a rename. The file is plain GeoJSON, a note a line, and
opens in any GIS. Notes are written only by R in a UTF-8 locale, where
letters outside ASCII are kept as they are.

## See also

[`rr_serve()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_serve.md)

## Examples

``` r
f <- tempfile(fileext = ".geojson")
rr_notes_add(f, "Is the bank's edge here?", lon = -14.6, lat = 64.0, author = "reader")
rr_notes_add(f, "Yes, at the 200 m contour.", reply_to = 1, author = "analyst")
rr_notes(f)[, c("id", "thread", "author", "text")]
#>   id thread  author                       text
#> 1  1      1  reader   Is the bank's edge here?
#> 2  2      1 analyst Yes, at the 200 m contour.
# a map served with them: rr_serve(m, notes = f)
```
