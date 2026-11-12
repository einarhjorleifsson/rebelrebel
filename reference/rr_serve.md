# Serve a map

Starts a server for an
[`rr_map()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_map.md)
in the background of this R session, on `host` only (`127.0.0.1`: this
computer), and opens the page. It answers while the console is idle: a
map waits while R runs something else. `rr_stop()` stops it. A grid's
`meta` and a geojson layer's `data` are read once, when the server
starts.

## Usage

``` r
rr_serve(
  map,
  port = NULL,
  host = "127.0.0.1",
  open = interactive(),
  query = NULL,
  notes = NULL,
  verdicts = NULL
)

rr_stop(port = NULL)
```

## Arguments

- map:

  An
  [`rr_map()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_map.md).

- port:

  Where the server listens: a port, or `NULL` (the default) for 8790 or,
  when another program holds it, the next free port to 8889. A map
  served with no port replaces the last one served so, as a map served
  on a port replaces the one there; the address returned says where it
  is.

- host:

  Where the server listens: `127.0.0.1`, this computer only.

- open:

  Whether to open the page: RStudio's Viewer when there is one, else a
  browser.

- query:

  Settings for the first view, in the page's address
  (`list(lang = "is", palette = "deep")`).

- notes:

  Opt-in, notes pinned on the map: the path of a `.geojson` file (in a
  folder that exists) where the server keeps them, made at the first
  note; `NULL`, none. The page then has a Notes group: "Add a note", a
  click on the map, a text, and the note is written to the file with the
  features under it; a reply joins a note's thread.
  [`rr_notes()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_notes.md)
  reads them in R and
  [`rr_notes_add()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_notes.md)
  answers. They are answered only for a page opened from this computer
  (as `localhost`, `127.0.0.1` or `host`), and never in a bundle.

- verdicts:

  Opt-in, the readers' verdicts on features: the path of a `.csv` file
  (in a folder that exists) where the server keeps them, made at the
  first verdict; `NULL`, none. A layer whose popup has a `verdict`
  ([`rr_layer()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_layer.md))
  then shows a button for each option in a feature's popup. Each answer
  is a row added to the file (when, the layer, the feature's value of
  the verdict's `field`, the option, the reader's line, the feature's
  title and place); a later one for the feature replaces it, and an
  empty answer withdraws it.
  [`rr_verdicts()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_verdicts.md)
  reads them. As the notes, answered only for a page opened from this
  computer, and never in a bundle.

## Value

The page's address, invisibly.

## Examples

``` r
# Opens a map in RStudio's Viewer (or a browser) when run by hand, e.g. by example(rr_serve)
if (interactive()) {
  harbours <- rr_layer(
    "harbours", "vector", label = "Harbours",
    tiles = rr_tiles_table(rr_example("dansk_harbours"), properties = "SI_HARB"),
    style = list(list(id = "harbours-fill", type = "fill", "source-layer" = "features",
                      paint = list("fill-color" = "#d9822b"))),
    popup = list(click = "harbours-fill", title = "SI_HARB")
  )
  m <- rr_map(rr_section("ref", "Reference", list(harbours)),
              title = "Danish harbours", view = list(center = c(10.5, 56), zoom = 7))
  rr_serve(m)
  # again, opening on the ocean base map (it replaces the first):
  # rr_serve(m, query = list(base = "ocean"))
  # with notes pinned on the map, kept in a file:
  # rr_serve(m, notes = "harbour_notes.geojson"); rr_notes("harbour_notes.geojson")
  # rr_stop() # when done
}
```
