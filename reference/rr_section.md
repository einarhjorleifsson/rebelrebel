# A group of layers in a map's sidebar

A group of layers in a map's sidebar

## Usage

``` r
rr_section(id, title = id, layers, open = TRUE)
```

## Arguments

- id:

  The group's name.

- title:

  Its title in the sidebar: a key of the map's `texts`, or the text
  itself.

- layers:

  A list of
  [`rr_layer()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_layer.md)s,
  in the order they are listed and drawn (the later over the earlier).

- open:

  Whether the group is unfolded when the page opens.

## Value

An `rr_section`, for
[`rr_map()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_map.md).

## Examples

``` r
point <- rr_layer(
  "point", "geojson", label = "A point",
  data = '{"type": "Point", "coordinates": [12.6, 56.0]}',
  style = list(list(id = "point-dot", type = "circle"))
)
ref <- rr_section("ref", "Reference", list(point), open = FALSE)
ref$title
#> [1] "Reference"
```
