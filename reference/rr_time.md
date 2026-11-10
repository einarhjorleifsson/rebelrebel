# A time panel under the map: one group's rows over time, linked to the map

`rr_time()` adds a panel under a map, or starts a map with it, that
shows one group's rows over time (a vessel's, a float's, an animal's:
the values of `by`):

- a column or several as lines, the first shown and the others ticked on
  in the panel;

- marks where a column says so (a flag), on the chart and on the map;

- lanes of intervals under the chart (visits, legs, phases), a click on
  one setting the window to it.

## Usage

``` r
rr_time(
  x,
  col,
  marks = NULL,
  lanes = NULL,
  span = NULL,
  max_gap_s = NULL,
  span_max = 3e+06,
  time_label = "UTC",
  data = NULL,
  lon = NULL,
  lat = NULL,
  time = NULL,
  by = NULL,
  title = "Track",
  port = 8790L
)
```

## Arguments

- x:

  Rows: a data frame or lazy DuckDB table with `lon` and `lat` (WGS 84),
  or an sf object of points; or a map, whose rows, time and group (from
  [`rr_path()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_path.md))
  the panel takes.

- col:

  The numeric columns drawn over time, a name, a string or
  [`c()`](https://rdrr.io/r/base/c.html) of them; the first shown.

- marks:

  Columns that mark rows, as `col`: a logical column marks its `TRUE`
  rows, any other its values that are not missing (such as a flag's
  label). Each column is a row of ticks along the chart's top and a ring
  on the map in a colour of its own, named in the sidebar;
  `c(truth = "wrong", clean = "flag")` names them.

- lanes:

  Intervals drawn as lanes under the chart: a named list (the names are
  the lanes' labels), each

  - a column of the rows, as a string: its runs of one value (a phase, a
    leg), missing values left out and a run broken by a silence longer
    than `max_gap_s`;

  - a table of intervals with the group's column (`by`), and its start
    and end in `start` and `end`;

  - or `list(data = , start = , end = , label = )`, naming the columns,
    `label` a column that names each interval.

- span:

  Units the page holds one at a time, chosen besides the whole series: a
  column of the rows that names them, as a string (a leg, a deployment:
  `"leg"`, or `c(leg = "leg_id")` to name the list), one span per value
  from its first row to its last; or a table of intervals, or
  `list(data = , start = , end = , label = )`, as for `lanes`, named as
  a column is with `list(legs = <table>)`. A span holds every row of its
  group from its start to its end, those whose value is missing too.
  Spans that overlap are reported: a state that comes and goes, such as
  behaviour, belongs in `lanes`. `NULL`, the default, offers the whole
  series alone.

- max_gap_s:

  A silence between two rows longer than this many seconds breaks the
  panel's line, on the chart and on the map, and a lane's run of one
  value. By default a path's
  ([`rr_path()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_path.md)'s
  `max_gap_s`) when the map has one already, else `Inf`: nothing broken,
  so no gap is hidden.

- span_max:

  The most rows the page holds at once: a group with more is offered by
  its spans, else by month. The browser holds far more than the default
  is likely to meet: a series of 2.2 million rows loaded in about 8
  seconds, most of it the transfer.

- time_label:

  The clock the times are on, shown beside them.

- data:

  Rows for this panel, in place of the map's.

- lon, lat, time:

  The position and time columns: by default the map's, else `"lon"`,
  `"lat"` and `"time"`.

- by:

  The column whose values each have a series, a name or a string: by
  default the map's (a path's `group`), else one series of all the rows.

- title, port:

  The page's title, and where the map is served when printed: set by the
  call that starts it.

## Value

An
[`rr_map()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_map.md)
with the time panel.

## Details

The panel and the map are linked. A drag across the chart sets the time
window, and a double click goes back to the whole span. The map draws
the window's path and points, and fits to them. A hover on the chart
marks that row on the map, and a click on a point of the window marks
its time on the chart. The group, the span and the window go into the
page's address.

The page holds one group's span at a time: by default its whole series;
with `span`, also each of its intervals, chosen under *Span*. A group
with more than `span_max` rows is offered by its spans only, or by month
where there are none. The map's other layers that carry the group and a
time
([`rr_path()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_path.md),
[`rr_point()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_point.md))
show only the chosen group and span; a path's line that runs over a
span's start or end is shown whole. The window's path is drawn whole,
its points thinned to about 5,000 (every k-th, a marked one always), so
the map stays quick.

Times are shown as the data hold them, in no time zone, labelled by
`time_label`.

## See also

[`rr_path()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_path.md)
and
[`rr_point()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_point.md),
the layers it pairs with;
[`rr_bundle()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_bundle.md),
which writes the panel's series beside the page.

## Examples

``` r
# \donttest{
d <- dplyr::filter(dplyr::collect(rr_example("dansk")), vessel_id %in% c("EX_1", "EX_12"))
m <- rr_map(d) |>
  rr_path(group = vessel_id, time = time_stamp) |>
  rr_time(speed, lanes = list(behaviour = "behaviour"))
format(m)
#> [1] "<rr_map> Map"                                             
#> [2] "  layers: paths (vector)"                                 
#> [3] "  time panel: speed over time, 2 groups; lanes: behaviour"
# m  # printed at the console, the map opens with the panel under it
# }
```
