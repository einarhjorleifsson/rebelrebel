# map.R — a map viewer: layers declared in R, a page (inst/engine/index.html, the engine) that shows them from a
# config, and a small httpuv server with fixed routes (serve.R). The data stay in DuckDB and reach the browser as
# vector tiles, so colours, filters and the legend work in the page. Moved from ramb's R/map.R on 2026-11-10
#: the names changed, the behaviour not.
#
# Routes of rr_serve():
#   /                                the page, with the map's config written in
#   /config.json                     the config
#   /vendor/<file>                   MapLibre, maplibre-contour and uPlot, as the package holds them (inst/engine/vendor)
#   /meta                            the engine's version and each layer's meta: a grid's options and ranges, a
#                                    vector layer's count
#   /tiles/<id>/{z}/{x}/{y}.pbf      a grid or vector layer's tile; a grid's query parameters are checked against
#                                    its filters before its tile function sees them
#   /tiles/<id>/{z}/{x}/{y}.png      a dem or contour layer's tile, an image of elevations (.webp for a WebP one)
#   /data/<id>.geojson               a geojson layer's data
#   /fit/<id>?bbox=w,s,e,n           a dem layer's floor and cap for the values in a box, from its colour's `fit`
#   /time?v=<vessel>&span=<span>     the time panel's series of one vessel's span (rr_time()), as JSON; the vessel
#                                    and the span are checked against the panel's before R sees them
#   /notes                           with rr_serve(notes = ): the notes (GET), a note or a reply added (POST),
#   /notes/<id>                      a note deleted (DELETE), each answered with the notes; only for a page opened
#                                    from this computer, a write only from the page itself
#   /verdicts                        with rr_serve(verdicts = ): the features' current verdicts (GET), a verdict
#                                    given or withdrawn (POST), answered with them; as the notes, from this computer
#                                    and the page only


.rr_map_engine <- 7L # the page's ENGINE: changed together

.rr_maps <- new.env(parent = emptyenv()) # the servers rr_serve() started, by port

.rr_tiles <- new.env(parent = emptyenv())

.rr_tiles$served <- list() # by port, the temporary tables (connection and name) of the map served there

#' A layer of a map
#'
#' One layer of an [rr_map()]: what it shows, how it is drawn, and where its data come from. Five kinds:
#'
#' * `"grid"`: vector tiles of cells, coloured in the page by a numeric field on a log scale, with the palette,
#'   stretch, floor, cap and histogram there too. Its `filters` become controls whose values go into the tile address
#'   (one grid per map).
#' * `"vector"`: vector tiles of features, drawn by `style`, such as a table of geometries served by
#'   [rr_tiles_table()].
#' * `"geojson"`: features sent whole as GeoJSON, drawn by `style`.
#' * `"dem"`: image tiles of elevations (Terrarium or Mapbox Terrain-RGB), such as an MBTiles file read by
#'   [rr_tiles_mbtiles()], drawn by `style` in the page under the base map's labels: MapLibre's `hillshade` and
#'   `color-relief` layers, so the shading and the colours are made in the browser.
#' * `"contour"`: contour lines made in the browser from the same image tiles of elevations, by `maplibre-contour`
#'   (loaded by the page only for a map that has one), at the intervals of `contours`, drawn by `style` (a `line`, and
#'   a `symbol` for the labels, from the source layer `"contours"`, its features' `ele` and `level`) under the base
#'   map's labels.
#'
#' Texts (`label`, `info`, a filter's `label`, a legend's `label`, ...) are keys of the map's `texts` ([rr_map()]), or
#' the text itself where no key matches. MapLibre styles and expressions are lists, so that a one-element array stays
#' an array in JSON: `list("get", "kind")`, `list("zoom")`; read from JSON with `jsonlite::fromJSON(simplifyVector =
#' FALSE)` they already are.
#'
#' @param id The layer's name, lower case: its checkbox, its source and its key in the page's address.
#' @param kind `"grid"`, `"vector"`, `"geojson"`, `"dem"` or `"contour"`.
#' @param label,info Its name in the sidebar and the text behind its ⓘ.
#' @param visible Whether it is on when the page opens; the address turns it off with `id=0` or on with `id=1`.
#' @param tiles The tile function. For a grid, `function(z, x, y, params)`, `params` a named list of the filters'
#'   values as text (absent when not given); for a vector layer, `function(z, x, y)`. It returns a Mapbox vector tile
#'   as raw bytes (DuckDB's `ST_AsMVT()`), or `raw(0)`. A dem layer's, `function(z, x, y)`, returns an image of
#'   elevations (PNG or WebP) as raw bytes, or `NULL` where there is none; a contour layer's the same.
#' @param meta For a grid, a list or a function returning one, read when the server starts: `options` (per filter
#'   with `meta = TRUE`, a data frame of `value` and, optionally, `name` and `code`), `ranges` (per range filter, its
#'   first and last value) and `v` (a version, put into the tile address so a rebuilt source is not hidden by the
#'   browser's cache).
#' @param data For a geojson layer: GeoJSON text, an sf object, or a function returning either, read when the server
#'   starts.
#' @param style A list of MapLibre layer specifications, without `source` (the layer's own).
#' @param popup For a vector or geojson layer, what a click shows: `click` (the style ids that answer), `title` (a
#'   field), `fields` (each a list of `field`, `label`, and optionally `values`, a value-to-text lookup whose text may
#'   hold `{field}` placeholders; `template`, a text with `{field}` placeholders shown in place of the value when there
#'   is one; and `format`, `"value"` or `"count"`), `maxWidth`. For a grid, `fields` only, the first in bold. Opt-in,
#'   a label on hover: `hover`, the style ids whose features show their `title` in a light label beside the pointer
#'   while it is over them (`TRUE` for those of `click`), and `hover_fields`, `TRUE` to show the `fields` there too.
#'   Over features of several layers, the topmost speaks, as for a click; a click hides the label until the pointer is
#'   over another feature. Opt-in, a verdict on each feature, kept by a server started with `rr_serve(verdicts = )`:
#'   `verdict`, a list of `field` (the field naming a feature: a verdict belongs to its value), `questions` (one to
#'   four, each an `id`, a `label`, two to six `options` and optionally `skip`), and `text` (`TRUE` for a note on the
#'   feature, in the reader's words). An option is a `value` (letters, digits, `_` or `-`), a `label` (a text or a key
#'   of the map's `texts`), optionally a `colour` for the feature's mark (the first green by default, the others red),
#'   and `other = TRUE` for an answer that names another feature: a box for its name, the names in view offered.
#'   `skip` names earlier questions and the answers that leave this one out (`list(name = "not")`). One question may be
#'   given as `options` alone. `store`, another layer's id, keeps the answers under that layer's and shows its answers
#'   (two layers of one kind of feature: a feature's answer the same whichever it is clicked on); the two must ask the
#'   same questions. The click's popup then holds the questions, a button for each answer; each answer is
#'   written to the server's file with the feature's place, and a mark drawn on the feature (the first question's
#'   colour its fill, the second's its ring). A later answer replaces the earlier; [rr_verdicts()] reads them.
#' @param legend A list of swatches, each `swatch` (`fill` and `stroke`, or `dot`) and `label`.
#' @param variants For a vector, geojson or dem layer, styles to choose between in the page, such as the column the
#'   points are coloured by: `label` (the control's text) and `options`, each a `value`, a `label`, `paint` (by the
#'   layer's style ids, the paint properties it sets) and optionally `legend` (in place of the layer's). The first
#'   option is shown when the page opens; a choice goes into the address as `<id>_by`, and nothing is fetched again.
#' @param count The text beside its label: `text` (a key whose `{1}` is the count) and, for geojson, `where` (a field
#'   and the value of the features to count). A vector layer's count is its tile function's `count` attribute.
#' @param missing The text shown when the server has none of it.
#' @param filters For a grid: a list of controls, each a list with `id`, `label` and one of: `options` (a list of
#'   `value`, `label` and optionally `tip`) with an optional `default`; `meta = TRUE` (options from `meta`, with an
#'   optional `unnamed` text for an option with no name; several values, comma separated, may come from the address);
#'   `type = "range"` with `params` (two names, such as `c("y1", "y2")`, its bounds from `meta`); `type = "metric"`
#'   (the choice among `metrics`).
#' @param metrics For a grid: a list of the numeric fields it can be coloured by, each `field` and `label`.
#' @param source_layer For a grid, the tile layer its cells are in (`"cells"`).
#' @param minzoom,maxzoom The zooms its tiles are made for.
#' @param opacity For a grid, its opacity when the page opens, 0-100.
#' @param encoding,tile_size,bounds For a dem or contour layer: how its images hold the elevation (`"terrarium"` or `"mapbox"`),
#'   their side in pixels, and the extent that has tiles (west, south, east, north), so the page asks for no other.
#'   Taken, with `minzoom` and `maxzoom`, from the attributes of `tiles` where not given ([rr_tiles_mbtiles()]
#'   sets them); else `"terrarium"` and 512. The image format (`"png"` or `"webp"`) is the `format` attribute's.
#' @param shade For a dem layer with a `hillshade` style: the relief's controls in the page, and their values when it
#'   opens, a list of any of `method` (`"standard"`, `"basic"`, `"combined"`, `"igor"` or `"multidirectional"`, three
#'   lights 45 degrees apart), `direction` (the light's azimuth, 0-359), `altitude` (0-90), `exaggeration` (the
#'   intensity, 0-1), `opacity` (0-100: the alpha of the shading's colours, which has no opacity of its own),
#'   `shadow` and `highlight` (colours as `"#rrggbb"`); MapLibre's own defaults for those not given, but an intensity of
#'   0.6 and a dark blue-grey shadow, `"#1b2a3a"`, as a bathymetry map in use settled on. `TRUE` takes
#'   every default. Each control applies as it moves, and goes into the page's address as `<id>_method`, `<id>_dir`,
#'   `<id>_alt`, `<id>_ex`, `<id>_op`, `<id>_shadow` and `<id>_light`. With `fit`, `TRUE` or a function of the view's
#'   box as a colour's `fit` returning a list with `exaggeration` (0-1), the intensity can follow the view (`auto`,
#'   `TRUE` by default; `<id>_auto` in the address, which leaves out `<id>_ex` while it follows), until it is set by
#'   hand. Tiles from [rr_tiles_dem()] are fitted in the page when no fit is given.
#' @param colour For a dem layer with a `color-relief` style: its values in colour, with their controls in the page, a
#'   list of any of `of` (`"depth"`, the elevation's negative, or `"elevation"`; what is 0 or less is clear),
#'   `palettes` (the ids of the palettes its control offers, of the map's palettes) and `palette` (the one it opens
#'   on), `floor` and `cap` (the values the palette's ends stand at, 1-10,000, set on one slider of two handles),
#'   `scale` (`"log"`, `"linear"` or, with a fit, `"equal"`), `mode` (`"ramp"` or `"bands"`), `bands` (the bands'
#'   breaks, rising), `cuts` (the cuts offered, 0 for none) and `cut` (one of them: values under it are drawn as
#'   `cut_as`, `"clear"` or `"grey"`), `opacity` (0-100). Defaults: depth; for a depth the seven looks of
#'   [rr_palettes()] (quiet, seabed, deep, chart, grey, drained and haxby), for an elevation every palette of the
#'   map; the first of them; floor 10, cap 2,000; in equal shares with a fit, else log; a ramp; bands at 10, 20, 50,
#'   100, 200, 500, 1,000 and 2,000; cuts 0, 10, 20 and 50, no cut, clear; 100. `TRUE` takes them all. Another
#'   palette, such as `"magma"`, is offered by naming it, `colour = list(palettes = c("quiet", "magma"))` (a `palette`
#'   not among `palettes` is put first in them), and one of your own by adding it to the map's,
#'   `rr_map(palettes = c(rr_palettes(), list(list(id = "mine", label = "mine", colors = <colours>))))`. Each control
#'   applies as it moves, and goes into the page's address as `<id>_pal`, `<id>_lo`, `<id>_hi`, `<id>_scale`,
#'   `<id>_mode`, `<id>_cut`, `<id>_cutas` and `<id>_cop`. With `fit`, the page has a "Fit to view" button and follows
#'   the view: it fits again each time the map settles, until a floor or cap is set by hand (`auto`, `TRUE` by default;
#'   `<id>_auto` in the address, where the floor and cap are left out while it follows). `fit = TRUE` makes the fit in
#'   the page from the layer's tiles in view, with no call to R (by the rules of [rr_bundle()]'s `fit`): the floor
#'   and the cap and the values' quantiles, which also offer `scale` `"equal"`, each colour an equal share of the values
#'   in view between the floor and the cap. Or `fit` is a function of the view's `west`, `south`, `east` and `north`,
#'   answered by the server at `/fit/<id>?bbox=w,s,e,n`, returning the floor and the cap for the values there (or
#'   `NULL` where there are none; or a list of `floor`, `cap` and `quantiles`, the values' quantiles rising); a bundle
#'   fits in the page all the same. Tiles from [rr_tiles_dem()] are fitted in the page when no fit is given.
#' @param contours For a contour layer: `thresholds`, a named list by zoom of the minor and the major interval from that
#'   zoom on (`list("6" = c(500, 1000), "9" = c(50, 200))`; a major line has `level` 1), and `multiplier`, by which the
#'   elevations are multiplied before the lines are drawn (-1 for depths, 1 by default). With `fit`, `TRUE` or a
#'   function as a colour's `fit` returning a list with the `quantiles` of the values in view, the lines can follow the
#'   view (`auto`,
#'   `TRUE` by default; `<id>_auto` in the address): about a dozen levels at round values near the view's quantiles,
#'   so the lines fall where the area is, drawn at a step of 1, 2 or 5 times a power of ten that makes some 40 of the
#'   view's range; unticked, the `thresholds` and the style's filters. Tiles from [rr_tiles_dem()] are fitted in the
#'   page when no fit is given (by depth with a negative `multiplier`, else by elevation).
#' @param state For a vector, geojson, dem or contour layer: values in the reader's hands that its `style` reads, in
#'   a filter, a paint or a layout property, as `list("global-state", "<id>_<name>")` (MapLibre's global state), such
#'   as whether a style is drawn, its colour, or a shift of the zoom it starts at. A list of controls, each with `id`
#'   (`<name>` above: lower-case letters and digits) and `label`, and either `options` (a list of `value`, a single
#'   number, text or `TRUE`/`FALSE`, and `label`), a list to choose from, with `default` one of the values (the first
#'   when not given); or no `options` and `default` `TRUE` or `FALSE`, a checkbox. Each applies as it changes, nothing
#'   fetched again, and goes into the page's address as `<id>_<name>`. Every one must be read by the style.
#' @return An `rr_layer`, for [rr_section()].
#' @seealso [rr_map()], [rr_tiles_table()]
#' @export
#' @examples
#' # A vector layer: the harbours as areas, a click shows the harbour's id
#' \donttest{
#' harbours <- rr_layer(
#'   "harbours", "vector", label = "Harbours",
#'   tiles = rr_tiles_table(rr_example("dansk_harbours"), properties = "SI_HARB"),
#'   style = list(list(id = "harbours-fill", type = "fill", "source-layer" = "features",
#'                     paint = list("fill-color" = "#d9822b"))),
#'   popup = list(click = "harbours-fill", title = "SI_HARB")
#' )
#' }
#'
#' # A grid: its tile function is given the filters' values in `params`. This one returns an
#' # empty tile; a real one returns DuckDB's ST_AsMVT() of the cells (see the "Maps in RStudio"
#' # article).
#' cells <- rr_layer(
#'   "cells", "grid", label = "Pings by cell",
#'   tiles = function(z, x, y, params) raw(0),
#'   filters = list(
#'     list(id = "behaviour", label = "Behaviour",
#'          options = list(list(value = "", label = "all"),
#'                         list(value = "Fishing", label = "fishing"))),
#'     list(id = "metric", type = "metric", label = "Metric")
#'   ),
#'   metrics = list(list(field = "n", label = "pings"))
#' )
#'
#' # A geojson layer: features sent whole, here one point
#' point <- rr_layer(
#'   "point", "geojson", label = "A point",
#'   data = '{"type": "FeatureCollection", "features": [{"type": "Feature", "properties": {},
#'            "geometry": {"type": "Point", "coordinates": [12.6, 56.0]}}]}',
#'   style = list(list(id = "point-dot", type = "circle", paint = list("circle-color" = "#c0392b")))
#' )
#' names(cells$config)
rr_layer <- function(
  id,
  kind = c("grid", "vector", "geojson", "dem", "contour"),
  label = id,
  info = NULL,
  visible = TRUE,
  tiles = NULL,
  meta = NULL,
  data = NULL,
  style = NULL,
  popup = NULL,
  legend = NULL,
  variants = NULL,
  count = NULL,
  missing = NULL,
  filters = NULL,
  metrics = NULL,
  source_layer = NULL,
  minzoom = NULL,
  maxzoom = NULL,
  opacity = NULL,
  encoding = NULL,
  tile_size = NULL,
  bounds = NULL,
  shade = NULL,
  colour = NULL,
  contours = NULL,
  state = NULL
) {
  kind <- match.arg(kind)
  if (!is.character(id) || length(id) != 1 || !grepl("^[a-z][a-z0-9_]*$", id)) {
    stop(
      "`id` is one name in lower case: letters, digits and _, starting with a letter.",
      call. = FALSE
    )
  }
  if (
    id %in%
      c(
        "metric",
        "palette",
        "stretch",
        "floor",
        "cap",
        "opacity",
        "base",
        "lang",
        "v"
      )
  ) {
    stop(
      sprintf("`id` \"%s\" is a name the page's address uses for itself.", id),
      call. = FALSE
    )
  }
  if (kind %in% c("grid", "vector", "dem", "contour") && !is.function(tiles)) {
    stop(
      sprintf(
        "A %s layer needs `tiles`, a function returning %s.",
        kind,
        if (kind %in% c("dem", "contour")) "an image tile" else "a vector tile"
      ),
      call. = FALSE
    )
  }
  if (kind == "grid" && !length(metrics)) {
    stop(
      "A grid layer needs `metrics`: the fields it can be coloured by.",
      call. = FALSE
    )
  }
  if (kind == "geojson" && is.null(data)) {
    stop("A geojson layer needs `data`.", call. = FALSE)
  }
  if (kind != "grid" && !length(style)) {
    stop(
      sprintf("A %s layer needs `style`: MapLibre layer specifications.", kind),
      call. = FALSE
    )
  }
  if (!is.null(variants)) {
    if (kind == "grid") {
      stop(
        "`variants` are for a vector, geojson or dem layer; a grid is coloured by its `metrics`.",
        call. = FALSE
      )
    }
    style_ids <- vapply(style, function(s) if (is.null(s$id)) "" else s$id, "")
    one <- function(o) {
      is.character(o$value) &&
        length(o$value) == 1 &&
        is.list(o$paint) &&
        length(o$paint) > 0 &&
        all(names(o$paint) %in% style_ids)
    }
    if (
      !length(variants$options) ||
        !all(vapply(variants$options, one, logical(1)))
    ) {
      stop(
        "`variants` is a list of `label` and `options`, each option a `value`, a `label` and `paint`: by the layer's style ids, the paint properties it sets.",
        call. = FALSE
      )
    }
    if (anyDuplicated(vapply(variants$options, `[[`, "", "value"))) {
      stop("The `options` of `variants` need distinct values.", call. = FALSE)
    }
    variants$options <- lapply(variants$options, function(o) {
      if (is.null(o$label)) {
        o$label <- o$value
      }
      o
    })
  }
  if (!is.null(popup[["hover"]]) || !is.null(popup[["hover_fields"]])) {
    popup <- .rr_map_hover(popup, kind, style)
  }
  if (!is.null(popup[["verdict"]])) {
    popup <- .rr_map_verdict_spec(popup, kind)
  }
  # one-element arrays stay arrays in the config's JSON
  if (!is.null(popup$click)) {
    popup$click <- as.list(popup$click)
  }
  if (!is.null(count$where)) {
    count$where <- as.list(count$where)
  }
  filters <- lapply(filters, function(f) {
    if (identical(f$type, "range")) {
      f$params <- as.list(f$params)
    }
    f
  })
  format <- NULL
  if (kind %in% c("dem", "contour")) {
    given_or_attr <- function(given, name, usual = NULL) {
      if (!is.null(given)) {
        given
      } else if (!is.null(attr(tiles, name))) {
        attr(tiles, name)
      } else {
        usual
      }
    }
    encoding <- given_or_attr(encoding, "encoding", "terrarium")
    format <- given_or_attr(NULL, "format", "png")
    if (!encoding %in% c("terrarium", "mapbox") || !format %in% c("png", "webp")) {
      stop(
        "A dem or contour layer's `encoding` is \"terrarium\" or \"mapbox\", its tiles' format \"png\" or \"webp\".",
        call. = FALSE
      )
    }
    tile_size <- as.integer(given_or_attr(tile_size, "tile_size", 512L))
    bounds <- given_or_attr(bounds, "bounds")
    if (!is.null(bounds)) {
      bounds <- as.list(as.numeric(bounds))
    }
    minzoom <- given_or_attr(minzoom, "minzoom")
    maxzoom <- given_or_attr(maxzoom, "maxzoom")
  } else if (!is.null(encoding) || !is.null(tile_size) || !is.null(bounds)) {
    stop("`encoding`, `tile_size` and `bounds` are for a dem or contour layer.", call. = FALSE)
  }
  # a colour's, the shading's or the contours' fit: "page", made in the page from the tiles in view, or a function kept
  # on the server, the page told only that there is one
  fit <- NULL
  # the tiles' own fit (rr_tiles_dem()'s "page", or a function), taken where the layer gives none, by a depth's values or
  # an elevation's
  tiles_fit <- if (kind %in% c("dem", "contour")) {
    af <- attr(tiles, "fit")
    if (is.function(af) || identical(af, "page")) af
  }
  from_tiles <- function(of) {
    if (identical(tiles_fit, "page")) {
      return("page")
    }
    force(of)
    function(west, south, east, north) tiles_fit(west, south, east, north, of = of)
  }
  # a fit given: TRUE, the page's from the tiles in view; or a function of the view's box, the server's
  given_fit <- function(f, what) {
    if (isTRUE(f)) {
      return("page")
    }
    if (!is.function(f)) {
      stop(
        sprintf("`%s$fit` is TRUE (made in the page from the tiles in view) or a function of west, south, east and north.", what),
        call. = FALSE
      )
    }
    f
  }
  if (kind == "contour") {
    if (is.list(contours) && !is.null(contours$fit)) {
      fit <- given_fit(contours$fit, "contours")
      contours$fit <- NULL
    }
    contours <- .rr_map_contours(contours)
    if (is.null(fit) && !is.null(tiles_fit)) {
      fit <- from_tiles(if (contours$multiplier < 0) "depth" else "elevation")
    }
    if (!is.null(fit)) {
      contours$fit <- TRUE
    }
  } else if (!is.null(contours)) {
    stop("`contours` is for a contour layer.", call. = FALSE)
  }
  if (is.list(shade) && !is.null(shade$fit)) {
    fit <- given_fit(shade$fit, "shade")
    shade$fit <- NULL
  }
  if (is.list(colour) && !is.null(colour$fit)) {
    if (!is.null(fit)) {
      stop("A layer has one fit: give the colour's and the shading's to two layers on the same tiles.", call. = FALSE)
    }
    fit <- given_fit(colour$fit, "colour")
    colour$fit <- NULL
  }
  if (is.null(fit) && !is.null(tiles_fit) && kind == "dem" && (!is.null(shade) || !is.null(colour))) {
    fit <- from_tiles(if (is.list(colour) && identical(colour$of, "elevation")) "elevation" else "depth")
  }
  if (!is.null(shade)) {
    shade <- .rr_map_shade(shade, kind, style)
    if (!is.null(fit)) {
      shade$fit <- TRUE
    }
  }
  if (!is.null(colour)) {
    colour <- .rr_map_colour(colour, kind, style, fitted = !is.null(fit))
    if (!is.null(fit)) {
      colour$fit <- TRUE
    } else if (identical(colour$scale, "equal")) {
      stop("`colour$scale` \"equal\" needs `colour$fit`, which gives the values' quantiles in view.", call. = FALSE)
    }
  }
  if (!is.null(state)) {
    state <- .rr_map_state(state, id, kind, style)
  }
  config <- list(
    id = id,
    kind = kind,
    label = label,
    info = info,
    visible = isTRUE(visible),
    style = style,
    popup = popup,
    legend = legend,
    variants = variants,
    count = count,
    missing = missing,
    filters = filters,
    metrics = metrics,
    source_layer = source_layer,
    minzoom = minzoom,
    maxzoom = maxzoom,
    opacity = opacity,
    encoding = encoding,
    tile_size = tile_size,
    bounds = bounds,
    format = format,
    shade = shade,
    colour = colour,
    contours = contours,
    state = state,
    fit = if (identical(fit, "page")) "page"
  )
  structure(
    list(
      config = Filter(Negate(is.null), config),
      tiles = tiles,
      meta = meta,
      data = data,
      fit = fit
    ),
    class = "rr_layer"
  )
}

# A popup's verdict, checked: a field naming the feature; one to four questions, each an id, a label, two to six
# options (a value, a label, a colour, and `other` for an answer that names another feature) and the answers of
# earlier questions that skip it; and whether a note goes with the feature. The single question of 2026.11.09
# (`options` with no `questions`) is the question "verdict". Lists stay arrays in the config's JSON
.rr_map_verdict_spec <- function(popup, kind) {
  if (!kind %in% c("vector", "geojson")) {
    stop("`popup$verdict` is for a vector or geojson layer.", call. = FALSE)
  }
  if (is.null(popup[["click"]])) {
    stop("`popup$verdict` is given in a click's popup: give `popup$click`.", call. = FALSE)
  }
  v <- popup[["verdict"]]
  if (!is.list(v) || !is.character(v$field) || length(v$field) != 1 || !nzchar(v$field)) {
    stop("`popup$verdict$field` is the field that names a feature.", call. = FALSE)
  }
  if (!is.null(v$questions) && !is.null(v$options)) {
    stop("`popup$verdict` has `options` (one question) or `questions`, not both.", call. = FALSE)
  }
  qs <- if (is.null(v$questions)) list(list(id = "verdict", options = v$options)) else v$questions
  if (!is.list(qs) || length(qs) < 1 || length(qs) > 4) {
    stop("`popup$verdict$questions` is one to four questions, each a list of `id`, `label` and `options`.", call. = FALSE)
  }
  ok_id <- function(x) is.character(x) && length(x) == 1 && grepl("^[A-Za-z0-9_-]{1,40}$", x)
  seen <- list()
  qs <- lapply(qs, function(q) {
    if (!is.list(q) || !ok_id(q$id) || q$id == "note") {
      stop("A verdict's question has an `id`: 1-40 letters, digits, `_` or `-`, not \"note\" (the note's).", call. = FALSE)
    }
    if (q$id %in% names(seen)) {
      stop("A verdict's questions have ids of their own.", call. = FALSE)
    }
    if (!is.null(q$label) && !(is.character(q$label) && length(q$label) == 1)) {
      stop("A verdict's question's `label` is one text (or a key of the map's texts).", call. = FALSE)
    }
    o <- q$options
    if (!is.list(o) || length(o) < 2 || length(o) > 6) {
      stop("A verdict's question has two to six `options`, each a list of `value` and `label`.", call. = FALSE)
    }
    o <- lapply(o, function(x) {
      if (!is.list(x) || !ok_id(x$value)) {
        stop("A verdict's option has a `value`: 1-40 letters, digits, `_` or `-`.", call. = FALSE)
      }
      if (is.null(x$label)) {
        x$label <- x$value
      }
      if (!is.character(x$label) || length(x$label) != 1) {
        stop("A verdict's option's `label` is one text (or a key of the map's texts).", call. = FALSE)
      }
      if (!is.null(x$colour) && !(is.character(x$colour) && length(x$colour) == 1 && grepl("^#[0-9A-Fa-f]{6}$", x$colour))) {
        stop("A verdict's option's `colour` is one colour, as \"#1a9850\".", call. = FALSE)
      }
      if (!is.null(x$other) && !(is.logical(x$other) && length(x$other) == 1 && !is.na(x$other))) {
        stop("A verdict's option's `other` is TRUE or FALSE: whether the answer names another feature.", call. = FALSE)
      }
      x$other <- isTRUE(x$other)
      x
    })
    values <- vapply(o, `[[`, "", "value")
    if (anyDuplicated(values)) {
      stop("A verdict's options have values of their own.", call. = FALSE)
    }
    skip <- q$skip
    if (!is.null(skip)) {
      if (!is.list(skip) || is.null(names(skip)) || any(!names(skip) %in% names(seen))) {
        stop("A verdict's question's `skip` names earlier questions, each with the answers that skip it.", call. = FALSE)
      }
      for (k in names(skip)) {
        if (!is.character(unlist(skip[[k]])) || !all(unlist(skip[[k]]) %in% seen[[k]])) {
          stop(sprintf("A verdict's question's `skip` gives answers of the question \"%s\": %s.", k,
                       paste(seen[[k]], collapse = ", ")), call. = FALSE)
        }
      }
      skip <- lapply(skip, function(x) as.list(unlist(x)))
    }
    seen[[q$id]] <<- values
    Filter(Negate(is.null), list(id = q$id, label = q$label, options = o, skip = skip))
  })
  if (!is.null(v$text) && !(is.logical(v$text) && length(v$text) == 1 && !is.na(v$text))) {
    stop("`popup$verdict$text` is TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(v$store) && !(is.character(v$store) && length(v$store) == 1 && grepl("^[A-Za-z0-9_-]{1,60}$", v$store))) {
    stop("`popup$verdict$store` is one id (letters, digits, `_` or `-`): the answers it shares, another layer's.", call. = FALSE)
  }
  popup[["verdict"]] <- Filter(Negate(is.null), list(field = v$field, questions = qs, text = isTRUE(v$text), store = v$store))
  popup
}

# A popup's hover, checked: style ids of the layer's own (TRUE for the click's), a title to show, hover_fields a flag
.rr_map_hover <- function(popup, kind, style) {
  if (!kind %in% c("vector", "geojson")) {
    stop("`popup$hover` is for a vector or geojson layer.", call. = FALSE)
  }
  if (is.null(popup[["hover"]])) {
    stop("`popup$hover_fields` goes with `popup$hover`, the style ids that show a label on hover.", call. = FALSE)
  }
  if (isTRUE(popup[["hover"]])) {
    if (is.null(popup[["click"]])) {
      stop("`popup$hover = TRUE` takes the style ids of `popup$click`: give `click`, or the ids to `hover`.", call. = FALSE)
    }
    popup[["hover"]] <- popup[["click"]]
  }
  ids <- unlist(popup[["hover"]])
  style_ids <- vapply(style, function(s) if (is.null(s$id)) "" else s$id, "")
  if (!is.character(ids) || !length(ids) || anyNA(ids) || !all(ids %in% style_ids)) {
    stop(
      "`popup$hover` is some of the layer's style ids (or TRUE for those of `click`): their features show a label on hover.",
      call. = FALSE
    )
  }
  if (!is.character(popup[["title"]]) || length(popup[["title"]]) != 1) {
    stop("`popup$hover` shows a feature's `title`: give `popup$title`, a field.", call. = FALSE)
  }
  hf <- popup[["hover_fields"]]
  if (!is.null(hf) && !(is.logical(hf) && length(hf) == 1 && !is.na(hf))) {
    stop("`popup$hover_fields` is TRUE or FALSE.", call. = FALSE)
  }
  popup[["hover"]] <- as.list(unique(ids))
  popup
}

#' A group of layers in a map's sidebar
#'
#' @param id The group's name.
#' @param title Its title in the sidebar: a key of the map's `texts`, or the text itself.
#' @param layers A list of [rr_layer()]s, in the order they are listed and drawn (the later over the earlier).
#' @param open Whether the group is unfolded when the page opens.
#' @return An `rr_section`, for [rr_map()].
#' @export
#' @examples
#' point <- rr_layer(
#'   "point", "geojson", label = "A point",
#'   data = '{"type": "Point", "coordinates": [12.6, 56.0]}',
#'   style = list(list(id = "point-dot", type = "circle"))
#' )
#' ref <- rr_section("ref", "Reference", list(point), open = FALSE)
#' ref$title
rr_section <- function(id, title = id, layers, open = TRUE) {
  if (inherits(layers, "rr_layer")) {
    layers <- list(layers)
  }
  if (!all(vapply(layers, inherits, logical(1), "rr_layer"))) {
    stop("`layers` is a list of rr_layer()s.", call. = FALSE)
  }
  structure(
    list(id = id, title = title, open = isTRUE(open), layers = layers),
    class = "rr_section"
  )
}

#' A map: layers in groups, texts, base maps and palettes
#'
#' The map is a page with a sidebar of groups that fold, each layer with its info and, when ticked, its controls and
#' legend under it; base maps to choose from; the whole view in the page's address, so a link gives the same view;
#' and on a phone the sidebar behind a menu button. [rr_serve()] serves it.
#'
#' Given data first, `rr_map(data)` starts a map that the layer functions add to ([rr_point()] and the others), the
#' data the layers' unless a layer brings its own.
#'
#' @param ... [rr_section()]s; or, alone, the data a map starts from (a parquet file, folder or glob, a lazy DuckDB
#'   table, a data frame or an sf object), which the layer functions ([rr_point()] and the others) draw.
#' @param title The page's title: a key of `texts`, or the text itself.
#' @param view The first view: `center` (longitude, latitude) and `zoom`.
#' @param texts The texts of the layers and groups, and of the page in other languages: a named list whose elements
#'   are a text, or a list of texts by language (`list(en = "Ports", is = "Hafnir")`). The page's own texts are
#'   English; give them in another language under the same keys, a base map's label under `b_<id>`.
#' @param languages The languages of the page, in the order of its language buttons (none for one language).
#' @param language The language the page opens in.
#' @param locales A list by language of the locales its numbers are written in, the first a browser has
#'   (`list(is = c("is-IS", "de-DE"))`, German standing in where a browser lacks Icelandic).
#' @param about Keys of `texts` for the paragraphs of the About group (none, no group).
#' @param choose Fields whose values the page offers to show, over every layer that has the field (each a control
#'   above the groups, with buttons that step through the values): a list with `fields`, each a `field`, a `label`,
#'   `multi` (`TRUE`: each value a chip ticked on and off, so several show at once; else a list, "all" first, to show
#'   one at a time), its `options` (each a `value` for the control and the address, a `label`, `match`, the value the
#'   layers' features carry, `NULL` for none, and optionally `bbox`, the extent the view is fitted to) and `layers`
#'   (the ids it filters); or, for a range, `range` (`"number"`, `"date"` or `"time"`), `closed` and its `bins`
#'   (each a `lo` and a `hi` as the features carry them, `from` and `to` as written, `n` and optionally `bbox`); and
#'   optionally `extents`, each a `key` (each field's value, or a range's bin from 0, joined by the character 31), `n`
#'   and a `bbox`, by which the fields are linked and the view fitted. [rr_map()] makes it from the columns its
#'   `choose` names. A choice goes into the address under the field's name, several values separated by commas, a
#'   range as `from..to`, and nothing is fetched again.
#'   For a map started from data, its columns written bare, `choose = c(id, year)`, made from the data and linked:
#'   text, a factor, a logical or a whole number (an id, a code, a year as an integer) is a list of its values (chips
#'   for up to 12); a fraction, a date or a time is a range with two handles, over each of its values where it has at
#'   most 64, else over bins between `pretty()` breaks. Where the data's dictionary (data-dict.tidyverse.org's, in the
#'   parquet file's metadata) gives a column's type, it decides: `number(id)`, `enum`, `string` or `boolean` a list,
#'   `number`, `number(quantity)`, `date` or `datetime` a range. A path is split by a range's bins, so a choice shows
#'   whole runs.
#' @param sections For a map started from data, the sidebar's boxes the layers name (their `section`), each a `title`
#'   and whether it is `open`: `list(Reference = list(open = FALSE))`.
#' @param palette The grid's palette when the page opens: an `id` of `palettes`.
#' @param palettes,basemaps The palettes and base maps to choose from: [rr_palettes()], [rr_basemaps()]. The
#'   grid offers the palettes for its use (`use` holding `"grid"`, or no `use`); a dem layer's colour offers its own
#'   list (`colour$palettes` of [rr_layer()]), by default the seven looks for a depth. A palette given again under
#'   an id replaces the earlier in its place, so `c(rr_palettes(), mine)` may redefine one.
#' @return An `rr_map`: `config` (what the page is sent) and `layers`.
#' @export
#' @examples
#' harbours <- rr_layer(
#'   "harbours", "vector", label = "Harbours",
#'   tiles = function(z, x, y) raw(0),
#'   style = list(list(id = "harbours-fill", type = "fill", "source-layer" = "features",
#'                     paint = list("fill-color" = "#d9822b")))
#' )
#' m <- rr_map(rr_section("ref", "Reference", list(harbours)), title = "Example",
#'             view = list(center = c(12.5, 56), zoom = 7))
#' names(m$config)
rr_map <- function(
  ...,
  title = "Map",
  view = list(center = c(0, 0), zoom = 2),
  texts = list(),
  languages = "en",
  language = languages[1],
  locales = NULL,
  about = NULL,
  choose = NULL,
  sections = NULL,
  palette = NULL,
  palettes = rr_palettes(),
  basemaps = rr_basemaps()
) {
  # a map started from data (the pipe): it holds the data until a layer function (rr_point() and the others)
  # adds the first layer; `choose` then names the data's columns, bare
  choose_q <- rlang::enquo(choose)
  first <- if (...length()) ..1
  is_data <- is.data.frame(first) || inherits(first, "tbl_lazy") ||
    (is.character(first) && length(first) == 1 && grepl("parquet|[*?]", first))
  if (is_data) {
    if (...length() > 1) {
      stop("rr_map(data) takes one table; its layers come from rr_point() and the other layer functions.", call. = FALSE)
    }
    return(structure(
      list(config = list(title = title), layers = list()),
      class = "rr_map",
      quick = list(pending = list(data = first, choose_q = choose_q, title = title, port = NULL, sections = sections))
    ))
  }
  choose <- rlang::eval_tidy(choose_q)
  groups <- list(...)
  if (
    !length(groups) ||
      !all(vapply(groups, inherits, logical(1), "rr_section"))
  ) {
    stop("`...` is one or more rr_section()s.", call. = FALSE)
  }
  layers <- unlist(lapply(groups, `[[`, "layers"), recursive = FALSE)
  ids <- vapply(layers, function(l) l$config$id, "")
  if (anyDuplicated(ids)) {
    stop(
      sprintf(
        "Layer ids are used twice: %s.",
        paste(unique(ids[duplicated(ids)]), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (
    sum(vapply(layers, function(l) l$config$kind == "grid", logical(1))) > 1
  ) {
    stop("A map has one grid layer at most.", call. = FALSE)
  }
  # a layer's state goes into the address and MapLibre's global state as <id>_<name>
  state_keys <- unlist(lapply(layers, function(l) {
    if (length(l$config$state)) paste0(l$config$id, "_", vapply(l$config$state, `[[`, "", "id"))
  }))
  if (length(clash <- intersect(state_keys, ids))) {
    stop(sprintf("A layer is named as another layer's state, %s: rename one.", paste(clash, collapse = ", ")),
         call. = FALSE)
  }
  # a palette given again under an id replaces the earlier, in its place: c(rr_palettes(), mine) redefines a look
  palette_ids <- vapply(palettes, `[[`, "", "id")
  if (anyDuplicated(palette_ids)) {
    last <- !duplicated(palette_ids, fromLast = TRUE)
    palettes <- lapply(which(!duplicated(palette_ids)), function(i) {
      palettes[[which(palette_ids == palette_ids[i] & last)]]
    })
    palette_ids <- vapply(palettes, `[[`, "", "id")
  }
  if (!is.null(palette) && !palette %in% palette_ids) {
    stop("`palette` is an id of `palettes`.", call. = FALSE)
  }
  # a dem layer's colours: the palettes its control offers, by default the depth's looks (every palette for an
  # elevation), with the one it opens on first if it is not among them
  for (i in seq_along(layers)) {
    co <- layers[[i]]$config$colour
    if (is.null(co)) {
      next
    }
    offered <- unlist(co$palettes)
    if (is.null(offered)) {
      offered <- if (identical(co$of, "elevation")) palette_ids else intersect(.rr_map_depth_looks, palette_ids)
      if (!length(offered)) {
        offered <- palette_ids
      }
    }
    if (!is.null(co[["palette"]]) && !co[["palette"]] %in% offered) {
      offered <- c(co[["palette"]], offered)
    }
    if (length(bad <- setdiff(offered, palette_ids))) {
      stop(
        sprintf(
          "Layer %s: its colour's palette%s %s %s not among the map's `palettes`: add %s to rr_map(palettes = ).",
          layers[[i]]$config$id,
          if (length(bad) > 1) "s" else "",
          paste(bad, collapse = ", "),
          if (length(bad) > 1) "are" else "is",
          if (length(bad) > 1) "them" else "it"
        ),
        call. = FALSE
      )
    }
    co$palettes <- as.list(offered)
    if (is.null(co[["palette"]])) {
      co[["palette"]] <- offered[[1]]
    }
    layers[[i]]$config$colour <- co
  }
  # the choices: the map's, over every layer with the field, and each layer's own, under its id in the address
  owns <- Filter(function(l) !is.null(l$config$choose), layers)
  if (!is.null(choose) || length(owns)) {
    taken <- c(
      ids,
      paste0(ids, "_by"),
      state_keys,
      "metric",
      "palette",
      "stretch",
      "floor",
      "cap",
      "opacity",
      "base",
      "lang",
      "v",
      unlist(lapply(layers, function(l) {
        unlist(lapply(l$config$filters, function(f) c(f$id, unlist(f$params))))
      })),
      unlist(lapply(layers, function(l) {
        c(
          if (!is.null(l$config$shade)) {
            paste0(l$config$id, "_", c("method", "dir", "alt", "ex", "op", "shadow", "light", "auto"))
          },
          if (!is.null(l$config$colour)) {
            paste0(l$config$id, "_", c("pal", "lo", "hi", "scale", "mode", "cut", "cutas", "cop", "auto"))
          },
          if (isTRUE(l$config$contours$fit)) {
            paste0(l$config$id, "_auto")
          }
        )
      }))
    )
  }
  if (!is.null(choose)) {
    choose <- .rr_choose_checked(choose, taken, "`choose`")
    taken <- c(taken, vapply(choose$fields, `[[`, "", "field"))
  }
  for (l in owns) {
    i <- match(l$config$id, ids)
    layers[[i]]$config$choose <- .rr_choose_checked(
      l$config$choose, taken, sprintf("Layer %s's `choose`", l$config$id), prefix = paste0(l$config$id, "_")
    )
  }
  names(layers) <- ids
  config <- list(
    title = title,
    view = list(center = as.list(view$center), zoom = view$zoom),
    languages = as.list(languages),
    language = language,
    locales = if (length(locales)) lapply(locales, as.list),
    about = if (length(about)) as.list(about),
    choose = choose,
    palette = palette,
    groups = lapply(groups, function(g) {
      list(
        id = g$id,
        title = g$title,
        open = g$open,
        layers = as.list(vapply(g$layers, function(l) l$config$id, ""))
      )
    }),
    layers = unname(lapply(layers, `[[`, "config")),
    basemaps = basemaps,
    palettes = lapply(palettes, function(p) {
      p$colors <- as.list(p$colors)
      if (!is.null(p$use)) {
        p$use <- as.list(p$use)
      }
      p
    }),
    texts = texts,
    # the rules of a fit made in the page (rr_bundle()'s defaults), where a layer's is
    fit = if (any(vapply(layers, function(l) identical(l$config$fit, "page"), NA))) .rr_map_bundle_fit(list())
  )
  structure(
    list(config = Filter(Negate(is.null), config), layers = layers),
    class = "rr_map"
  )
}

# A layer's state: controls whose values its style reads as MapLibre's global state, `<id>_<name>`; each checked, and
# each read by the style, so a control that changes nothing is not offered
.rr_map_state <- function(state, id, kind, style) {
  if (kind == "grid") {
    stop("`state` is for a vector, geojson, dem or contour layer: a grid is drawn by the page.", call. = FALSE)
  }
  shape <- "`state` is a list of controls, each a list with `id`, `label`, and `options` (each a `value` and a `label`) or a `default` TRUE or FALSE."
  if (!is.list(state) || !length(state) || !all(vapply(state, is.list, logical(1)))) {
    stop(shape, call. = FALSE)
  }
  # the names the page's address gives a layer's own controls
  own <- c("by", "method", "dir", "alt", "ex", "op", "shadow", "light", "pal", "lo", "hi", "scale", "mode", "cut",
           "cutas", "cop", "auto")
  scalar <- function(v) length(v) == 1 && !is.na(v) && (is.character(v) || is.numeric(v) || is.logical(v))
  read <- as.character(jsonlite::toJSON(style, auto_unbox = TRUE))
  state <- lapply(state, function(s) {
    if (!is.character(s$id) || length(s$id) != 1 || !grepl("^[a-z][a-z0-9]*$", s$id)) {
      stop("A state's `id` is lower-case letters and digits, starting with a letter.", call. = FALSE)
    }
    if (s$id %in% own) {
      stop(sprintf("A state's `id` \"%s\" is a name the page's address uses for a layer's own controls.", s$id),
           call. = FALSE)
    }
    key <- paste0(id, "_", s$id)
    if (!grepl(sprintf("[\"global-state\",\"%s\"]", key), read, fixed = TRUE)) {
      stop(sprintf("The state \"%s\" is read by none of the layer's styles: read it as list(\"global-state\", \"%s\").",
                   s$id, key), call. = FALSE)
    }
    if (is.null(s$label)) {
      s$label <- s$id
    }
    if (is.null(s$options)) {
      if (!isTRUE(s$default) && !isFALSE(s$default)) {
        stop(shape, call. = FALSE)
      }
      return(s)
    }
    if (!is.list(s$options) || !length(s$options) ||
          !all(vapply(s$options, function(o) is.list(o) && scalar(o$value), logical(1)))) {
      stop(shape, call. = FALSE)
    }
    values <- vapply(s$options, function(o) as.character(o$value), "")
    if (anyDuplicated(values)) {
      stop(sprintf("The options of the state \"%s\" need distinct values.", s$id), call. = FALSE)
    }
    if (is.null(s$default)) {
      s$default <- s$options[[1]]$value
    } else if (!scalar(s$default) || !as.character(s$default) %in% values) {
      stop(sprintf("The `default` of the state \"%s\" is one of its options' values.", s$id), call. = FALSE)
    }
    s$options <- lapply(s$options, function(o) {
      if (is.null(o$label)) {
        o$label <- as.character(o$value)
      }
      o
    })
    s
  })
  if (anyDuplicated(vapply(state, `[[`, "", "id"))) {
    stop("A layer's state ids are used once each.", call. = FALSE)
  }
  state
}

# A dem layer's relief controls: MapLibre's hillshade defaults for what is not given, but for the intensity and the
# shadow's colour, as a bathymetry map in use settled on (0.6 and a dark blue-grey); each value checked
.rr_map_shade <- function(shade, kind, style) {
  if (kind != "dem" || !any(vapply(style, function(s) identical(s$type, "hillshade"), logical(1)))) {
    stop("`shade` is for a dem layer with a `hillshade` style.", call. = FALSE)
  }
  given <- if (isTRUE(shade)) list() else shade
  known <- c("method", "direction", "altitude", "exaggeration", "opacity", "shadow", "highlight", "auto")
  if (!is.list(given) || (length(given) && (is.null(names(given)) || !all(names(given) %in% known)))) {
    stop(sprintf("`shade` is TRUE or a list of any of %s.", paste(known, collapse = ", ")), call. = FALSE)
  }
  v <- utils::modifyList(
    list(method = "standard", direction = 335, altitude = 45, exaggeration = 0.6, opacity = 100,
         shadow = "#1b2a3a", highlight = "#ffffff", auto = TRUE),
    given
  )
  num <- function(x, lo, hi) is.numeric(x) && length(x) == 1 && !is.na(x) && x >= lo && x <= hi
  hex <- function(x) is.character(x) && length(x) == 1 && grepl("^#[0-9A-Fa-f]{6}$", x)
  ok <- c(
    method = is.character(v$method) && length(v$method) == 1 &&
      v$method %in% c("standard", "basic", "combined", "igor", "multidirectional"),
    direction = num(v$direction, 0, 359),
    altitude = num(v$altitude, 0, 90),
    exaggeration = num(v$exaggeration, 0, 1),
    opacity = num(v$opacity, 0, 100),
    shadow = hex(v$shadow),
    highlight = hex(v$highlight),
    auto = is.logical(v$auto) && length(v$auto) == 1 && !is.na(v$auto)
  )
  if (!all(ok)) {
    stop(
      sprintf(
        "`shade`: %s out of range (method one of MapLibre's five; direction 0-359, altitude 0-90, exaggeration 0-1, opacity 0-100; colours \"#rrggbb\").",
        paste(names(ok)[!ok], collapse = ", ")
      ),
      call. = FALSE
    )
  }
  v$shadow <- tolower(v$shadow)
  v$highlight <- tolower(v$highlight)
  v
}

# A contour layer's intervals by zoom, as an object of the zoom's name, and its multiplier, each checked
.rr_map_contours <- function(contours) {
  t <- contours$thresholds
  z <- suppressWarnings(as.numeric(names(t)))
  one <- function(x) is.numeric(x) && length(x) %in% 1:2 && !anyNA(x) && all(x > 0) && !is.unsorted(x)
  if (
    !is.list(contours) || !all(names(contours) %in% c("thresholds", "multiplier", "auto")) || !is.list(t) || !length(t) ||
      anyNA(z) || any(z < 0 | z > 24 | z != round(z)) || anyDuplicated(z) || !all(vapply(t, one, logical(1)))
  ) {
    stop(
      "A contour layer's `contours` is a list of `thresholds`, by zoom (0-24, as names) the minor and major interval from that zoom on, both above 0, and optionally `multiplier`.",
      call. = FALSE
    )
  }
  m <- if (is.null(contours$multiplier)) 1 else contours$multiplier
  if (!is.numeric(m) || length(m) != 1 || is.na(m) || m == 0) {
    stop("`contours$multiplier` is a number other than 0.", call. = FALSE)
  }
  a <- if (is.null(contours$auto)) TRUE else contours$auto
  if (!is.logical(a) || length(a) != 1 || is.na(a)) {
    stop("`contours$auto` is TRUE or FALSE.", call. = FALSE)
  }
  list(thresholds = lapply(t[order(z)], as.list), multiplier = m, auto = a)
}

# A dem layer's colour controls: the defaults for what is not given, each value checked
.rr_map_colour <- function(colour, kind, style, fitted = FALSE) {
  if (kind != "dem" || !any(vapply(style, function(s) identical(s$type, "color-relief"), logical(1)))) {
    stop("`colour` is for a dem layer with a `color-relief` style.", call. = FALSE)
  }
  given <- if (isTRUE(colour)) list() else colour
  known <- c("of", "palette", "palettes", "floor", "cap", "scale", "mode", "bands", "cuts", "cut", "cut_as",
             "opacity", "auto")
  if (!is.list(given) || (length(given) && (is.null(names(given)) || !all(names(given) %in% known)))) {
    stop(sprintf("`colour` is TRUE or a list of any of %s.", paste(known, collapse = ", ")), call. = FALSE)
  }
  p <- given$palettes
  if (!is.null(p) && !(is.character(p) && length(p) > 0 && !anyNA(p) && !anyDuplicated(p))) {
    stop("`colour$palettes` is the ids of the palettes the colour offers, each once.", call. = FALSE)
  }
  # in equal shares where the view's quantiles are at hand (a fit), as a bathymetry map in use settled on
  v <- utils::modifyList(
    list(of = "depth", palette = NULL, floor = 10, cap = 2000, scale = if (fitted) "equal" else "log", mode = "ramp",
         bands = c(10, 20, 50, 100, 200, 500, 1000, 2000), cuts = c(0, 10, 20, 50), cut = 0, cut_as = "clear",
         opacity = 100, auto = TRUE),
    given
  )
  if (!is.null(v$palettes)) {
    v$palettes <- as.list(v$palettes)
  }
  one_of <- function(x, ok) is.character(x) && length(x) == 1 && x %in% ok
  num <- function(x, lo, hi) is.numeric(x) && length(x) == 1 && !is.na(x) && x >= lo && x <= hi
  rising <- function(x) is.numeric(x) && length(x) > 0 && !anyNA(x) && all(x >= 0) && !is.unsorted(x, strictly = TRUE)
  ok <- c(
    of = one_of(v$of, c("depth", "elevation")),
    palette = is.null(v[["palette"]]) || (is.character(v[["palette"]]) && length(v[["palette"]]) == 1),
    floor = num(v$floor, 1, 10000),
    cap = num(v$cap, 1, 10000),
    scale = one_of(v$scale, c("log", "linear", "equal")),
    mode = one_of(v$mode, c("ramp", "bands")),
    bands = rising(v$bands) && all(v$bands > 0),
    cuts = rising(v$cuts),
    cut = num(v$cut, 0, Inf) && v$cut %in% v$cuts,
    cut_as = one_of(v$cut_as, c("clear", "grey")),
    opacity = num(v$opacity, 0, 100),
    auto = is.logical(v$auto) && length(v$auto) == 1 && !is.na(v$auto)
  )
  if (!all(ok)) {
    stop(
      sprintf(
        "`colour`: %s out of range (of depth or elevation; floor and cap 1-10,000; scale log, linear or equal; mode ramp or bands; bands and cuts rising, the cut one of the cuts; cut_as clear or grey; opacity 0-100).",
        paste(names(ok)[!ok], collapse = ", ")
      ),
      call. = FALSE
    )
  }
  v$bands <- as.list(v$bands)
  v$cuts <- as.list(v$cuts)
  Filter(Negate(is.null), v)
}

#' Show a map
#'
#' At an interactive console a map is served when printed ([rr_serve()], on the port the call that started it gave,
#' else on 8790 or the next free port) and opens in RStudio's Viewer or a browser; elsewhere, as when a document is
#' knitted, its title and layers are printed.
#'
#' @param x An [rr_map()].
#' @param ... Not used.
#' @return `x`, invisibly.
#' @export
print.rr_map <- function(x, ...) {
  if (rlang::is_interactive()) {
    rr_serve(x, port = attr(x, "port"))
  } else {
    cat(format(x), sep = "\n")
  }
  invisible(x)
}

#' @rdname print.rr_map
#' @export
format.rr_map <- function(x, ...) {
  layers <- vapply(
    x$config$layers,
    function(l) sprintf("%s (%s)", l$id, l$kind),
    ""
  )
  tp <- x$config$time
  if (!length(layers) && !is.null(attr(x, "quick")$pending)) {
    return(c(sprintf("<rr_map> %s", .rr_or(x$config$title, "")), "  no layers yet: add them with rr_point() and the other layer functions"))
  }
  c(
    sprintf("<rr_map> %s", .rr_or(x$config$title, "")),
    sprintf("  layers: %s", paste(layers, collapse = ", ")),
    if (!is.null(tp)) {
      sprintf(
        "  time panel: %s over time, %d group%s%s%s",
        paste(vapply(tp$cols, `[[`, "", "field"), collapse = ", "),
        length(tp$vessels),
        if (length(tp$vessels) == 1) "" else "s",
        if (length(tp$lanes)) {
          sprintf("; lanes: %s", paste(vapply(tp$lanes, `[[`, "", "label"), collapse = ", "))
        } else {
          ""
        },
        if (!is.null(tp$span_label)) {
          sprintf(
            "; spans: %s (%s)",
            tp$span_label,
            format(
              sum(vapply(tp$vessels, function(v) sum(vapply(v$spans, `[[`, "", "value") != "all"), 1)),
              big.mark = ","
            )
          )
        } else {
          ""
        }
      )
    }
  )
}

.rr_or <- function(a, b) if (is.null(a)) b else a


# A `choose` checked and made ready for the config's JSON: each field a `field` and its `options`, its address key
# (the field's name, after `prefix` for a layer's own) not one the page uses already, and one-element arrays kept as
# arrays
.rr_choose_checked <- function(choose, taken, what, prefix = "") {
  one <- function(f) {
    is.character(f$field) && length(f$field) == 1 && (length(f$options) > 0 || length(f$bins) > 0)
  }
  if (!length(choose$fields) || !all(vapply(choose$fields, one, logical(1)))) {
    stop(
      what, " is a list of `fields`, each a `field`, its `options` (each a `value` and a `match`) or a range's ",
      "`bins` (each a `lo` and a `hi`), and its `layers`.",
      call. = FALSE
    )
  }
  clash <- intersect(paste0(prefix, vapply(choose$fields, `[[`, "", "field")), taken)
  if (length(clash)) {
    stop(
      sprintf(
        "%s has a field, %s, named as the page's address names a layer or a control.",
        what,
        paste(substring(clash, nchar(prefix) + 1), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  choose$fields <- lapply(choose$fields, function(f) {
    if (!is.null(f$layers)) {
      f$layers <- as.list(f$layers)
    }
    boxes <- function(os) {
      lapply(os, function(o) {
        if (!is.null(o$bbox)) {
          o$bbox <- as.list(o$bbox)
        }
        o
      })
    }
    if (!is.null(f$options)) {
      f$options <- boxes(f$options)
    }
    if (!is.null(f$bins)) {
      f$bins <- boxes(f$bins)
    }
    f
  })
  choose$extents <- lapply(choose$extents, function(e) {
    e$bbox <- as.list(e$bbox)
    e
  })
  choose
}
