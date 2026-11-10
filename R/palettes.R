# palettes.R — the palettes and base maps a map offers.
# Moved from ramb's R/map.R on 2026-11-10.

#' The palettes and base maps of a map
#'
#' `rr_palettes()`: eighteen palettes, each marked by `use` for the grid's colours, a depth's or both.
#'
#' * For the grid, twelve of nine colours, light (low) to dark (high). The first ten change in lightness steadily
#'   (OKLab), from viridisLite (magma, viridis, plasma) and tidyterra (deep, scotland, population_dens, etopo1_bathy,
#'   colombia_bathy, gn_yl, blues); the last two, terrain palettes, rise and fall in lightness.
#' * For a depth, seven looks, the colours a dem layer offers by default (its `colour$palettes`), shallow to deep:
#'   `quiet`, pale cyan-grey to mid grey-blue, low in chroma and stopping at mid lightness, so the relief's shading
#'   reads through it (the default); `seabed`, sand shading into mud seen through the water, a look and not a
#'   substrate; `deep`, the grid's too; `chart`, the nautical chart's convention, the shallows blue and the deeps near
#'   white; `grey`, a ground for other layers, pulling no overlay's hue; and two daring ones, `drained`, the seabed as
#'   if the sea were drained (GMT's globe hypsometry, the deeps green lowland, the banks brown hills), and `haxby`,
#'   marine geology's multibeam palette, orange shallows through green to deep blue. Each ends at least 0.03 in OKLab
#'   from the light base maps' land, so the shallowest water does not read as land. They were made for a
#'   bathymetry map of Icelandic waters (2026-11-09): quiet, seabed, chart and grey designed in OKLCh, drained and haxby resampled from
#'   tidyterra to 17 stops at even steps in OKLab, their whitest stops dropped.
#'
#' `rr_basemaps()`: base maps that show the sea floor, and none: EMODnet's sea floor (its 2020 base layer, the first
#' and the one a map opens on), the same darkened, EMODnet's lighter atlas (v12), and Esri's ocean, which reaches beyond
#' Europe's seas. Each is a picture with the place and sea names of a Carto style over it, Positron's on the light ones
#' and Dark Matter's on the dark. A base map that shows a flat sea is left out: the depth is the subject.
#'
#' @return A list, each element with `id` and `label`: a palette with `source`, `colors` and `use` (`"grid"`,
#'   `"depth"` or both); a base map with `style` (a MapLibre style's address) or `tiles` (a raster address) with
#'   `maxzoom` and `attribution`, and optionally `paint` (MapLibre's raster paint, such as `raster-brightness-max`),
#'   `background` (the colour beyond the picture's tiles), `labels` (a MapLibre style whose names are drawn over the
#'   picture, with its fonts) and `label_layers` (a regular
#'   expression for which of its layers: by default its symbol layers whose id starts `place_`, `watername_` or
#'   `waterway_label`), or neither (none). A palette of your own, given to [rr_map()]'s `palettes`, needs
#'   `id`, `label` and `colors`; with no `use` it is offered to the grid.
#' @export
#' @examples
#' vapply(rr_palettes(), `[[`, "", "id")
#' vapply(rr_basemaps(), `[[`, "", "id")
rr_palettes <- function() {
  p <- function(id, source, colors, label = id, use = "grid") {
    list(id = id, label = label, source = source, colors = colors, use = use)
  }
  list(
    p(
      "magma",
      "viridisLite",
      c(
        "#FCFDBF",
        "#FEC287",
        "#FB8861",
        "#E65164",
        "#B63679",
        "#822681",
        "#51127C",
        "#1D1147",
        "#000004"
      )
    ),
    p(
      "viridis",
      "viridisLite",
      c(
        "#FDE725",
        "#AADC32",
        "#5DC863",
        "#27AD81",
        "#21908C",
        "#2C728E",
        "#3B528B",
        "#472D7B",
        "#440154"
      )
    ),
    p(
      "plasma",
      "viridisLite",
      c(
        "#F0F921",
        "#FDC328",
        "#F89441",
        "#E56B5D",
        "#CC4678",
        "#A92395",
        "#7E03A8",
        "#4C02A1",
        "#0D0887"
      )
    ),
    p(
      "deep",
      "tidyterra whitebox",
      c(
        "#FEFED7",
        "#EBF7B1",
        "#C5E7B4",
        "#7ECCBB",
        "#40B5C3",
        "#1D90BF",
        "#215DA7",
        "#243493",
        "#081C57"
      ),
      use = c("grid", "depth")
    ),
    p(
      "scotland",
      "tidyterra princess",
      c(
        "#E6CCAD",
        "#A98066",
        "#6C3B27",
        "#493835",
        "#173442",
        "#0F2531",
        "#071720",
        "#051017",
        "#01060B"
      )
    ),
    p(
      "population_dens",
      "tidyterra grass",
      c(
        "#FFFFFF",
        "#AF6419",
        "#8B4010",
        "#83370E",
        "#7A2E0C",
        "#72250B",
        "#6A1C08",
        "#621104",
        "#5A0400"
      )
    ),
    p(
      "etopo1_bathy",
      "tidyterra hypso",
      c(
        "#BCE6FF",
        "#55D7FF",
        "#34BFFF",
        "#17A0FC",
        "#176EF2",
        "#0026DE",
        "#0010CD",
        "#1505AF",
        "#1A0089"
      )
    ),
    p(
      "colombia_bathy",
      "tidyterra hypso",
      c(
        "#C6ECFF",
        "#8FD5FF",
        "#53BCF2",
        "#1B9FD3",
        "#017FB3",
        "#015F92",
        "#013F72",
        "#002B5E",
        "#001E50"
      )
    ),
    p(
      "gn_yl",
      "tidyterra whitebox",
      c(
        "#FFFFCC",
        "#E7F6B2",
        "#CEEB9E",
        "#B3DF91",
        "#93D283",
        "#70C274",
        "#46AC5D",
        "#218C49",
        "#006837"
      )
    ),
    p(
      "blues",
      "tidyterra grass",
      c(
        "#FFFFFF",
        "#EEE2FF",
        "#DCC4FF",
        "#C8A8FF",
        "#B38BFF",
        "#9B6FFF",
        "#7E52FF",
        "#5932FF",
        "#0000FF"
      )
    ),
    p(
      "high_relief",
      "tidyterra whitebox",
      c(
        "#488737",
        "#96B170",
        "#E2DBAB",
        "#E4C793",
        "#E4B47B",
        "#CDA886",
        "#B69C90",
        "#DBCCC6",
        "#FFFFFF"
      ),
      label = "high_relief (lightness not steady)"
    ),
    p(
      "bcyr",
      "tidyterra grass",
      c(
        "#0000FF",
        "#5377FF",
        "#4FC8FF",
        "#6CFFE7",
        "#C4FF9E",
        "#F3FF47",
        "#FFD100",
        "#FF8800",
        "#FF0000"
      ),
      label = "bcyr (lightness not steady)"
    ),
    # a depth's looks (made for a bathymetry map of Icelandic waters, 2026-11-09)
    p(
      "quiet",
      "OKLCh",
      c("#DFECEE", "#CBE0E3", "#B9D2D8", "#A9C4CE", "#99B7C4", "#8AA9BB", "#7D9BB0", "#708DA7", "#657E9E"),
      use = "depth"
    ),
    p(
      "seabed",
      "OKLCh, sand and mud through water",
      c("#D8CAA2", "#B2BEA9", "#9AADA1", "#869D96", "#738C8B", "#5F7D81", "#4B6D78", "#3C5C6D", "#2E4C61"),
      label = "seabed (sand to deep water)",
      use = "depth"
    ),
    p(
      "chart",
      "OKLCh, after the nautical chart",
      c("#60A7D6", "#75B0D9", "#87B9DC", "#99C2DF", "#AACBE3", "#BAD4E7", "#CADDEB", "#D8E6F1", "#E5F0F8"),
      label = "chart (shallow blue, deep white)",
      use = "depth"
    ),
    p(
      "grey",
      "OKLCh",
      c("#E7EAED", "#D8DBDE", "#CACDD0", "#BBBFC2", "#ADB1B5", "#9FA3A7", "#91959A", "#84888C", "#767B80"),
      label = "grey (under other layers)",
      use = "depth"
    ),
    p(
      "drained",
      "tidyterra gmt_globe_hypso, resampled",
      c(
        "#CCCCCC", "#C1ACAC", "#B68C8C", "#AC6B6B", "#9F5E45", "#9B6F05", "#A48F18", "#C7A224", "#E6B858",
        "#F7D09A", "#E7E0AA", "#B0E28D", "#6DD474", "#35BF5B", "#37A041", "#378325", "#336600"
      ),
      label = "drained (the seabed as land)",
      use = "depth"
    ),
    p(
      "haxby",
      "tidyterra grass haxby, resampled",
      c(
        "#FFBA85", "#FFA44C", "#FFB955", "#F9D568", "#ECEF7F", "#C6FDA4", "#99F0AC", "#7EECD0", "#6BEBFD",
        "#52D7FF", "#37C1FF", "#2CAAFE", "#2893FD", "#287BF6", "#2865DE", "#274FC6", "#2539AF"
      ),
      label = "haxby (multibeam)",
      use = "depth"
    )
  )
}

# The looks a dem layer's colour offers by default, in this order, the first opening (rr_palettes())
.rr_map_depth_looks <- c("quiet", "seabed", "deep", "chart", "grey", "drained", "haxby")

#' @rdname rr_palettes
#' @export
rr_basemaps <- function() {
  emodnet <- "Sea floor \u00a9 EMODnet Bathymetry Consortium (CC BY 4.0)"
  esri <- "Tiles \u00a9 Esri and its sources"
  light <- "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json"
  dark <- "https://basemaps.cartocdn.com/gl/dark-matter-gl-style/style.json"
  floor <- "https://tiles.emodnet-bathymetry.eu/2020/baselayer/web_mercator/{z}/{x}/{y}.png"
  list(
    # the 2020 base layer's tiles stop at zoom 10 (blank beyond), drawn larger past it
    list(id = "emodnet", label = "Sea floor (EMODnet)", maxzoom = 10, attribution = emodnet, labels = light,
         tiles = floor),
    list(id = "emodnet_dark", label = "Sea floor, dark (EMODnet)", maxzoom = 10, attribution = emodnet, labels = dark,
         tiles = floor, background = "#15191d",
         paint = list("raster-brightness-max" = 0.42, "raster-saturation" = -0.35, "raster-contrast" = 0.15)),
    list(id = "emodnet_light", label = "Sea floor, light (EMODnet)", maxzoom = 12, attribution = emodnet,
         labels = light, tiles = "https://tiles.emodnet-bathymetry.eu/v12/mean_atlas_land/web_mercator/{z}/{x}/{y}.png"),
    list(id = "ocean", label = "Ocean (Esri)", maxzoom = 13, attribution = esri, labels = light,
         tiles = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}"),
    list(id = "none", label = "None")
  )
}

