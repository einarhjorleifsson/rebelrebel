# Seabed and land elevation around the Danish tracks

Each row is a cell of a grid of elevations, 30 arc-seconds square, over
the box of the Danish pings (2.5-13 E, 54.5-58.5 N): the depth of the
North Sea, the Skagerrak, the Kattegat and the western Baltic, and the
height of the land around them; 1,260 by 480 cells. Read it with
`rr_example("dansk_relief")`.

## Format

A parquet file of 604,800 rows in the package's `extdata` folder,
`dansk_relief.parquet`, with the columns:

- `lon`: Longitude of the cell's centre. Unit: degrees east.

- `lat`: Latitude of the cell's centre. Unit: degrees north.

- `elevation`: Elevation of the seabed or the land, negative below sea
  level (ETOPO 2022's heights are to the EGM2008 geoid); 460,862 of the
  cells are below sea level. Unit: metres.

## Source

NOAA National Centers for Environmental Information (2022). "ETOPO 2022
15 Arc-Second Global Relief Model." doi:10.25921/fd45-gt74
<https://doi.org/10.25921/fd45-gt74>, Bed elevation, read from NOAA's
image service on 2026-11-09.

Pante E, Simon-Bouhet B, Irisson J (2025). *marmap: Import, Plot and
Analyze Bathymetric and Topographic Data*.
doi:10.32614/CRAN.package.marmap
<https://doi.org/10.32614/CRAN.package.marmap>, R package version
1.0.12.

Licence: Public domain: a work of NOAA, an agency of the United States
government; NOAA NCEI asks that it be cited.

## Details

ETOPO 2022's 15 arc-second bed elevation, read with marmap's
`getNOAA.bathy()` at 30 arc-seconds (each cell one of the model's,
sampled by NOAA's image service, not their mean), with each cell placed
at its centre (marmap labels them from the box's edge to its edge) and
the elevation stored as a 32-bit number (`data-raw/dansk_relief.R`,
2026-11-09).

## See also

[`rr_example()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_example.md);
the data dictionary `data-dict.yaml` in the package's `extdata` folder.

Other example data:
[`dansk`](https://einarhjorleifsson.github.io/rebelrebel/reference/dansk.md),
[`dansk_harbours`](https://einarhjorleifsson.github.io/rebelrebel/reference/dansk_harbours.md),
[`trawlsurvey`](https://einarhjorleifsson.github.io/rebelrebel/reference/trawlsurvey.md),
[`trawlsurveytows`](https://einarhjorleifsson.github.io/rebelrebel/reference/trawlsurveytows.md)
