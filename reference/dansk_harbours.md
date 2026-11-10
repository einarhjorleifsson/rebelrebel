# Harbour polygons for the Danish tracks

Each row is a harbour, as a polygon; 942 harbours, most of them in
Europe. Read it with `rr_example("dansk_harbours")`.

## Format

A parquet file of 942 rows in the package's `extdata` folder,
`dansk_harbours.parquet`, with the columns:

- `SI_HARB`: The harbour, a sequential id.

- `geometry`: The harbour's polygon (POLYGON), DuckDB GEOMETRY,
  longitude and latitude on WGS 84.

## Source

ICES (2022). "Workshop on Geo-Spatial Data for Small-Scale Fisheries
(WKSSFGEO)." ICES Scientific Reports 4:10, International Council for the
Exploration of the Sea. doi:10.17895/ices.pub.10032
<https://doi.org/10.17895/ices.pub.10032>.

ICES WKSSFGEO (2021). "WKSSFGEO: code and example data of the ICES
Workshop on Geo-Spatial Data for Small-Scale Fisheries." Commit 3da870d.
MIT licence, <https://github.com/ices-eg/WKSSFGEO>.

Licence: MIT, Copyright (c) 2021 ICES Expert Groups.

## Details

`data/harbours.rds` of the WKSSFGEO repository, with the id `SI_HARB`
added (`data-raw/DATASET.R`).

## See also

[`rr_example()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_example.md);
the data dictionary `data-dict.yaml` in the package's `extdata` folder.

Other example data:
[`dansk`](https://einarhjorleifsson.github.io/rebelrebel/reference/dansk.md),
[`dansk_relief`](https://einarhjorleifsson.github.io/rebelrebel/reference/dansk_relief.md),
[`trawlsurvey`](https://einarhjorleifsson.github.io/rebelrebel/reference/trawlsurvey.md),
[`trawlsurveytows`](https://einarhjorleifsson.github.io/rebelrebel/reference/trawlsurveytows.md)
