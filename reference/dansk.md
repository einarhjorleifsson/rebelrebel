# Danish vessel tracks (AIS)

Each row is an AIS position of one of 14 Danish fishing vessels with
four gears, a median 10 s apart, from December 2012 to October 2020,
with an expert's label of fishing or other. Read it with
`rr_example("dansk")`.

## Format

A parquet file of 395,558 rows in the package's `extdata` folder,
`dansk.parquet`, with the columns:

- `.rowid`: Row number, in the order of vessel and time.

- `vessel_id`: The vessel, anonymised.

- `time_stamp`: Time of the position, as given; the source states no
  zone.

- `lon`: Longitude. Unit: degrees east.

- `lat`: Latitude. Unit: degrees north.

- `speed`: Speed over ground, knots. Unit: knots.

- `course`: Course over ground, degrees, as given; a few values above
  360 (up to 33,098) are errors of the source. Unit: degrees.

- `gear`: The gear the vessel used. Values: `GN` gillnet, not specified;
  `GNS` set gillnet; `GTR` trammel net; `OTB` bottom otter trawl.

- `behaviour`: The expert's label of the position. Values: `Fishing`,
  `Other`.

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

`data-examples/example_data_AIS.csv` of the WKSSFGEO repository, with
repeated (vessel_id, time_stamp) rows dropped and `.rowid` added
(`data-raw/DATASET.R`).

## See also

[`rr_example()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_example.md);
the data dictionary `data-dict.yaml` in the package's `extdata` folder.

Other example data:
[`dansk_harbours`](https://einarhjorleifsson.github.io/rebelrebel/reference/dansk_harbours.md),
[`dansk_relief`](https://einarhjorleifsson.github.io/rebelrebel/reference/dansk_relief.md),
[`trawlsurvey`](https://einarhjorleifsson.github.io/rebelrebel/reference/trawlsurvey.md),
[`trawlsurveytows`](https://einarhjorleifsson.github.io/rebelrebel/reference/trawlsurveytows.md)
