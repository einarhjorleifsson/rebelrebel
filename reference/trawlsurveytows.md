# Icelandic groundfish survey, March 2019, tows

Each row is a tow of the same survey, as a line from its start to its
end; 581 tows. Read it with `rr_example("trawlsurveytows")`.

## Format

A parquet file of 581 rows in the package's `extdata` folder,
`trawlsurveytows.parquet`, with the columns:

- `id.lgs`: The tow's station id in MFRI's database.

- `geometry`: The tow (LINESTRING), DuckDB GEOMETRY, longitude and
  latitude on WGS 84.

## Source

Marine and Freshwater Research Institute (2019). "Icelandic groundfish
survey in March (SMB, Stofnmæling botnfiska á Íslandsmiðum), 2019:
vessel tracks and tows." Extracted from MFRI's database by the ramb
maintainer, <https://www.hafogvatn.is>.

Licence: MFRI data, shipped by its maintainer at MFRI; the terms are not
stated here.

## Details

extracted from MFRI's database (`data-raw/DATASET.R`).

## See also

[`rr_example()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_example.md);
the data dictionary `data-dict.yaml` in the package's `extdata` folder.

Other example data:
[`dansk`](https://einarhjorleifsson.github.io/rebelrebel/reference/dansk.md),
[`dansk_harbours`](https://einarhjorleifsson.github.io/rebelrebel/reference/dansk_harbours.md),
[`dansk_relief`](https://einarhjorleifsson.github.io/rebelrebel/reference/dansk_relief.md),
[`trawlsurvey`](https://einarhjorleifsson.github.io/rebelrebel/reference/trawlsurvey.md)
