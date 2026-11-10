# Icelandic groundfish survey, March 2019, vessel tracks

Each row is a position of one of four vessels on the Icelandic
groundfish survey in March (SMB), 27 February to 20 March 2019, with
whether a survey tow was on. Read it with `rr_example("trawlsurvey")`.

## Format

A parquet file of 8,918 rows in the package's `extdata` folder,
`trawlsurvey.parquet`, with the columns:

- `vid`: The vessel's Icelandic registry number.

- `lon`: Longitude. Unit: degrees east.

- `lat`: Latitude. Unit: degrees north.

- `speed`: Speed over ground, knots. Unit: knots.

- `heading`: Heading, degrees. Unit: degrees.

- `time`: Time of the position, Icelandic time, which is UTC all year.

- `behaviour`: Whether a survey tow was on. Values: `hauling`,
  `not hauling`.

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
[`trawlsurveytows`](https://einarhjorleifsson.github.io/rebelrebel/reference/trawlsurveytows.md)
