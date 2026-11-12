# Read an example dataset shipped with rebelrebel

rebelrebel's example data are parquet files in its `extdata` folder,
opened lazily as DuckDB tables; collect one with
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html).
Each dataset has its own help page
([`?dansk`](https://einarhjorleifsson.github.io/rebelrebel/reference/dansk.md),
[`?dansk_relief`](https://einarhjorleifsson.github.io/rebelrebel/reference/dansk_relief.md),
...): what a row is, its columns, its sources and its licence. The same
is in the folder's data dictionary, `data-dict.yaml`
(<https://data-dict.tidyverse.org/>), and in each file's own parquet
metadata, so a file copied out of the package still says what it is. The
sources are BibTeX entries in the package's `REFERENCES.bib`. The files
were made by the R package ramb, whose `data-raw` folder holds the
scripts.

## Usage

``` r
rr_example(name = NULL, con = NULL)
```

## Arguments

- name:

  The dataset's name; `NULL` for the catalogue of all of them.

- con:

  A DuckDB connection to open the dataset on; rebelrebel's own by
  default. Lazy tables used together must share a connection, so give
  the one your other tables are on
  ([`dbplyr::remote_con()`](https://dbplyr.tidyverse.org/reference/remote_name.html)
  of one of them).

## Value

A lazy DuckDB table; or, for `name = NULL`, a tibble with one row per
dataset: `name`, `label`, `description`, `rows`, `licence`, `cite` (its
sources as text) and `bibtex` (the same as BibTeX), read from the files'
own metadata.

## Details

The two datasets with shapes (`dansk_harbours`, `trawlsurveytows`) hold
them in `geometry` as DuckDB's GEOMETRY (GeoParquet, longitude and
latitude on WGS 84), which DuckDB's `ST_` functions work on. Collected
into R it is WKB; for sf:
`x <- dplyr::collect(rr_example("dansk_harbours"))`, then
`sf::st_sf(x[names(x) != "geometry"], geometry = sf::st_as_sfc(structure(x$geometry, class = "WKB"), crs = 4326))`.

## Examples

``` r
rr_example()
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpqTczC4/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> # A tibble: 5 × 7
#>   name            label                  description   rows licence cite  bibtex
#>   <chr>           <chr>                  <chr>        <int> <chr>   <chr> <chr> 
#> 1 dansk           Danish vessel tracks … Each row i… 395558 MIT, C… "ICE… "@Tec…
#> 2 dansk_harbours  Harbour polygons for … Each row i…    942 MIT, C… "ICE… "@Tec…
#> 3 dansk_relief    Seabed and land eleva… Each row i… 604800 Public… "NOA… "@Mis…
#> 4 trawlsurvey     Icelandic groundfish … Each row i…   8918 MFRI d… "Mar… "@Mis…
#> 5 trawlsurveytows Icelandic groundfish … Each row i…    581 MFRI d… "Mar… "@Mis…
rr_example("dansk_harbours")
#> # A query:  ?? x 2
#> # Database: DuckDB 2.0.0 [unknown@Linux 7.0.0-1019-azure:R 4.6.2/:memory:]
#>    SI_HARB geometry   
#>      <int> <list>     
#>  1       1 <raw [93]> 
#>  2       2 <raw [93]> 
#>  3       3 <raw [109]>
#>  4       4 <raw [109]>
#>  5       5 <raw [93]> 
#>  6       6 <raw [125]>
#>  7       7 <raw [125]>
#>  8       8 <raw [93]> 
#>  9       9 <raw [109]>
#> 10      10 <raw [109]>
#> # ℹ more rows
cat(rr_example()$bibtex[1])
#> @TechReport{ices2022wkssfgeo,
#>   author = {{ICES}},
#>   title = {Workshop on Geo-Spatial Data for Small-Scale Fisheries ({WKSSFGEO})},
#>   institution = {International Council for the Exploration of the Sea},
#>   number = {4:10},
#>   doi = {10.17895/ices.pub.10032},
#>   type = {ICES Scientific Reports},
#>   year = {2022},
#> }
#> 
#> @Misc{wkssfgeo_github,
#>   author = {{ICES WKSSFGEO}},
#>   title = {{WKSSFGEO}: code and example data of the {ICES} Workshop on Geo-Spatial Data for Small-Scale Fisheries},
#>   url = {https://github.com/ices-eg/WKSSFGEO},
#>   year = {2021},
#>   note = {Commit 3da870d. MIT licence},
#> }
```
