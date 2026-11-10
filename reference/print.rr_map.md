# Show a map

At an interactive console a map is served when printed
([`rr_serve()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_serve.md),
on the port its first layer function set, else 8790) and opens in
RStudio's Viewer or a browser; elsewhere, as when a document is knitted,
its title and layers are printed.

## Usage

``` r
# S3 method for class 'rr_map'
print(x, ...)

# S3 method for class 'rr_map'
format(x, ...)
```

## Arguments

- x:

  An
  [`rr_map()`](https://einarhjorleifsson.github.io/rebelrebel/reference/rr_map.md).

- ...:

  Not used.

## Value

`x`, invisibly.
