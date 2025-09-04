
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tsjson

<!-- badges: start -->

![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)
[![R-CMD-check](https://github.com/gaborcsardi/tsjson/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gaborcsardi/tsjson/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Extract and manipulate parts of JSON files without touching the
formatting and comments in other parts.

## Installation

You can install the development version of tsjson from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("gaborcsardi/tsjson")
```

## Documentation

See at
[`https://gaborcsardi.github.io/tsjson/`](https://gaborcsardi.github.io/tsjson/reference/index.html/)
and also in the installed package: `help(package = "tsjson")`.

## Quickstart

### Create a tjson object

Create a tsjson object from a string:

``` r
txt <- r"(
// this is a comment
{
  "a": {
    "a1": [1, 2, 3],
    // comment
    "a2": "string"
  },
  "b": [
    {
      "b11": true,
      "b12": false
    },
    {
      "b21": false,
      "b22": false
    }
  ]
}
)"
json <- load_json(text = txt)
```

Pretty print a tsjson object:

``` r
json
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="/Users/gaborcsardi/works/tsjson/man/figures/print-json-dark.svg">
<img src="/Users/gaborcsardi/works/tsjson/man/figures/print-json.svg" />
</picture>

### Select elements in a tsjson object

Select element by objects key:

``` r
select(json, "a")
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="/Users/gaborcsardi/works/tsjson/man/figures/select-key-dark.svg">
<img src="/Users/gaborcsardi/works/tsjson/man/figures/select-key.svg" />
</picture>

Select element inside element:

``` r
select(json, "a", "a1")
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="/Users/gaborcsardi/works/tsjson/man/figures/select-select-dark.svg">
<img src="/Users/gaborcsardi/works/tsjson/man/figures/select-select.svg" />
</picture>

Select element(s) of an array:

``` r
select(json, "a", "a1", 1:2)
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="/Users/gaborcsardi/works/tsjson/man/figures/select-array-dark.svg">
<img src="/Users/gaborcsardi/works/tsjson/man/figures/select-array.svg" />
</picture>

Select multiple keys from an object:

``` r
select(json, "a", c("a1", "a2"))
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="/Users/gaborcsardi/works/tsjson/man/figures/select-multiple-dark.svg">
<img src="/Users/gaborcsardi/works/tsjson/man/figures/select-multiple.svg" />
</picture>

### Delete elements

Delete selected elements:

``` r
select(json, "a", "a1") |> delete_selected()
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="/Users/gaborcsardi/works/tsjson/man/figures/delete-dark.svg">
<img src="/Users/gaborcsardi/works/tsjson/man/figures/delete.svg" />
</picture>

### Insert elements

Insert element into an array:

``` r
select(json, "a", "a1") |> insert_into_selected(at = 2, "new")
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="/Users/gaborcsardi/works/tsjson/man/figures/insert-array-dark.svg">
<img src="/Users/gaborcsardi/works/tsjson/man/figures/insert-array.svg" />
</picture>

Inserting into an array reformats the array.

Insert element into an object, at the specified key:

``` r
select(json, "a") |>
  insert_into_selected(key = "a0", at = 0, list("new", "element"))
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="/Users/gaborcsardi/works/tsjson/man/figures/insert-object-dark.svg">
<img src="/Users/gaborcsardi/works/tsjson/man/figures/insert-object.svg" />
</picture>

### Update elements

Update existing element:

``` r
select(json, "a", c("a1", "a2")) |> update_selected("new value")
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="/Users/gaborcsardi/works/tsjson/man/figures/update-dark.svg">
<img src="/Users/gaborcsardi/works/tsjson/man/figures/update.svg" />
</picture>

Inserts the element if some parents are missing:

``` r
json <- load_json(text = "{ \"a\": { \"b\": true } }")
json
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="/Users/gaborcsardi/works/tsjson/man/figures/update-insert-dark.svg">
<img src="/Users/gaborcsardi/works/tsjson/man/figures/update-insert.svg" />
</picture>

``` r
select(json, "a", "x", "y") |> update_selected(list(1,2,3))
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="/Users/gaborcsardi/works/tsjson/man/figures/update-insert-2-dark.svg">
<img src="/Users/gaborcsardi/works/tsjson/man/figures/update-insert-2.svg" />
</picture>

# License

MIT © Posit Software, PBC
