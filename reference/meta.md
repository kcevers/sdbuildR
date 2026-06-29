# Modify meta of stock-and-flow model

The meta of a stock-and-flow model contains metadata about the model,
such as the name, author, and version. Modify the meta of an existing
model with standard or custom properties.

## Usage

``` r
meta(
  object,
  name = "My Model",
  caption = "My Model Description",
  created = Sys.time(),
  author = "Me",
  version = "0.0.0.9000",
  URL = "",
  doi = "",
  ...
)
```

## Arguments

- object:

  Stock-and-flow model, object of class
  [`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md).

- name:

  Model name. Defaults to "My Model".

- caption:

  Model description. Defaults to "My Model Description".

- created:

  Date the model was created. Defaults to Sys.time().

- author:

  Creator of the model. Defaults to "Me".

- version:

  Model version. Defaults to "0.0.0.9000", a development version.

- URL:

  URL associated with model. Defaults to "".

- doi:

  DOI associated with the model. Defaults to "".

- ...:

  Optional other entries to add to the meta.

## Value

A stock-and-flow model object of class
[`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md)

## Examples

``` r
sfm <- stockflow() |>
  meta(
    name = "My first model",
    caption = "This is my first model",
    author = "Kyra Evers",
    version = "1.1"
  )
```
