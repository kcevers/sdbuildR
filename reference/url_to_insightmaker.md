# Extract Insight Maker model from URL

Create XML string from Insight Maker URL. For internal use; use
[`import_insightmaker()`](https://kcevers.github.io/sdbuildR/reference/import_insightmaker.md)
to import an Insight Maker model.

## Usage

``` r
url_to_insightmaker(url, file = NULL)
```

## Arguments

- url:

  String with URL to an Insight Maker model

- file:

  If specified, file path to save Insight Maker model to. If NULL, do
  not save model.

## Value

XML string with Insight Maker model

## See also

[`import_insightmaker()`](https://kcevers.github.io/sdbuildR/reference/import_insightmaker.md)

## Examples

``` r
url <- "https://insightmaker.com/insight/43tz1nvUgbIiIOGSGtzIzj/Romeo-Juliet"
xml <- url_to_insightmaker(url)

# Save model to file
file <- tempfile(fileext = ".InsightMaker")
xml <- url_to_insightmaker(url, file = file)
file.remove(file)
#> [1] TRUE
```
