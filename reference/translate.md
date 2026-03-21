# Translate strings using LibreTranslate selected instance

Use this function to translate strings, from regular text to entire
dataframes.

## Usage

``` r
translate(q, from = "auto", to)
```

## Arguments

- q:

  The string you want to translate

- from:

  The origin language of the text. If you leave it empty, it will be set
  to "auto".

- to:

  The destination language you want the string to be translated to.

## Examples

``` r
if (FALSE) { # \dontrun{
translate(q = "hello, nice to meet you", from = "en", to = "es")
} # }
```
