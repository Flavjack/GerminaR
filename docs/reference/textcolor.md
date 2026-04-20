# Colourise text for display in the terminal.

If R is not currently running in a system that supports terminal colours
the text will be returned unchanged.

## Usage

``` r
textcolor(text, fg = "red", bg = NULL)
```

## Arguments

- text:

  character vector

- fg:

  foreground colour, defaults to white

- bg:

  background colour, defaults to transparent

## Details

Allowed colours are: black, blue, brown, cyan, dark gray, green, light
blue, light cyan, light gray, light green, light purple, light red,
purple, red, white, yellow

## Author

testthat package

## Examples

``` r
print(textcolor("Red", "red"))
#> [1] "\033[0;31mRed\033[0m"
cat(textcolor("Red", "red"), "\n")
#> Red 
cat(textcolor("White on red", "white", "red"), "\n")
#> White on red 
```
