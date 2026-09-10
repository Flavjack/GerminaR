# Osmotic potential calculator

Function to calculate the grams of salt or PEG-6000 needed for
determinated osmotic potential

## Usage

``` r
osmp(type = "salt", vol, pres, temp, mw, ki)
```

## Arguments

- type:

  Salt or PEG-6000 c("salt", "peg6000"). Default: "salt".

- vol:

  volume (liters)

- pres:

  Pressure (Mpa) in negative values. 1 bar = 0.1 Mpa

- temp:

  Temperature (centigrade)

- mw:

  Molecular weight

- ki:

  Salt dissociation constant (NaCl = 1.8)

## Value

Numeric value (grams)
