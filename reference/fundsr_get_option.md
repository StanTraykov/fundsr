# Get a fundsr option

Reads `getOption("fundsr.<name>")`. If unset, uses `default` if
provided; otherwise falls back to the package default from
`.fundsr_option_defaults()`. Errors on unknown option names.

## Usage

``` r
fundsr_get_option(name, default)
```

## Arguments

- name:

  A single option name without the "fundsr." prefix (e.g. "reload").

- default:

  Optional fallback value used if the option is unset. If not supplied,
  the package default is used.

## Value

The option value (or its fallback/default).
