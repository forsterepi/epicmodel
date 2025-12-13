# Export mechanisms

Exports one or all sufficient cause mechanisms as PNG, PDF, SVG, or
PostScript using
[`DiagrammeR::export_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/export_graph.html).

## Usage

``` r
export_mechanism(
  mechanism,
  sc = NULL,
  file_name = NULL,
  file_type = "png",
  title = NULL,
  ...
)
```

## Arguments

- mechanism:

  An object of class `epicmodel_mechanism`.

- sc:

  A single integer value (can be specified as numeric, e.g., 2 instead
  of 2L). If provided, a graph is only exported for the specified
  sufficient cause, e.g., for SC2 if sc = 2. If sc = NULL (default),
  graphs for all sufficient causes are exported.

- file_name:

  The name of the exported file (including it's extension).

- file_type:

  The type of file to be exported. Options for graph files are: `png`,
  `pdf`, `svg`, and `ps`.

- title:

  An optional title for the output graph.

- ...:

  Arguments passed on to
  [`DiagrammeR::export_graph`](https://rich-iannone.github.io/DiagrammeR/reference/export_graph.html)

  `width`

  :   Output width in pixels or `NULL` for default. Only useful for
      export to image file formats `png`, `pdf`, `svg`, and `ps`.

  `height`

  :   Output height in pixels or `NULL` for default. Only useful for
      export to image file formats `png`, `pdf`, `svg`, and `ps`.

## Value

Saves the mechanisms as PNG, PDF, SVG, or PostScript.

## See also

- [`DiagrammeR::export_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/export_graph.html)

- [`mechanism()`](https://forsterepi.github.io/epicmodel/reference/mechanism.md)
  for information on sufficient cause mechanisms

## Examples

``` r
# Derive mechanisms
mech <- mechanism(scc_rain)

# Export mechanism plot of sufficient cause (sc) 1
if(interactive()){
tmp <- tempfile(fileext = ".png")
export_mechanism(mech, sc = 1, file_name = tmp, title = "Sufficient Cause 1")
unlink(tmp) # delete saved file
}
```
