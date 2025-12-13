# Investigate mechanisms

Creates graphs that visiualize the mechanisms behind each sufficient
cause using the `DiagrammeR` package.

`new_mechanism()` and `validate_mechanism()` define the
`epicmodel_mechanism` S3 class, which is created by `mechanism()`.

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) renders the
graphs in the RStudio Viewer.

[`print()`](https://rdrr.io/r/base/print.html) prints the legend for
node labels in the console.

## Usage

``` r
mechanism(scc, modules = TRUE, module_colors = NULL)

new_mechanism(x = list())

validate_mechanism(x)

# S3 method for class 'epicmodel_mechanism'
plot(x, reverse = TRUE, ...)

# S3 method for class 'epicmodel_mechanism'
print(x, ...)
```

## Arguments

- scc:

  For `mechanism()`, an object of class `epicmodel_scc`.

- modules:

  For `mechanism()`, TRUE (default) or FALSE, indicating if nodes in the
  same module should be colored equally (TRUE) or if all nodes have
  white background (FALSE). Colors are only applied, if modules have
  actually been specified in the `epicmodel_steplist`. If modules are
  considered by `mechanism()`, the module keywords are added to the
  legend (accessable via
  [`print()`](https://rdrr.io/r/base/print.html)).

- module_colors:

  For `mechanism()`, if nodes are colored by module, colors can be
  provided via this argument. Colors must be provided as a character
  vector. Both named colors and hexadecimal color codes are allowed. The
  function has 8 colors stored internally. If `module_colors` = NULL
  (default), these colors are used. If the model has more than 8
  modules, `module_colors` must be specified. If more colors than
  necessary are specified, the function takes as many as necessary from
  the start of the vector.

- x:

  `x` is used in several functions:

  - `new_mechanism()`: A list to be converted to class
    `epicmodel_mechanism`.

  - `validate_mechanism()`: An object of class `epicmodel_mechanism` to
    be validated.

  - `plot.epicmodel_mechanism()`: An object of class
    `epicmodel_mechanism`.

  - `print.epicmodel_mechanism()`: An object of class
    `epicmodel_mechanism`.

- reverse:

  For `plot.epicmodel_mechanism()`, TRUE or FALSE indicating if the
  output should be displayed in reverse order. Since graphs rendered
  later show up first in the viewer pane, reverse = T leads to SC1 being
  the last rendered and the one displayed on top.

- ...:

  Additional arguments for generics
  [`print()`](https://rdrr.io/r/base/print.html) and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

- `mechanism()`: An object of class `epicmodel_mechanism`. Use
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) to plot the
  graphs in the RStudio Viewer. Use
  [`print()`](https://rdrr.io/r/base/print.html) to print the legend in
  the console. Use
  [`export_mechanism()`](https://forsterepi.github.io/epicmodel/reference/export_mechanism.md)
  to save the graphs as PNG, PDF, SVG, or PostScript.

- `new_mechanism()`: An object of class `epicmodel_mechanism`.

- `validate_mechanism()`: An object of class `epicmodel_mechanism` that
  has been checked to have the correct structure.

- `plot.epicmodel_mechanism()`: Renders the graphs in the RStudio
  Viewer.

- `print.epicmodel_mechanism()`: Prints the legend of the
  `epicmodel_mechanism` object in the console.

## Details

### The graphs

One graph per sufficient cause is created. The graphs display steps as
nodes and IF/IFNOT relations as edges. Nodes will not be labeled with
their IDs or descriptions due to limited space, but with newly created
labels. These labels are based on the type of node and are listed
together with the step description in the `legend` (accessed by
[`print()`](https://rdrr.io/r/base/print.html)). Step descriptions are
also accessible via tooltips in the graph. Just put your cursor on the
node labels.

There are 4 different types of nodes:

- `Component causes`: Labeled `"CC"`, squares, gray border

- `Interventions`: Labeled `"I"`, triangles, gray border

- `End steps`: Labeled `"E"`, circles, black border

- `Other steps`: Labeled `"S"`, circles, gray border

There are 2 types of edges:

- `IF conditions`: gray arrows

- `IFNOT conditions`: red and T-shaped

### `epicmodel_mechanism` objects

`epicmodel_mechanism` objects are created by `mechanism()`. They are
lists containing 2 elements:

- `legend`:

  A data.frame with up to 3 variables:

  - `Label`: Contains the labels used in the graphs.

  - `Module`: Contains the name of the module to which this step
    belongs. Only available if `modules = TRUE` in `mechanism()` and if
    the SCC model actually uses modules (specified in element
    `sc_use_modules` of `epicmodel_scc` objects).

  - `Step`: A description of the corresponding step.

- `graph`:

  A list of length equal to the number of sufficient causes. Each
  element contains another list with 2 elements:

  - `ndf`: A data.frame containing information about nodes in the graph
    (see
    [`DiagrammeR::node_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/node_aes.html)).

  - `edf`: A data.frame containing information about edges in the graph
    (see
    [`DiagrammeR::edge_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_aes.html)).

- `ndf`:

  Data.frames containing the following variables:

  - `id`: Node ID used internally by `DiagrammeR` to define edges
    (`from` and `to` in `edf` data.frames).

  - `type`: Type of node as defined by `epicmodel`. Possible options
    are: `cc` (component cause), `int` (intervention), `end` (step that
    is part of an outcome definition), `other` (all other steps).

  - `label`: The label displayed in the graph and listed in variable
    `Label` of `legend`.

  - `tooltip`: The text displayed when putting the cursor on top of the
    node label. Corresponds to the step descriptions in variable `Step`
    of `legend`.

  - `shape`: The shape of the node. `square` for type `cc`, `triangle`
    for type `int`, and `circle` for types `end` and `other`.

  - `color`: Color of the node border. Gray for types `cc`, `int`, and
    `other`, and black for type `end`.

  - `fillcolor`: Color of the background, which is similar for all steps
    in the same module. If modules are not considered, `fillcolor` is
    white for all nodes.

  - `fontcolor`: Color of the node label. Always black.

- `edf`:

  Data.frames containing the following variables:

  - `id`: Edge ID used internally by `DiagrammeR`.

  - `from`: Node ID of the node from which the edge starts.

  - `to`: Node ID of the node at which the edge ends.

  - `rel`: Type of edge as defined by `epicmodel`. Possible options are:
    `if` (`from` node is in IF condition of `to` node), `ifnot` (`from`
    node is in IFNOT condition of `to` node).

  - `arrowhead`: Type of arrow. `normal` for rel `if` and `tee` for rel
    `ifnot`.

  - `arrowsize`: Size of arrow. 1 for rel `if` and 1.2 for rel `ifnot`.

  - `color`: Color of arrow. Gray for rel `if` and `#A65141` for rel
    `ifnot`.

## See also

- [`export_mechanism()`](https://forsterepi.github.io/epicmodel/reference/export_mechanism.md)
  for saving the plots

- [`DiagrammeR::node_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/node_aes.html)
  for a list of node-related variables in `DiagrammeR`

- [`DiagrammeR::edge_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_aes.html)
  for a list of edge-related variables in `DiagrammeR`

## Examples

``` r
# Create some SCC model
steplist_checked <- check_steplist(steplist_rain)
#> 
#> ── Checking epicmodel_steplist steplist_rain ───────────────────────────────────
#> ✔ Checking WHAT IDs was successful.
#> ✔ Checking DOES IDs was successful.
#> ✔ Checking WHERE IDs was successful.
#> ✔ Checking Module IDs was successful.
#> ✔ Checking ICC IDs was successful.
#> ✔ Checking WHAT keywords was successful.
#> ✔ Checking DOES keywords was successful.
#> ✔ Checking WHERE keywords was successful.
#> ✔ Checking Module keywords was successful.
#> ✔ Checking Modules was successful.
#> ✔ Checking ICC entries was successful.
#> ✔ Checking WHAT segments was successful.
#> ✔ Checking DOES segments was successful.
#> ✔ Checking WHERE segments was successful.
#> ✔ Checking references was successful.
#> ✔ Checking start/end steps was successful.
#> ✔ Checking THEN statements was successful.
#> ✔ Checking THEN/IF/IFNOT equality was successful.
#> ✔ Checking outcome definitions was successful.
#> ── Summary ─────────────────────────────────────────────────────────────────────
#> ✔ Checking successful!
scc_model <- create_scc(steplist_checked)
#> 
#> ── Create SCC Model ──
#> 
#> ✔ 15/15 | Check if set of component causes is sufficient
#> ✔ 5/5 | Check if sufficiency dependends on IFNOT conditions
#> ✔ 5/5 | Check if sufficient cause is minimal
#> ℹ 2/5 sufficient causes are minimal

# Derive mechanisms
mech <- mechanism(scc_model)

# new_mechanism() and validate_mechanism() are used inside mechanism()
# nonetheless, you can check its structure using validate_mechanism()
validate_mechanism(mech)
#>  Label Module   Step          
#>  CC1   activity no vacation   
#>  CC2   fate     weekday       
#>  CC3   weather  rain          
#>  CC4   activity get groceries 
#>  I1    activity take vacation 
#>  I2    activity take umbrella 
#>  I3    activity work from home
#>  S1    activity walk to work  
#>  S2    activity go outside    
#>  E1    weather  you get wet   

# Plot the mechanisms
plot(mech)
#> $SC2
#> 
#> $SC1
#> 

# Print the legend
print(mech)
#>  Label Module   Step          
#>  CC1   activity no vacation   
#>  CC2   fate     weekday       
#>  CC3   weather  rain          
#>  CC4   activity get groceries 
#>  I1    activity take vacation 
#>  I2    activity take umbrella 
#>  I3    activity work from home
#>  S1    activity walk to work  
#>  S2    activity go outside    
#>  E1    weather  you get wet   
mech
#>  Label Module   Step          
#>  CC1   activity no vacation   
#>  CC2   fate     weekday       
#>  CC3   weather  rain          
#>  CC4   activity get groceries 
#>  I1    activity take vacation 
#>  I2    activity take umbrella 
#>  I3    activity work from home
#>  S1    activity walk to work  
#>  S2    activity go outside    
#>  E1    weather  you get wet   
```
