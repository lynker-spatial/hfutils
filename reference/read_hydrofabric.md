# Read Hydrofabric Layers from a GeoPackage (or accept in-memory \`sf\` objects)

Convenience reader for a Hydrofabric GeoPackage that returns a named
list of \`sf\` objects: \`list(divides = ..., flowpaths = ...)\`. You
may provide a GeoPackage path and let the function auto-detect layer
names, \*or\* pass in-memory \`sf\` objects directly. Optionally
transforms to a target CRS and harmonizes CRS across returned layers.

## Usage

``` r
read_hydrofabric(
  gpkg = NULL,
  divides = NULL,
  flowpaths = NULL,
  realization = c("all", "divides", "flowpaths"),
  crs = NULL,
  verbose = Sys.getenv("hydrofab_verbose") != "false"
)
```

## Arguments

- gpkg:

  Character path to a GeoPackage. If \`NULL\`, the function will only
  use the \`divides\` and/or \`flowpaths\` arguments (if provided).
  Default: \`NULL\`.

- divides:

  Either \`NULL\`, a character layer name within \`gpkg\`, or an
  in-memory \`sf\` object representing divides/catchments.

- flowpaths:

  Either \`NULL\`, a character layer name within \`gpkg\`, or an
  in-memory \`sf\` object representing flowpaths/flowlines.

- realization:

  One of \`"all"\`, \`"divides"\`, or \`"flowpaths"\`. Controls which
  layers to auto-discover/read from \`gpkg\` when corresponding
  arguments are \`NULL\`. Default: \`"all"\`.

- crs:

  Optional CRS to transform outputs to (e.g., EPSG code like \`5070\`,
  an \`sf::crs\` object, or any input accepted by \`sf::st_crs()\`).

- verbose:

  Logical; print progress messages. Default uses the environment
  variable \`hydrofab_verbose\` (anything other than literal \`"false"\`
  is treated as verbose).

## Value

A named list with up to two elements:

- \`divides\`: an \`sf\` object (if available)

- \`flowpaths\`: an \`sf\` object (if available)

## Details

\- If \`divides\`/\`flowpaths\` are provided as in-memory \`sf\`
objects, these are used as-is (subject to optional CRS
transformation). - If \`gpkg\` is provided and a corresponding layer
argument is:

- \`NULL\`: the function tries to auto-detect the layer name.

- a character: that name is used (and validated).

\- Auto-discovery looks for:

- Flowpaths: names matching \`flowpath\|flowline\`, excluding
  \`attributes\|edge_list\`.

- Divides: names matching \`divide\|catchment\`, excluding \`network\`.

If multiple candidates are found, an error is thrown with the candidates
listed.

CRS behavior: - If \`crs\` is provided, all returned layers are
transformed to that CRS. - Otherwise, if two layers are returned with
different CRSs, the second is transformed to the CRS of the first.

## Examples

``` r
if (FALSE) { # \dontrun{
# Auto-detect layers from a HydroFabric GPKG and return both
x <- read_hydrofabric("path/to/hydrofabric.gpkg")

# Read only flowpaths (auto-detected) and transform to EPSG:5070
x <- read_hydrofabric("path/to/hydrofabric.gpkg", realization = "flowpaths", crs = 5070)

# Supply in-memory sf layers (no gpkg), harmonize to EPSG:3857
x <- read_hydrofabric(divides = my_divides_sf, flowpaths = my_flow_sf, crs = 3857)
} # }
```
