# Read a QML style file

Read a QML style file

## Usage

``` r
read_qml(qml_file)
```

## Arguments

- qml_file:

  Path to a \`.qml\` file.

## Value

The file contents as a single character string.

## Examples

``` r
qml <- read_qml(system.file("qml", "nexus.qml", package = "hfutils"))
substr(qml, 1, 40)
#> [1] "<!DOCTYPE qgis PUBLIC 'http://mrcc.com/q"
```
