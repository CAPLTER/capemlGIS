<!-- README.md is generated from README.Rmd. Please edit that file. -->

<br>
<br>

## capemlGIS: tools to generate EML metadata for spatial raster data

### overview

`capemlGIS` is a companion package to
[CAPLTER/capeml](https://github.com/CAPLTER/capeml) that facilitates the
creation of Ecological Metadata Language (EML) `spatialRaster` entities
for publishing to the
[Environmental Data Initiative (EDI)](https://edirepository.org/) data
repository.

> **Looking for vector data (KML, GeoJSON, shapefile)?**
> Vector metadata functionality has moved to the dedicated
> [CAPLTER/capemlVector](https://github.com/CAPLTER/capemlVector) package.

The capeml package ecosystem:

| package | scope |
|---|---|
| [capeml](https://github.com/CAPLTER/capeml) | tabular data, dataset-level metadata |
| [capemlVector](https://github.com/CAPLTER/capemlVector) | spatial vector data (KML, GeoJSON, shapefile) |
| capemlGIS | spatial raster data |

### installation

Install with [pak](https://pak.r-lib.org/):

``` r
# install pak if needed
# install.packages("pak")

pak::pak("CAPLTER/capemlGIS")
```

`capemlGIS` depends on [capeml](https://github.com/CAPLTER/capeml),
which will be installed automatically via the `Remotes` field when using
`pak`.

### getting started

Creating an EML dataset starts with the
[CAPLTER/capeml](https://github.com/CAPLTER/capeml) package.
`capemlGIS` is designed to create EML entities of type `spatialRaster`;
users should begin with the `capeml` workflow, including creating a
`config.yaml` in the working directory that contains project-level
metadata required by the raster functions.

### options

#### EML version

This package defaults to the current version of EML. Users can switch to
the previous version with `emld::eml_version("eml-2.1.1")`.

#### project naming

`create_raster()` will name output files with the format
`identifier`\_`object-name`.`file-extension` when
`projectNaming = TRUE` (the default). The identifier is read from
`config.yaml`. Set `projectNaming = FALSE` to use the object name as-is.

### functions

| function | output | description |
|---|---|---|
| `create_raster()` | zipped raster file(s) + EML `spatialRaster` | generates EML metadata for a raster object |
| `write_raster_factors()` | `_raster_factors.yaml` template | creates a metadata template for categorical raster values |
| `zipRelatedFiles()` | zipped archive | zips files sharing a base name for inclusion in a data package |

### workflow: prepare raster factor metadata

If the raster contains categorical values, generate a factor metadata
template before calling `create_raster()`:

``` r
capemlGIS::write_raster_factors(
  raster_entity = my_raster,
  value_name    = "category_field"
)
```

Edit the generated yaml file to supply definitions for each category
value. The file will be read automatically by `create_raster()`.

### workflow: create a spatialRaster

``` r
my_raster <- raster::raster("path/to/my_raster.img")

my_raster_desc <- "a description of the raster data entity"

my_raster_SR <- capemlGIS::create_raster(
  rasterName    = my_raster,
  description   = my_raster_desc,
  epsgProjection = 4326,
  overwrite     = TRUE,
  projectNaming = TRUE
)

# my_raster_SR is an EML spatialRaster — add it to the EML dataset
```

