
<!-- README.md is generated from README.Rmd. Please edit the latter. -->

## capemlGIS: tools to aid the generation of EML metadata for spatial vectors and rasters

### overview

This package extends the
[CAPLTER/capeml](https://github.com/CAPLTER/capeml) package to
facilitate the creation of EML spatialRaster and spatialVector metadata.

### installation

Install from GitHub (after installing the
[devtools](https://cran.r-project.org/web/packages/devtools/index.html)
package:

``` r
devtools::install_github("CAPLTER/capemlGIS")
```

### getting started

Creating a EML dataset starts with the
[CAPLTER/capeml](https://github.com/CAPLTER/capeml) package, `capemlGIS`
is designed only to facilitate creating EML entities of type
`spatialRaster` and `spatialVector`. Users should please start with the
`capeml` workflow, including creating a `config.yaml` file that will
feature project details that `spatialRaster` and `spatialVector` require
for processing.

### options

#### EML

This package defaults to the current version of EML. If desired, users
can switch to the previous version with
`emld::eml_version("eml-2.1.1")`.

#### project naming

Most EML-generating functions in the capemlGIS package will create both
physical objects and EML references to those objects with the format:
`project-id`\_`object-name`\_`object-hash`.`file-extension` (e.g.,
*664\_site\_map\_5fb7b8d53d48010eab1a2e73db7f1941.kml*). The target
object (e.g., site\_map.kml) is renamed with the additional metadata and
this object name is referenced in the EML metadata. The exception to
this approach are spatialVectors where the hash of the file/object is
not included in the new object name. Note that the project-id is not
passed to any of the functions, and must exist in `config.yaml` (as
`projectid`).

Project-naming functionality can be turned off by setting the
`projectNaming` option in `create_spatialRaster()` to FALSE. When set to
FALSE, the object name is not changed, and the file name of the object
is included in the EML.

### tools to generate entity metadata

  - `write_attributes()` creates a template as a csv file for supplying
    attribute metadata for a spatial vector object that resides in the R
    environment
  - `write_raster_factors()` creates a template as a csv file for
    supplying code definition metadata for spatial rasters if raster
    values are categorical

### tools to create EML entities

  - `create_spatialRaster()` creates a EML entity of type spatialRaster
    - see
    [vignette](https://caplter.github.io/capeml/articles/create_spatialRaster.html)
    for more detail
  - `create_spatialVector()` creates a EML entity of type
    `spatialVector`. Output includes:
      - EML entity of type `spatialVector` that can be added to a EML
        dataset
      - input data written to a kml file with project naming (if
        selected). Note that regardless of source type, all spatial
        vector input data are converted to type kml.

### overview: create a spatialVector

**example: myvector**

``` r
# load spatial vector object
myvector <- read_sf(
  dsn = "vector_file_directory",
  layer = "vector_file_layer_name"
)

# drop geometry before writing attributes
myvector <- myvector %>%
  st_drop_geometry()

# assign Name to the site identifier variable
myvector <- myvector %>%
  mutate(Name = site_identifer)

write_attributes(myvector)

# re-load spatial vector object
myvector <- read_sf(
  dsn = "vector_file_directory",
  layer = "vector_file_layer_name"
)

myvector_desc <- "desc"

myvector_SV <- create_spatialVector(
  svname = myvector,
  description = myvector_desc
)
```
