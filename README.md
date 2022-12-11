---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit the latter. -->

## capemlGIS: tools to aid the generation of EML metadata for spatial vectors and rasters

### overview
  
This package extends the [CAPLTER/capeml](https://github.com/CAPLTER/capeml) package to facilitate the creation of EML spatialRaster and spatialVector objects and metadata.


### installation

Install from GitHub (after installing the [devtools](https://cran.r-project.org/web/packages/devtools/index.html) package:


```r
devtools::install_github("CAPLTER/capemlGIS")
```

### getting started

Creating a EML dataset starts with the [CAPLTER/capeml](https://github.com/CAPLTER/capeml) package, `capemlGIS` is designed only to facilitate creating EML entities of type `spatialRaster` and `spatialVector`. Users should please start with the `capeml` workflow, including creating a `config.yaml` file that will feature project details that `spatialRaster` and `spatialVector` require for processing.


### options

#### EML

This package defaults to the current version of EML. If desired, users can switch to the previous version with `emld::eml_version("eml-2.1.1")`.

#### project naming

Most EML-generating functions in the capeml and capemlGIS packages will create both physical objects and EML references to those objects. By default, the package will name output files with the format `project identifier (number)`\_`object-name`.`file-extension` (e.g., *664_site_map.kml*). The target object (e.g., site_map.png from the previous example) is renamed with the additional metadata and this object name is referenced in the EML metadata. Project naming can be disabled by setting the `projectNaming` or `project_naming` flag to `FALSE`. When set to FALSE, the object name is not changed, and the name of the data object as read into the R environment is written to file and referenced in the EML. Note that the project identifier (number) is not passed as an argument, and must exist in `config.yaml` (as `identifer`). 

### tools to generate entity metadata
*note that write_attributes and write_factors are in the capeml package*

* `capeml::write_attributes()` creates a template as a yaml file for supplying attribute metadata for a spatial vector object that resides in the R environment.
* `capeml::write_factors()` creates a template as a yaml file for supplying code definition metadata for factors in vector data object that resides in the R environment.
* `capemlGIS::write_raster_factors()` creates a template as a yaml file for supplying code definition metadata for spatial rasters if raster values are categorical.

### tools to create spatial data objects and corresponding EML metadata entities

* `create_spatialRaster` Output includes:
  + EML entity of type `spatialRaster`
  + see
    [vignette](https://caplter.github.io/capeml/articles/create_spatialRaster.html)
    for more detail
* `create_vector` Output includes:
  + EML entity of type `spatialVector` that can be added to a EML dataset.
  + Input data written to a kml or GeoJSON file with project naming (if selected).
* `create_vector_shape` Output includes:
  + EML entity of type `spatialVector` that can be added to a EML dataset.
  + Input data **written** to a shapefile with project naming (if selected). Shapefile files are written to a directory that is zipped.
* `package_vector_shape` Output includes:
  + EML entity of type `spatialVector` that can be added to a EML dataset.
  + **Harvests** all relevant files that constitute a single shapefile into a directory that is then zipped.
  + `package_vector_shape` differs from the `create_*` series of functions in that it does not write data to file but rather packages the files that constitute a shapefile into a new directory that is then zipped for inclusion in a data package. Use package shape if it is important that the input data are not read into R or otherwise altered during the construction of the dataset. A limitation of this approach is that the data cannot be modified.


### overview: create a spatialVector

#### output to shapefile


```r
# load spatial vector object; because create_vector_shape will generate a new
# shapefile, we have complete flexibility over the shapefile name and
# manipulating the data - here we are starting with an existing shapefile named
# CORETT but will generate a shapefile with the name
# ejido_titles_points_of_decree

ejido_titles_points_of_decree <- sf::read_sf(
  dsn   = "data/Regularizacion/ejidal",
  layer = "CORETT"
  ) |>
formatting_or_data_manipuation...

# write attributes (and factors if relevant)

try(
  capeml::write_attributes(
    dfname    = ejido_titles_points_of_decree,
    overwrite = FALSE
  )
)

# generate a description for the data entity

ejido_titles_points_of_decree_desc <- "polygons of land regularized by the
National Agency, CORETT; polygons were georeferenced from 281 paper maps,
consolidated into 87 unique regularization degrees of ejidos that became
privitzed from 1987-2007; includes the area of each polygon, the date of
regularization, the name of the ejido, the delegation, and the 'plane number'
that could be used to find the original map file in the CORETT office; it only
includes expropriation for the delegations Xochimilco, Magdalena Contreras,
Iztapalapa, Tlahuac, Gustavo Madero, Cuajimalpa, Alvaro Obregon, Tlalpan,
Coyoacan, and Milpa Alta"

ejido_titles_points_of_decree_SV <- capemlGIS::create_vector_shape(
  vector_name   = ejido_titles_points_of_decree,
  description   = ejido_titles_points_of_decree_desc,
  coord_sys     = "WGS_1984_UTM_Zone_55N",
  layer_opts    = "SHPT=POLYGON",
  overwrite     = TRUE,
  projectNaming = TRUE,
  )

# The resulting spatialVector entity can be added to a EML dataset. Note also
# in this example that we are passing additional layer options, which
# ultimately feed to sf::st_write, necessary here to generate a multi-polygon
# shapefile.
```

#### output to kml

The workflow for writing to kml or GeoJSON is nearly identical to the workflow for writing to shapefile. Some differences include that we need (or, at least, should) have a `Name` field if writing to kml file that serves as a unique identifier for each data entity (added in the workflow below as an additional line in the call to `mutate`). Also, some parameters are different, such as unlike `create_vector_shape`, which requires the user to pass an EML-compliant coordinate reference system, since `create_vector` writes a new kml or GeoJSON file, the resulting CRS is *always* EPSG 4326 and thus hard-coded into the function.


```r
# load spatial vector object; because create_vector_shape will generate a new
# shapefile, we have complete flexibility over the shapefile name and
# manipulating the data - here we are starting with an existing shapefile named
# CORETT but will generate a shapefile with the name
# ejido_titles_points_of_decree 

ejido_titles_points_of_decree <- sf::read_sf(
  dsn   = "path-to-directory/",
  layer = "CORETT"
  ) |>
formatting_or_data_manipuation...

# write attributes (and factors if relevant)

try(write_attributes(ejido_titles_points_of_decree, overwrite = FALSE))

# generate a description for the data entity

ejido_titles_points_of_decree_desc <- "polygons of land regularized by the
National Agency, CORETT; polygons were georeferenced from 281 paper maps,
consolidated into 87 unique regularization degrees of ejidos that became
privatized from 1987-2007; includes the area of each polygon, the date of
regularization, the name of the ejido, the delegation, and the 'plane number'
that could be used to find the original map file in the CORETT office; it only
includes expropriation for the delegations Xochimilco, Magdalena Contreras,
Iztapalapa, Tlahuac, Gustavo Madero, Cuajimalpa, Alvaro Obregon, Tlalpan,
Coyoacan, and Milpa Alta"

ejido_titles_points_of_decree_SV <- capemlGIS::create_vector(
  vector_name   = ejido_titles_points_of_decree,
  description   = ejido_titles_points_of_decree_desc,
  driver        = "kml",
  overwrite     = TRUE,
  projectNaming = TRUE
  )

# The resulting spatialVector entity can be added to a EML dataset.
```
