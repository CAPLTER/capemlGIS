---
title: "create_spatialRaster"
author: "S. Earl"
date: "2021-12-10"
output: 
  rmarkdown::html_vignette:
    keep_md: true
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

### overview

The capemlGIS package provides the user considerable flexibility to process spatial
raster data and generate EML spatial raster metadata. 

Of note, the package allows for processing an individual raster file (e.g.,
xxx.tiff) or a raster file with supporting metadata (e.g., as xml or otherwise)
in a zipped directory. A preffered approach is likely to publish the raster
data file as a spatialRaster entity, and supporting documents (e.g., a color
profile) as otherEntities separately but zipping all entities together in a
single otherEntity is provided as a processing option. In the case of zipping
multiple files, all files in the parent directory where the raster file is
located are aggregated into a single compressed (zipped) file. If projectNaming
is enabled (default), the resulting entity is renamed with the project id +
base file name + file extension (zip in the case when multiple files
are aggregated).

Note that the ability to zip related files was dropped from `create_spatialRaster()`; `create_spatialRaster()` is now used only to process a single raster file; the ability to zip related files will be available as a new function in a future release.

The `create_spatialRaster()` function takes the following arguments:

- rasterFile
 (character) Quoted full path to raster file.
- description
 (character) Description of the raster.
- epsgProjection
 (integer) Four- or five-digit EPSG numeric code of raster Coordinate
 Reference System (CRS)
- rasterValueDescription
 (character) Description of raster values
- rasterValueUnits
 (character) Raster value units. Units must be EML-compliant or annotated by
 a custom unit definition.
- geoDescription
 (character) A textual description of the geographic study area of the raster.
 This parameter allows the user to overwrite the geographicDesciption value
 provided in the project config.yaml.
- projectNaming
(logical) Logical indicating if the raster file should be renamed per the style
used by the CAP LTER (default) with the project id + base file name + file
extension. The passed file or directory name will be used if this parameter is
set to FALSE.

The raster file is read into the R environment where select metadata are
extracted using functions from the raster package.

### spatial projection

A projection is required for EML spatialRaster entities. This critical piece of
metadata is provided by supplying the numeric epsg code of the projection
(e.g., 4326 for WGS 1984). Ultimately, this must value must be paired to an
EML-compliant projection name (see list
[here](https://raw.githubusercontent.com/NCEAS/eml/main/xsd/eml-spatialReference.xsd)),
which the package will attempt to match but stop if a match is not available.
The package has a limited number of epsg codes matched to EML-compliant
projection names, mostly those commonly used for the southwestern USA
investigators. The function will stop if a match cannot be identified, and the
user should contact the package administrator to add the needed projection.

### harvested raster metadata

In addition to the user-supplied metadata, the raster will be read into the R
environment (with the
[raster](https://cran.r-project.org/web/packages/raster/index.html) package)
from which additional metadata will be harvested, including: raster value
number type (only for rasters < 500 Mb), raster extents, number of raster
bands, and cell size. The `horizontalAccuracy` and `verticalAcurracy`
attributes are required by EML but, as these are generally not known, the
string `METADATA_NOT_PROVIDED` is provided as content for those two attributes.
Resulting EML should be hand-edited to adjust those values.

Note that EML requires that geographic extents are provided as decimal degrees.
Because it is often impractical or inadviseable to change the projection of
rasters, a spatial coverage is not constructed if the projection of a raster is
in units of meters.

### categorical raster values

If the raster values are categorical, generate a metadata file to catalog the
unique raster value categories and their meaning using the
`write_raster_factors()` function.

*Raster categorical values metadata example*

|rasterValue | categoryName |
|:-----------|:-------------|
|1 | Water |
|2 | Asphalt/Road |
|3 | Concrete/Buildings |
|4 | Urban mixture |
|5 | Residential |
|6 | Residential (white rooftops) |
|7 | Active crop |
|8 | Inactive crop |
|9 | Cultivated vegetation |
|10 | Natural vegetation |
|11 | Soil/Desert |


### calling the function

Call the `create_spatialRaster()` function to generate the EML to describe the
raster. Arguments include the quoted full or relative path to the raster file
(or files), the quoted full or relative path and name of the raster metadata
file, and the quoted full or relative path and name of the raster value
categories (if needed). Output of the function yields EML that can be
incorporated into the metadata for a data set.


```r
raster_desc <- "NDVI for the central Arizona region derived from 2015 NAIP
imagery. NAIP NDVI data are presented as a series of tiles each representing
a portion of the overall central Arizona coverage area. The relative position
of this tile to the entire coverage area is detailed in the files
NAIP_GRID.kml, NAIP_GRID.pdf, and NAIP_GRID.png included with this data set."

my_area <- "one in a series of tiles covering the central-Arizona Phoenix
region"

NAIP_NDVI_2015_SV <- capemlGIS::create_spatialRaster(
   rasterFile             = "path-to-file/NAIP_NDVI_2015.tiff",
   description            = raster_desc,
   epsgProjection         = 4326,
   rasterValueDescription = "Normalized Difference Vegetation Index (NDVI)",
   rasterValueUnits       = "dimensionless",
   geoDescription         = "my_area",
   projectNaming          = FALSE
 )
```

