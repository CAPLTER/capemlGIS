#' @title generate a EML entity of type spatialRaster
#'
#' @description create_spatialRaster generates a EML entity of type
#'   spatialRaster
#'
#' @details a spatialRaster entity is created from a single data file (e.g.,
#' CAP_1985.img). The resulting entity is renamed with the project id + base
#' file name + file extension or base file name + file extension depending on
#' desired output.
#'
#' @note create_spatialRaster will look for a package number in config.yaml;
#' congig.yaml::packageNum must exist in the project directory.
#'
#' @note EML requires that geographic extents are provided as decimal degrees.
#' Because it is often impractical or inadviseable to change the projection of
#' rasters, a spatial coverage is not constructed if the projection of a raster
#' is in units of meters.
#'
#' @param rasterFile
#'  (character) Quoted full path to raster file.
#' @param description
#'  (character) Description of the raster.
#' @param epsgProjection
#'  (integer) Four- or five-digit EPSG numeric code of raster Coordinate
#'  Reference System (CRS)
#' @param rasterValueDescription
#'  (character) Description of raster values
#' @param rasterValueUnits
#'  (character) Raster value units. Units must be EML-compliant or annotated by
#'  a custom unit definition.
#' @param geoDescription
#'  (character) A textual description of the geographic study area of the
#'  raster. This parameter allows the user to overwrite the
#'  geographicDesciption value provided in the project config.yaml.
#' @param projectNaming
#' (logical) Logical indicating if the raster file should be renamed per the
#' style used by the CAP LTER (default) with the project id + base file name +
#' file extension. The passed file or directory name will be used if this
#' parameter is set to FALSE.
#'
#' @import EML
#' @importFrom raster raster extent ncell crs bandnr nrow ncol xres yres
#' @importFrom tools md5sum file_ext
#' @importFrom utils read.csv
#' @importFrom yaml yaml.load_file
#'
#' @return EML spatial data object is returned. Additionally, if projectNaming
#' is set to TRUE (default) the spatial data file is renamed with the project
#' id + base file name + file extension (zip in the case when multiple files
#' are aggregated).
#'
#' @examples
#' \dontrun{
#'
#' rasterDesc <- "NDVI for the central Arizona region derived from 2015 NAIP
#' imagery. NAIP NDVI data are presented as a series of tiles each representing
#' a portion of the overall central Arizona coverage area. The relative position
#' of this tile to the entire coverage area is detailed in the files
#' NAIP_GRID.kml, NAIP_GRID.pdf, and NAIP_GRID.png included with this data set."
#'
#' my_area <- "one in a series of tiles covering the central-Arizona Phoenix
#' region" # supercedes yaml
#'
#' NAIP_NDVI_2015_SV <- capemlGIS::create_spatialRaster(
#'    rasterFile             = "path-to-file/NAIP_NDVI_2015.tiff",
#'    description            = rasterDesc,
#'    epsgProjection         = 4326,
#'    rasterValueDescription = "Normalized Difference Vegetation Index (NDVI)",
#'    rasterValueUnits       = "dimensionless",
#'    geoDescription         = "my_area",
#'    projectNaming          = FALSE
#'  )
#'
#' }
#'
#' @export

create_spatialRaster <- function(
  rasterFile,
  description,
  epsgProjection,
  rasterValueDescription,
  rasterValueUnits,
  geoDescription = NULL,
  projectNaming  = TRUE
  ) {


  # set options -------------------------------------------------------------

  options(scipen = 999)


  # required parameters -----------------------------------------------------

  # do not proceed if a description is not provided
  if (missing("description")) {

    stop("please provide a description for this raster")

  }

  # do not proceed if a description of the raster values is not provided
  if (missing("rasterValueDescription")) {

    stop("please provide a desription of the raster cell values")

  }

  # do not proceed if a epsg of EML-compliant projection is not provided
  if (missing("epsgProjection")) {

    stop("please provide a EPSG projection for this raster")

  }


  # load raster -------------------------------------------------------------

  raster_object <- raster::raster(rasterFile)


  # establish raster file parent directory ----------------------------------

  directory_name <- dirname(rasterFile)


  # build attribute table ---------------------------------------------------

  rasterFactorsFileName <- paste0(directory_name, "/", basename(file_path_sans_ext(rasterFile)), "_factors.csv")

  # compile components for attributeList of dataTable

  # condition: factors present
  if (file.exists(rasterFactorsFileName)) {

    rasterAttributes <- data.frame(
      attributeName       = "raster_value",
      attributeDefinition = rasterValueDescription
    )

    rasterFactors <- utils::read.csv(rasterFactorsFileName)

    attr_list <- EML::set_attributes(
      attributes  = rasterAttributes,
      factors     = rasterFactors,
      col_classes = "factor"
    )

    # condition: factors not present (presuming that vars are not categorical)

  } else {

    # do not proceed if the units for the rater values is not provided
    if (missing("rasterValueUnits")) { stop("please provide units for the raster cell values") }

    # build base attributes data frame
    rasterAttributes <- data.frame(
      attributeName       = "raster_value",
      attributeDefinition = rasterValueDescription,
      unit                = rasterValueUnits
    )

    # determine raster value number type
    # this code is run only if the raster is a reasonable size (<= 500 Mb)
    if (file.size(rasterFile) <= 524288000) {

      # determine raster number type
      # sample of raster values (20% of values sans NAs)
      rasterValuesSample <- na.omit(sample(raster_object, size = 0.2 * raster::ncell(raster_object)))
      rasterValuesSample <- rasterValuesSample[is.finite(rasterValuesSample)] # remove infs (just in case)

      rounded <- floor(rasterValuesSample)

      if (length(rasterValuesSample) - sum(rasterValuesSample == rounded, na.rm = T) > 0) {

        rasterNumberType <- "real" # all

      } else if (min(rasterValuesSample, na.rm = T) > 0) {

        rasterNumberType <- "natural" # 1, 2, 3, ... (sans 0)

      } else if (min(rasterValuesSample, na.rm = T) < 0) {

        rasterNumberType <- "integer" # whole + negative values

      } else {

        rasterNumberType <- "whole" # natural + 0

      }

      rasterAttributes$numberType <- rasterNumberType

    } else {

      rasterAttributes$numberType <- "real"

    } # close raster value number type

    attr_list <- EML::set_attributes(
      attributes  = rasterAttributes,
      col_classes = "numeric"
    )

  } # close condition: factors not present

  # add additionalInfo - projections ----------------------------------------

  projections <- list(
    section = list(
      paste0("<title>user-provided coordinate reference system</title>\n<para>", epsgProjection, "</para>"),
      paste0("<title>raster-derived coordinate reference system</title>\n<para>", as.character(crs(raster_object)), "</para>")
    )
  )

  message(
    paste0("user-provided CRS: ", epsgProjection, "\n"),
    paste0("raster-derived CRS: ", as.character(raster::crs(raster_object)))
  )

  # identify EML-compliant spatial reference --------------------------------

  if (!epsgProjection %in% eml_valid_crs$epsg) {

    stop("cannot identify EML-compliant projection for EPSG: ", epsgProjection)

  }

  emlProjection <- eml_valid_crs[eml_valid_crs$epsg == epsgProjection,]$value


  # create spatial raster entity --------------------------------------------

  # set authentication (md5)
  fileAuthentication                <- EML::eml$authentication(method = "MD5")
  fileAuthentication$authentication <- md5sum(rasterFile)

  # set file size
  fileSize      <- EML::eml$size(unit = "byte")
  fileSize$size <- deparse(file.size(rasterFile))

  # set file format
  fileDataFormat <- EML::eml$dataFormat(
    externallyDefinedFormat = EML::eml$externallyDefinedFormat(formatName = file_ext(rasterFile))
  )

  rasterBaseName    <- basename(rasterFile)
  directory_name    <- dirname(rasterFile)
  directoryNameFull <- sub("/$", "", path.expand(directory_name))
  pathToFile        <- path.expand(rasterFile)

  if (projectNaming == TRUE) {

    # if using project naming, add project-name specific elements to
    # spatialRaster entity

    # retrieve package number from config.yaml
    if (!file.exists("config.yaml")) {
      stop("config.yaml not found")
    }

    packageNum <- yaml::yaml.load_file("config.yaml")$packageNum

    newRasterName <- paste0(
      packageNum, "_",
      file_path_sans_ext(rasterBaseName), ".",
      file_ext(rasterFile)
    )

    newRasterNameDir <- paste0(directoryNameFull, "/", newRasterName)

    file.copy(
      from = rasterFile,
      to   = newRasterNameDir
    )

    # identifier
    resource_ident <- newRasterName

    # close projectNaming == TRUE

  } else {

    # if not using not project naming, add source-name specific elements to
    # spatialRaster entity

    # identifier
    resource_ident <- rasterBaseName

  } # close projectNaming == FALSE

# distribution

fileURL <- yaml::yaml.load_file("config.yaml")$baseURL

fileDistribution <- EML::eml$distribution(
  EML::eml$online(url = paste0(fileURL, resource_ident))
)

# build physical

spatialRasterPhysical <- EML::eml$physical(
  objectName     = resource_ident,
  authentication = fileAuthentication,
  size           = fileSize,
  dataFormat     = fileDataFormat,
  distribution   = fileDistribution
)

# build spatialRaster

newSR <- EML::eml$spatialRaster(
  entityName          = resource_ident, # diff
  entityDescription   = description,
  physical            = spatialRasterPhysical,
  additionalInfo      = projections,
  attributeList       = attr_list,
  spatialReference    = EML::eml$spatialReference(
    horizCoordSysName = emlProjection
    ),
  numberOfBands       = raster::bandnr(raster_object),
  rows                = raster::nrow(raster_object),
  columns             = raster::ncol(raster_object),
  horizontalAccuracy  = EML::eml$horizontalAccuracy(accuracyReport = "METADATA_NOT_PROVIDED"),
  verticalAccuracy    = EML::eml$verticalAccuracy(accuracyReport = "METADATA_NOT_PROVIDED"),
  cellSizeXDirection  = raster::xres(raster_object),
  cellSizeYDirection  = raster::yres(raster_object),
  rasterOrigin        = "Upper Left",
  verticals           = 1,
  cellGeometry        = "pixel",
  id                  = resource_ident # diff
)

# add (spatial) coverage --------------------------------------------------

# EML requires that geographic extents are provided as decimal degrees; a
# spatial coverage is not constructed for rasters is in units of meters.

if (abs(raster::extent(raster_object)@xmin) > 180) {

  warning("projection in meters, spatial coverage not constructed")

} else {

  if (!is.null(geoDescription)) {

    # retrieve geographic description from config.yaml
    if (!file.exists("config.yaml")) {

      stop("could not locate geographic description, config.yaml not found")

    }

    geoDesc <- yaml::yaml.load_file("config.yaml")$geographicCoverage$geographicDescription

    if (is.na(geoDesc) | is.null(geoDesc) | geoDesc == "") {

      warning("geographic description provided in config.yaml is empty")

    }

  } else {

    geoDesc <- geoDescription

  }

  rasterSpatialCoverage <- list(
    geographicDescription = geoDesc,
    boundingCoordinates   = list(
      westBoundingCoordinate  = raster::extent(raster_object)@xmin,
      eastBoundingCoordinate  = raster::extent(raster_object)@xmax,
      northBoundingCoordinate = raster::extent(raster_object)@ymax,
      southBoundingCoordinate = raster::extent(raster_object)@ymin
    )
  )

  newSR$coverage$geographicCoverage <- rasterSpatialCoverage

}


# closing message ---------------------------------------------------------

message(paste0("created spatialRaster: ", resource_ident))


# return spatial raster object --------------------------------------------

return(newSR)

} # close create_spatialRaster
