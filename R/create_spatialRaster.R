#' @title create_spatialRaster
#'
#' @description create_spatialRaster generates a EML entity of type
#'   spatialRaster
#'
#' @details a spatialRaster entity is created from a single data file (e.g.,
#'  CAP_1985.img) or a collection of related files (e.g., CAP_1985.img,
#'  CAP_1985.img.aux.xml). In the case of multiple files, all files in the
#'  parent directory where the raster file is located are aggregated into a
#'  single compressed (zipped) file. In all cases, the resulting entity is
#'  renamed with the project id + base file name + md5sum + file extension (zip
#'  in the case when multiple files are aggregated).
#' @note create_spatialRaster will look for a package number in config.yaml;
#'  congig.yaml::packageNum must exist in the project directory.
#'
#' @param rasterFile
#'  (character) Quoted full path to raster file.
#' @param description
#'  (character) Description of the raster.
#' @param epsgProjection
#'  (integer) EPSG numeric code of raster coordinate reference system
#' @param rasterValueDescription
#'  (character) Description of raster values
#' @param rasterValueUnits
#'  (character) Raster value units
#' @param geoDescription
#'  (character) A textual description of the geographic study area of the
#'  raster. This parameter allows the user to overwrite the
#'  geographicDesciption value provided in the project config.yaml.
#' @param zipFiles
#'  (logical) Logical indicating whether spatial raster entity should be
#'  constructed from a single raster file (FALSE, default) or entire directory
#'  (TRUE)
#' @param baseURL
#'  (character) The base path of the web-accessible location of the data file;
#'  the name of the resulting file will be passed to the base path to generate
#'  a web-resolvable file path. This parameter is required with the default set
#'  to the CAP LTER file path
#' @param projectNaming
#'  (logical) Logical indicating if the raster file (or parent directory if
#'  zipFiles == TRUE) should be renamed per the style used by the CAP LTER
#'  (default) with the project id + base file name + md5sum + file extension.
#'  The passed file or directory name will be used if this parameter is set to
#'  FALSE.
#'
#' @import EML
#' @import dplyr
#' @import raster
#' @importFrom tools md5sum file_ext
#' @importFrom utils read.csv
#' @importFrom yaml yaml.load_file
#'
#' @return EML spatial data object is returned. Additionally, if projectNaming
#'  is set to TRUE (default) the spatial data file is renamed with the project
#'  id + base file name + md5sum + file extension (zip in the case when
#'  multiple files are aggregated).
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
#' geoDesc <- "one in a series of tiles covering the central-Arizona Phoenix
#' region"
#'
#' NAIP_NDVI_2015_SV <- create_spatialRaster(
#'    rasterFile = "path-to-file/NAIP_NDVI_2015.tiff",
#'    description = rasterDesc,
#'    epsgProjection = 4326,
#'    rasterValueDescription = "Normalized Difference Vegetation Index (NDVI)",
#'    rasterValueUnits = "dimensionless",
#'    geoDescription = geoDesc,
#'    projectNaming = FALSE)
#' }
#'
#' @export

create_spatialRaster <- function(
  rasterFile,
  description,
  epsgProjection,
  rasterValueDescription,
  rasterValueUnits,
  zipFiles = FALSE,
  geoDescription,
  baseURL = "https://data.gios.asu.edu/datasets/cap/",
  projectNaming = TRUE) {


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

  rasterObject <- raster(rasterFile)


  # establish raster file parent directory ----------------------------------

  directoryName <- dirname(rasterFile)


  # build attribute table ---------------------------------------------------

  rasterFactorsFileName <- paste0(directoryName, "/", basename(file_path_sans_ext(rasterFile)), "_factors.csv")

  # compile components for attributeList of dataTable

  # condition: factors present
  if (file.exists(rasterFactorsFileName)) {

    rasterAttributes <- data.frame(
      attributeName = "raster_value",
      attributeDefinition = rasterValueDescription
    )

    rasterFactors <- utils::read.csv(rasterFactorsFileName)

    attr_list <- EML::set_attributes(
      attributes = rasterAttributes,
      factors = rasterFactors,
      col_classes = "factor"
    )

    # condition: factors not present (presuming that vars are not categorical)
  } else {

    # do not proceed if the units for the rater values is not provided
    if (missing("rasterValueUnits")) { stop("please provide units for the raster cell values") }

    # build base attributes data frame
    rasterAttributes <- data.frame(
      attributeName = "raster_value",
      attributeDefinition = rasterValueDescription,
      unit = rasterValueUnits
    )

    # determine raster value number type
    # this code is run only if the raster is a reasonable size (<= 500 Mb)
    if (file.size(rasterFile) <= 524288000) {

      # determine raster number type
      # sample of raster values (20% of values sans NAs)
      rasterValuesSample <- na.omit(sample(rasterObject, size = 0.2 * ncell(rasterObject)))
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
      attributes = rasterAttributes,
      col_classes = "numeric"
    )

  } # close condition: factors not present


  # add additionalInfo - projections ----------------------------------------

  projections <- list(
    section = list(
      paste0("<title>user-provided coordinate reference system</title>\n<para>", epsgProjection, "</para>"),
      paste0("<title>raster-derived coordinate reference system</title>\n<para>", as.character(crs(rasterObject)), "</para>")
    )
  )

  message(
    paste0("user-provided CRS: ", epsgProjection, "\n"),
    paste0("raster-derived CRS: ", as.character(crs(rasterObject)))
  )

  # identify EML-compliant spatial reference --------------------------------

  if (epsgProjection == 26912) {

    emlProjection <- "NAD_1983_UTM_Zone_12N"

  } else if (epsgProjection == 26712) {

    emlProjection <- "NAD_1927_UTM_Zone_12N"

  } else if (epsgProjection == 4326) {

    emlProjection <- "GCS_WGS_1984"

  } else if (epsgProjection == 32612) {

    emlProjection <- "WGS_1984_UTM_Zone_12N"

  } else {

    stop("cannot identify EML-compliant projection, contact package developer")

  }


  # coverage ----------------------------------------------------------------

  if (missing("geoDescription")) {

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

#   rasterSpatialCoverage  <- EML::set_coverage(
#     geographicDescription = geoDesc,
#     west = raster::extent(rasterObject)@xmin,
#     east = raster::extent(rasterObject)@xmax,
#     north = raster::extent(rasterObject)@ymax,
#     south = raster::extent(rasterObject)@ymin
#   )

  rasterSpatialCoverage <- list(
    geographicDescription = geoDesc,
    boundingCoordinates = list(
      westBoundingCoordinate = raster::extent(rasterObject)@xmin,
      eastBoundingCoordinate = raster::extent(rasterObject)@xmax,
      northBoundingCoordinate = raster::extent(rasterObject)@ymax,
      southBoundingCoordinate = raster::extent(rasterObject)@ymin
    )
  )



  # create spatial raster entity --------------------------------------------

  # if zipping a directory
  if (zipFiles == TRUE) {

    # zip directory
    directoryNameFull <- sub("/$", "", path.expand(directoryName))
    zippedDirName <- paste0(directoryNameFull, ".zip")
    zipShell <- paste0("zip -jX ", zippedDirName, " ", directoryNameFull, "/*")
    system(zipShell)

    zipBaseName <- basename(zippedDirName)

    # set authentication (md5)
    fileAuthentication <- EML::eml$authentication(method = "MD5")
    fileAuthentication$authentication <- md5sum(zippedDirName)

    # set file size
    fileSize <- EML::eml$size(unit = "byte")
    fileSize$size <- deparse(file.size(zippedDirName))

    # set file format
    fileDataFormat <- EML::eml$dataFormat(
      externallyDefinedFormat = EML::eml$externallyDefinedFormat(formatName = "application/zip")
    )

    if (projectNaming == TRUE) {

      # if using project naming, add project-name specific elements to
      # spatialRaster entity

      # retrieve package number from config.yaml
      if (!file.exists("config.yaml")) {
        stop("config.yaml not found")
      }

      packageNum <- yaml::yaml.load_file("config.yaml")$packageNum

      # rename zipped dir with md5sum
      zipHashName <- paste0(
        packageNum, "_",
        file_path_sans_ext(basename(zippedDirName)), "_",
        md5sum(zippedDirName),
        ".zip"
      )
      zipHashDirName <- paste0(dirname(directoryName), "/", zipHashName)
      renameShell <- paste0("mv ", zippedDirName, " ", zipHashDirName)
      system(renameShell)

      # set distribution
      fileDistribution <- EML::eml$distribution(
        EML::eml$online(url = paste0(baseURL, zipHashName))
      )

      # build physical
      spatialRasterPhysical <- EML::eml$physical(
        objectName = zipHashName,
        authentication = fileAuthentication,
        size = fileSize,
        dataFormat = fileDataFormat,
        distribution = fileDistribution
      )

      newSR <- EML::eml$spatialRaster(
        entityName = zipHashName,
        entityDescription = description,
        physical = spatialRasterPhysical,
        #         coverage = spatialCoverage,
        additionalInfo = projections,
        attributeList = attr_list,
        spatialReference = EML::eml$spatialReference(
          horizCoordSysName = emlProjection
        ),
        numberOfBands = bandnr(rasterObject),
        rows = nrow(rasterObject),
        columns = ncol(rasterObject),
        horizontalAccuracy = EML::eml$horizontalAccuracy(accuracyReport = "METADATA_NOT_PROVIDED"),
        verticalAccuracy = EML::eml$verticalAccuracy(accuracyReport = "METADATA_NOT_PROVIDED"),
        cellSizeXDirection = xres(rasterObject),
        cellSizeYDirection = yres(rasterObject),
        rasterOrigin = "Upper Left",
        verticals = 1,
        cellGeometry = "pixel",
        id = zipHashName
      )

      # close projectNaming == TRUE
    } else {

      # if not using not project naming, add source-name specific elements to
      # spatialRaster entity

      # set distribution
      fileDistribution <- EML::eml$distribution(
        EML::eml$online(url = paste0(baseURL, zipBaseName))
      )

      # build physical
      spatialRasterPhysical <- EML::eml$physical(
        objectName = zipBaseName,
        authentication = fileAuthentication,
        size = fileSize,
        dataFormat = fileDataFormat,
        distribution = fileDistribution
      )

      newSR <- EML::eml$spatialRaster(
        entityName = zipBaseName,
        entityDescription = description,
        physical = spatialRasterPhysical,
        #         coverage = spatialCoverage,
        additionalInfo = projections,
        attributeList = attr_list,
        spatialReference = EML::eml$spatialReference(
          horizCoordSysName = emlProjection
        ),
        numberOfBands = bandnr(rasterObject),
        rows = nrow(rasterObject),
        columns = ncol(rasterObject),
        horizontalAccuracy = EML::eml$horizontalAccuracy(accuracyReport = "METADATA_NOT_PROVIDED"),
        verticalAccuracy = EML::eml$verticalAccuracy(accuracyReport = "METADATA_NOT_PROVIDED"),
        cellSizeXDirection = xres(rasterObject),
        cellSizeYDirection = yres(rasterObject),
        rasterOrigin = "Upper Left",
        verticals = 1,
        cellGeometry = "pixel",
        id = zipBaseName
      )

    } # close projectNaming == FALSE

    # close if zipping a directory
  } else {

    # if working with a raster file (i.e., not zipping a directory)

    # set authentication (md5)
    fileAuthentication <- EML::eml$authentication(method = "MD5")
    fileAuthentication$authentication <- md5sum(rasterFile)

    # set file size
    fileSize <- EML::eml$size(unit = "byte")
    fileSize$size <- deparse(file.size(rasterFile))

    # set file format
    fileDataFormat <- EML::eml$dataFormat(
      externallyDefinedFormat = EML::eml$externallyDefinedFormat(formatName = file_ext(rasterFile))
    )

    rasterBaseName <- basename(rasterFile)
    directoryName <- dirname(rasterFile)
    directoryNameFull <- sub("/$", "", path.expand(directoryName))
    pathToFile <- path.expand(rasterFile)

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
        file_path_sans_ext(rasterBaseName), "_",
        md5sum(rasterFile), ".",
        file_ext(rasterFile)
      )

      newRasterNameDir <- paste0(directoryNameFull, "/", newRasterName)

      file.copy(
        from = rasterFile,
        to = newRasterNameDir
      )

      # set distribution
      fileDistribution <- EML::eml$distribution(
        EML::eml$online(url = paste0(baseURL, newRasterName))
      )

      # build physical
      spatialRasterPhysical <- EML::eml$physical(
        objectName = newRasterName,
        authentication = fileAuthentication,
        size = fileSize,
        dataFormat = fileDataFormat,
        distribution = fileDistribution
      )

      # build spatialRaster
      newSR <- EML::eml$spatialRaster(
        entityName = newRasterName,
        entityDescription = description,
        physical = spatialRasterPhysical,
        #         coverage = spatialCoverage,
        additionalInfo = projections,
        attributeList = attr_list,
        spatialReference = EML::eml$spatialReference(
          horizCoordSysName = emlProjection
        ),
        numberOfBands = bandnr(rasterObject),
        rows = nrow(rasterObject),
        columns = ncol(rasterObject),
        horizontalAccuracy = EML::eml$horizontalAccuracy(accuracyReport = "METADATA_NOT_PROVIDED"),
        verticalAccuracy = EML::eml$verticalAccuracy(accuracyReport = "METADATA_NOT_PROVIDED"),
        cellSizeXDirection = xres(rasterObject),
        cellSizeYDirection = yres(rasterObject),
        rasterOrigin = "Upper Left",
        verticals = 1,
        cellGeometry = "pixel",
        id = newRasterName
      )

      # close projectNaming == TRUE

    } else {

      # if not using not project naming, add source-name specific elements to
      # spatialRaster entity

      # set distribution
      fileDistribution <- EML::eml$distribution(
        EML::eml$online(url = paste0(baseURL, rasterBaseName))
      )

      # build physical
      spatialRasterPhysical <- EML::eml$physical(
        objectName = rasterBaseName,
        authentication = fileAuthentication,
        size = fileSize,
        dataFormat = fileDataFormat,
        distribution = fileDistribution
      )

      # build spatialRaster
      newSR <- EML::eml$spatialRaster(
        entityName = rasterBaseName,
        entityDescription = description,
        physical = spatialRasterPhysical,
        #         coverage = spatialCoverage,
        additionalInfo = projections,
        attributeList = attr_list,
        spatialReference = EML::eml$spatialReference(
          horizCoordSysName = emlProjection
        ),
        numberOfBands = bandnr(rasterObject),
        rows = nrow(rasterObject),
        columns = ncol(rasterObject),
        horizontalAccuracy = EML::eml$horizontalAccuracy(accuracyReport = "METADATA_NOT_PROVIDED"),
        verticalAccuracy = EML::eml$verticalAccuracy(accuracyReport = "METADATA_NOT_PROVIDED"),
        cellSizeXDirection = xres(rasterObject),
        cellSizeYDirection = yres(rasterObject),
        rasterOrigin = "Upper Left",
        verticals = 1,
        cellGeometry = "pixel",
        id = rasterBaseName
      )

    } # close projectNaming == FALSE

  } # if working with a raster file (i.e., not zipping a directory)


  # add (spatial) coverage --------------------------------------------------

  # EML requires that geographic extents are provided as decimal degrees.
  # Because it is impractical to project some rasters, if the projection of a
  # raster is in units of meters, a spatial coverage is not constructed.

  if (abs(raster::extent(rasterObject)@xmin) > 180) {

    warning("projection in meters, coverage not constructed")

  } else {

    newSR$coverage$geographicCoverage <- rasterSpatialCoverage

  }


  # closing message ---------------------------------------------------------

  message("spatialRaster created")

  # return spatial raster object --------------------------------------------

  return(newSR)

} # close create_spatialRaster
