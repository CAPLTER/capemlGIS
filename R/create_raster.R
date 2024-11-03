#' @title generate a EML entity of type spatialRaster
#'
#' @description \code{create_raster} generates a EML entity of type
#' spatialRaster
#'
#' @details a spatialRaster entity is created from a single data file (e.g.,
#' CAP_1985.img). The resulting entity is renamed with the project id + base
#' file name + file extension or base file name + file extension depending on
#' desired output.
#'
#' @note EML requires that geographic extents are provided as decimal degrees.
#' Because it is often impractical or inadviseable to change the projection of
#' rasters, a spatial coverage is not constructed if the projection of a raster
#' is in units of meters.
#'
#' @param raster_file
#'  (character) Quoted full path to raster file.
#' @param description
#'  (character) Description of the raster.
#' @param epsg
#'  (integer) Four- or five-digit EPSG numeric code of raster Coordinate
#'  Reference System (CRS)
#' @param raster_value_description
#'  (character) Description of raster values
#' @param raster_value_units
#'  (character) Raster value units. Units must be EML-compliant or annotated by
#'  a custom unit definition. This argument is ignored for categorical values
#'  (i.e., as indicated by a corresponding `_factors.yaml` file).
#' @param geographic_description
#'  (character) A textual description of the geographic study area of the
#'  raster. This parameter allows the user to overwrite the study-wide
#'  geographic_description value provided in config.yaml (default).
#' @param project_naming
#'  (logical) Logical indicating if the raster file should be renamed per the
#'  style: project id + base file name + file extension. If true,
#'  \code{create_raster} will look for a package identifer in
#'  config.yaml.
#' @param file_url
#'  (character) Optional parameter detailing the online location where the data
#'  entity can be accessed. In most cases, the online location is the same for
#'  all files in the dataset and is detailed in config.yaml. \code{file_url}
#'  allows the user to overide project-level configuration in config.yaml and
#'  provide a specific, unique resource (e.g., a link to a file in Dropbox) for
#'  each data entity.
#'
#' @import EML
#' @importFrom raster raster extent ncell crs bandnr nrow ncol xres yres
#' @importFrom tools md5sum file_ext
#' @importFrom utils read.csv
#' @importFrom yaml yaml.load_file
#' @importFrom capeml read_package_configuration
#'
#' @return EML spatial data object is returned. Additionally, if project_naming
#' is set to TRUE the spatial data file is renamed with the project
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
#' NAIP_NDVI_2015_SV <- capemlGIS::create_raster(
#'    raster_file               = "path-to-file/NAIP_NDVI_2015.tiff",
#'    description               = rasterDesc,
#'    epsg                      = 4326,
#'    raster_value_description  = "Normalized Difference Vegetation Index (NDVI)",
#'    raster_value_units        = "UNITLESS",
#'    geographic_description    = "my_area",
#'    project_naming            = FALSE
#'  )
#'
#' }
#'
#' @export
#'
create_raster <- function(
  raster_file,
  description,
  epsg,
  raster_value_description,
  raster_value_units,
  geographic_description = NULL,
  project_naming         = FALSE,
  file_url               = NULL
  ) {


  # set options -------------------------------------------------------------

  options(scipen = 999)


  # required parameters -----------------------------------------------------

  if (missing("description")) {

    stop("please provide a description for this raster")

  }

  # if (missing("raster_value_description")) {
  #
  #   stop("please provide a desription of the raster cell values")
  #
  # }

  if (missing("epsg")) {

    stop("please provide a EPSG projection for this raster")

  }


  # load raster ----------------------------------------------------------------

  this_raster <- suppressWarnings(raster::raster(raster_file))
  entity_name <- basename(tools::file_path_sans_ext(raster_file))


  # attributes -----------------------------------------------------------------

  # raster factor yaml metadata present ~ values are categorical:

  raster_factors_file_name <- paste0(entity_name, "_factors.yaml")

  if (file.exists(raster_factors_file_name)) {

    raster_factors <- yaml.load_file(raster_factors_file_name) |>
      yaml::yaml.load() |>
      tibble::enframe() |>
      tidyr::unnest_wider(value) |>
      tidyr::unnest_wider(attribute) |>
      tidyr::unnest_longer(levels) |>
      tidyr::unnest_wider(levels) |>
      dplyr::select(-one_of("name"))

    raster_attributes <- data.frame(
      attributeName       = raster_factors[["attributeName"]][[1]],
      attributeDefinition = raster_value_description
    )

    attr_list <- EML::set_attributes(
      attributes  = raster_attributes,
      factors     = raster_factors,
      col_classes = "factor"
    )

  }


  # raster factor yaml not present ~ values are not categorical:

  raster_attributes_file_name <- paste0(entity_name, "_attrs.yaml")

  if (file.exists(raster_attributes_file_name)) {

    # attributes <- capeml::read_attributes(
    #   entity_name        = entity_name,
    #   missing_value_code = NULL,
    #   entity_id          = tools::md5sum(project_name)
    #   )[["eml"]]

    attrs <- yaml::yaml.load_file(raster_attributes_file_name)

    attrs <- yaml::yaml.load(attrs)
    attrs <- tibble::enframe(attrs) |>
      tidyr::unnest_wider(value) |>
      dplyr::select(-one_of("name"))


    # column classes to vector (req'd by set_attributes)
    classes <- attrs |>
      dplyr::pull(columnClasses)

    # copy attributeDefinition to defintion as appropriate; remove col classes
    # from attrs (req'd by set_attributes); remove empty columns (targets here
    # are max and min values, which can throw an error for data without any
    # numeric columns) empty strings to NA

    attrs[attrs == ""] <- NA

    # helper function to remove missing columns
    not_all_na <- function(x) {
      !all(is.na(x))
    }

    entity_id <- tools::md5sum(raster_file)

    attrs <- attrs |>
      dplyr::mutate(
        id         = paste0(entity_id, "_", row.names(attrs)),
        definition = NA_character_,
        definition = dplyr::case_when(
          grepl("character", columnClasses) & ((is.na(definition) | definition == "")) ~ attributeDefinition,
          TRUE ~ definition
        )
      ) |>
      dplyr::select(-columnClasses) |>
      dplyr::select_if(not_all_na)

    attr_list <- EML::set_attributes(
      attributes    = attrs,
      col_classes   = classes
    )


  } else {

    if (missing("raster_value_units")) { stop("missing units for raster cell values") }

    raster_attributes <- data.frame(
      attributeName       = "raster_value",
      attributeDefinition = raster_value_description,
      unit                = raster_value_units
    )

    # determine raster value number type (run only if file <= 500 Mb)

    if (file.size(raster_file) <= 524288000) {

      # determine raster number type by sampling 20% of values sans NAs

      number_raster_cells <- this_raster@ncols * this_raster@nrows

      rasterValuesSample <- na.omit(sample(x = this_raster, size = 0.2 * number_raster_cells))
      rasterValuesSample <- rasterValuesSample[is.finite(rasterValuesSample)]

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

      raster_attributes$numberType <- rasterNumberType

    } else {

      raster_attributes$numberType <- "real"

    } # close raster value number type

    attr_list <- EML::set_attributes(
      attributes  = raster_attributes,
      col_classes = "numeric"
    )

  } # close condition: factors not present


  # additionalInfo ------------------------------------------------------------

  this_projection <- raster::projection(this_raster)

  projections <- list(
    section = list(
      paste0("<title>user-provided coordinate reference system</title>\n<para>", epsg, "</para>"),
      paste0("<title>raster-derived projection</title>\n<para>", as.character(this_projection), "</para>")
    )
  )

  message(
    "user-provided CRS: ", epsg, "\n",
    "raster-derived projection: ", as.character(this_projection)
  )


  # identify EML-compliant spatial reference --------------------------------

  if (!epsg %in% capemlGIS::eml_valid_crs$epsg) {

    stop(epsg, " is not a EML-compliant projection")

  }

  emlProjection <- capemlGIS::eml_valid_crs[capemlGIS::eml_valid_crs$epsg == epsg,]$value


  # create spatial raster entity --------------------------------------------

  fileAuthentication                <- EML::eml$authentication(method = "MD5")
  fileAuthentication$authentication <- tools::md5sum(raster_file)

  fileSize      <- EML::eml$size(unit = "byte")
  fileSize$size <- deparse(file.size(raster_file))

  fileDataFormat <- EML::eml$dataFormat(
    externallyDefinedFormat = EML::eml$externallyDefinedFormat(formatName = file_ext(raster_file))
  )

  rasterBaseName    <- basename(raster_file)
  directory_name    <- dirname(raster_file)
  directoryNameFull <- sub("/$", "", path.expand(directory_name))
  pathToFile        <- path.expand(raster_file)


  # retrieve dataset details from config.yaml

  configurations <- capeml::read_package_configuration()


  # project naming

  if (project_naming == TRUE) {

    newRasterName <- paste0(
      configurations$identifier, "_",
      file_path_sans_ext(rasterBaseName), ".",
      file_ext(raster_file)
    )

    newRasterNameDir <- paste0(directoryNameFull, "/", newRasterName)

    file.copy(
      from = raster_file,
      to   = newRasterNameDir
    )

    resource_ident <- newRasterName

  } else {

    resource_ident <- rasterBaseName

  }


  # distribution

  if (!is.null(file_url)) {

    fileDistribution <- EML::eml$distribution(
      EML::eml$online(url = file_url)
    )

  } else {

    fileURL <- configurations$fileURL

    fileDistribution <- EML::eml$distribution(
      EML::eml$online(url = paste0(fileURL, resource_ident))
    )

  }


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
    entityName          = resource_ident,
    entityDescription   = description,
    physical            = spatialRasterPhysical,
    additionalInfo      = projections,
    attributeList       = attr_list,
    spatialReference    = EML::eml$spatialReference(
      horizCoordSysName = emlProjection
      ),
    numberOfBands       = this_raster@file@nbands,
    rows                = this_raster@nrows,
    columns             = this_raster@ncols,
    horizontalAccuracy  = EML::eml$horizontalAccuracy(accuracyReport = "METADATA_NOT_PROVIDED"),
    verticalAccuracy    = EML::eml$verticalAccuracy(accuracyReport = "METADATA_NOT_PROVIDED"),
    cellSizeXDirection  = suppressWarnings(raster::xres(this_raster)),
    cellSizeYDirection  = suppressWarnings(raster::yres(this_raster)),
    rasterOrigin        = "Upper Left",
    verticals           = 1,
    cellGeometry        = "pixel",
    id                  = resource_ident
  )


  # spatial coverage (units must be decimal degrees)

  if (abs(this_raster@extent@xmin) > 180) {

    warning("projection in meters ~ spatial coverage not constructed")

  } else {

    if (!is.null(geographic_description)) {

      this_geographic_description <- geographic_description

    } else {

      this_geographic_description <- configurations[["geographic_description"]]

      if (is.na(this_geographic_description) | is.null(this_geographic_description) | this_geographic_description == "") {

        this_geographic_description <- "METADATA NOT PROVIDED"
        warning("geographic description not provided and missing from config")

      }

    }


    rasterSpatialCoverage <- list(
      geographicDescription = this_geographic_description,
      boundingCoordinates   = list(
        westBoundingCoordinate  = this_raster@extent@xmin,
        eastBoundingCoordinate  = this_raster@extent@xmax,
        northBoundingCoordinate = this_raster@extent@ymax,
        southBoundingCoordinate = this_raster@extent@ymin
      )
    )

    newSR$coverage$geographicCoverage <- rasterSpatialCoverage

  }


  # return -----------------------------------------------------------------------

  message("created spatialRaster: ", resource_ident)

  return(newSR)

}
