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

    ###
    if (!is.null(find_element(attr_list, "unit"))) {

      write_raster_units(
        entity_name = entity_name,
        entity_id   = entity_id
      )

    }
    ###

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


#' @description \code{find_element} is a helper function to
#' \code{create_dataTable} that checks for the presence of a particular element
#' in a list. In this case, we use \code{find_element} to determine if any of
#' the attributes of a datatable have units.

find_element <- function(attributes, element) {

  if (utils::hasName(attributes, element)) {

    attributes[[element]]

  } else if (is.list(attributes)) {

    for (obj in attributes) {

      ret <- Recall(obj, element)
      if (!is.null(ret)) return(ret)

    }

  } else {

    NULL

  }

}

#' @export

read_raster_attributes <- function(
  entity_name,
  missing_value_code = NULL,
  entity_id          = "data_entity"
  ) {

  # entity_name <- basename(tools::file_path_sans_ext(entity_name))

  # establish references to the data entity and entity name

  if (rlang::is_expression(entity_name)) {

    string_pointer <- rlang::get_expr(entity_name)
    # object_pointer <- get(x = entity_name, envir = globalenv())

  } else {

    string_pointer <- deparse(substitute(entity_name))
    # object_pointer <- entity_name

  }


  # attributes ----------------------------------------------------------------

  # load attributes from yaml or csv (default to yaml)
  if (file.exists(paste0(string_pointer, "_attrs.yaml"))) {

    attrs <- yaml::yaml.load_file(paste0(string_pointer, "_attrs.yaml"))
    attrs <- yaml::yaml.load(attrs)
    attrs <- tibble::enframe(attrs) |>
      tidyr::unnest_wider(value) |>
      dplyr::select(-one_of("name"))

  } else if (!file.exists(paste0(string_pointer, "_attrs.yaml")) && file.exists(paste0(string_pointer, "_attrs.csv"))) {

    attrs <- utils::read.csv(paste0(string_pointer, "_attrs.csv"))

  } else {

    stop(paste0("attributes file: ", string_pointer, "_attrs.yaml ", "not found in ", getwd()))

  }

  # column classes to vector (req'd by set_attributes)
  classes <- attrs |>
    dplyr::pull(columnClasses)

  # copy attributeDefinition to defintion as appropriate;
  # remove col classes from attrs (req'd by set_attributes);
  # remove empty columns (targets here are max and min values, which can throw
  # an error for data without any numeric columns)
  # empty strings to NA

  attrs[attrs == ""] <- NA

  # helper function to remove missing columns
  not_all_na <- function(x) {
    !all(is.na(x))
  }

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


  # return --------------------------------------------------------------------

  attr_list <- EML::set_attributes(
    attributes    = attrs,
    col_classes   = classes
  )

  attrs["columnClasses"] <- classes

  return(
    list(
      eml   = attr_list,
      table = attrs
    )
  )

}

write_raster_units <- function(
  entity_name,
  entity_id
) {

  qudt   <- FALSE
  custom <- FALSE

  attributes_table <- read_raster_attributes(
    entity_name = entity_name,
    entity_id   = entity_id
  )[["table"]]

  attributes_units_unique <- unique(attributes_table[!is.na(attributes_table$unit), ][["unit"]])
  attributes_units_types  <- purrr::map(.x = attributes_units_unique, ~ capeml::get_unit_type(this_unit = .x))

  if (!all(sapply(attributes_units_types, is.null))) {

    qudt_and_custom <- attributes_units_types |> 
      purrr::list_rbind() |> 
      dplyr::inner_join(
        y  = attributes_table[, c("attributeName", "id", "unit")],
        by = c("name" = "unit")
      )

    # type: QUDT

    if (nrow(qudt_and_custom[grepl("qudt", qudt_and_custom$type, ignore.case = TRUE), ]) > 0) {

      qudt <- TRUE

      qudt_units <- qudt_and_custom[grepl("qudt", qudt_and_custom$type, ignore.case = TRUE), ]
      qudt_units$id_name <- paste0(qudt_units$id, "_", qudt_units$name)

      qudt_annotations <- split(
        x = qudt_units,
        f = qudt_units$id_name
      ) |>
        {\(row) purrr::map(.x = row, ~ 
          list(
            name          = .x$name,
            valueLabel    = .x$label,
            valueURI      = .x$unit,
            references    = .x$id,
            propertyLabel = "has unit",
            propertyURI   = "http://qudt.org/schema/qudt/hasUnit"
          )
        )}() |> 
        unique()


      if (file.exists("annotations.yaml")) {

        existing_annotations <- yaml::yaml.load_file("annotations.yaml") |> 
          unique()

        c(existing_annotations, qudt_annotations) |> 
          unique() |> 
          yaml::write_yaml(
            file         = "annotations.yaml",
            column.major = FALSE
          )

      } else {

        yaml::write_yaml(
          x            = qudt_annotations,
          file         = "annotations.yaml",
          column.major = FALSE
        )

      }

    }


    # type: custom

    if (nrow(qudt_and_custom[qudt_and_custom$type == "custom", ]) > 0) {

      custom <- TRUE

      new_custom_units <- qudt_and_custom[qudt_and_custom$type == "custom", ]["name"] |> 
        dplyr::mutate(description = "") |>
        as.list() |> 
        purrr::list_transpose(simplify = FALSE)

      # message("new_custom_units: ", qudt_and_custom[qudt_and_custom$type == "custom", ]["name"])

    }


    # construct <additionalMetadata><metadata><unitList><unit>

    if (qudt == TRUE && custom == TRUE) {

      qudt_for_unitlist <- unique(purrr::map(.x = qudt_annotations, ~ list(name = .x$"name")))
      new_custom_units  <- c(new_custom_units, qudt_for_unitlist)

    } else if (qudt == TRUE && custom == FALSE) {

      new_custom_units <- unique(purrr::map(.x = qudt_annotations, ~ list(name = .x$"name")))

    }


    # write to file

    if (qudt == TRUE || custom == TRUE) {

      if (file.exists("custom_units.yaml")) {

        existing_custom_units <- yaml::yaml.load_file("custom_units.yaml")

        # do not add new CUs if they already exist in cu.yaml
        existing_custom_units_names <- existing_custom_units |>
        purrr::map("name") |>
        unique()

        new_custom_units <- purrr::discard(
          .x = new_custom_units,
          .p = \(x) x[["name"]] %in% existing_custom_units_names
        )

        c(existing_custom_units, new_custom_units) |> 
          unique() |> 
          yaml::write_yaml(
            file         = "custom_units.yaml",
            column.major = FALSE
          )

      } else {

        yaml::write_yaml(
          x            = new_custom_units,
          file         = "custom_units.yaml",
          column.major = FALSE
        )

      }

    }

  } else {

    # message(entity_name, ": neither QUDT or custom units were detected")
    return(NULL)

  }

}
