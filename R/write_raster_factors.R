#' @title create a template as a yaml file for supplying code definition
#' metadata for spatial rasters if raster values are categorical
#'
#' @description \code{write_raster_factors} creates a template as a yaml file
#' for supplying code definition metadata for spatial rasters if raster values
#' are categorical.
#'
#' @details Generate a yaml template file that supports the ability to provide
#' a definition for each level or categorical type. The resulting yaml file is
#' written with the name of the raster file + "_factors". The
#' \code{create_spatialRaster} function will search for this file when creating
#' a EML spatialRaster entity.
#'
#' @note \code{write_raster_factors} will take as input either an object of
#' class raster that is already in the environment (\code{raster_entity}) or
#' will read a raster file (\code{raster_entity}) given a path and filename.
#'
#' @note Constructing the template can be extremely computationally and time
#' intensive for large rasters.
#'
#' @param raster_file
#' (character) Quoted full path to raster file.
#' @param raster_entity (character)
#' Unquoted name of the raster object in the R environment.
#' @param value_name
#' (character) Quoted name describing the raster value category (category,
#' code, level would be common examples)
#' @param overwrite
#' (logical) Overwrite an existing template if one exists.
#'
#' @importFrom raster ratify
#' @importFrom tools file_path_sans_ext
#' @importFrom purrr map
#' @importFrom yaml as.yaml write_yaml
#'
#' @return A template for providing code definition metadata as a yaml file
#' with the file name of the raster + "_factors.yaml" (created in the working
#' directory).
#'
#' @examples
#'
#' \dontrun{
#'
#' capemlGIS::write_raster_factors(
#'  raster_entity = denp3p1,
#'  value_name    = "lulc_codes"
#' )
#'
#' capemlGIS::write_raster_factors(
#'  raster_file = "file-path-to/denp3p1.img",
#'  value_name  = "weight_class_bins"
#' )
#'
#' }
#'
#' @export
#'
write_raster_factors <- function(
  raster_file,
  raster_entity,
  value_name,
  overwrite = FALSE
  )  {

  # set options ----------------------------------------------------------------

  options(scipen = 999)


  # helper functions -----------------------------------------------------------

  factors_to_yaml <- function(raster_values_categories, raster_value_name) {

    factors_yaml <- list(
      attribute       = list(
        attributeName = raster_value_name,
        levels        = purrr::map(
          .x = raster_values_categories,
          .f = map_factor_levels
        )
      )
    )

    return(factors_yaml)

  }


  map_factor_levels <- function(level) {

    var_levs <- list(
      code       = level,
      definition = "metadata_not_provided"
    )

    return(var_levs)

  }


  # process raster and build yaml ---------------------------------------------

  if (!missing(raster_entity)) {

    this_raster <- raster_entity
    entity_name <- deparse(substitute(raster_entity))

  } else {

    this_raster <- suppressWarnings(raster::raster(raster_file))
    entity_name <- basename(tools::file_path_sans_ext(raster_file))

  }

  raster_ratify <- suppressWarnings(raster::ratify(this_raster))
  these_levels  <- raster_ratify@data@attributes[[1]][["ID"]]

  factors_as_yaml <- factors_to_yaml(
    raster_values_categories = these_levels,
    raster_value_name        = value_name
    )

  factors_as_yaml <- list(factors_as_yaml) # hack to match capeml format
  factors_as_yaml <- yaml::as.yaml(factors_as_yaml)


  # write to file -------------------------------------------------------------

  file_name <- paste0(entity_name, "_factors.yaml")


  if (file.exists(file_name) && overwrite == FALSE) {

    stop(file_name, " already exists, use 'overwrite = TRUE' to overwrite")

  }


  yaml::write_yaml(
    x    = factors_as_yaml,
    file = file_name
  )

  message("constructed raster yaml: ", file_name)

}
