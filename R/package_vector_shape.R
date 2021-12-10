#' @title Create metadata of type spatial vector and package shapefile into a
#' zipped directory
#'
#' @description package_vector_shape generates a EML entity of type
#' spatialVector, and harvests all relevant files that constitute a single
#' shapefile into a directory that is then zipped.
#'
#' @details Unlike some other capemlGIS functions for managing vector data,
#' package_vector_shape does not create a new spatial entity but rather
#' collects all of the files that consititue a single shapefile, aggregates
#' those files into a single directory, then zips that directory for inclusion
#' in the data package. Metadata are generated on both spatial aspects of the
#' data and physical aspects of the zipped directory. Because a new spatial
#' object is not created, metadata must be constructed on the data as they
#' exist. That is, some data operations can be performed, such as classifying a
#' numerical ID field as character so that it does not require a unit, but the
#' data from which the metadata are constructed should not be edited. If
#' present, the function reads attribute and factor metadata from supporting
#' yaml file(s) generated from the capeml::write_attributes() and
#' capeml::write_factors() functions - package_vector_shape will look for files
#' in the working directory with a name of type "entity name"_attrs.yaml (or
#' "entity name"_attrs.csv if an older project) for attribute metadata, and
#' "entity name"_factors.yaml (or "entity name"_factors.csv if an older
#' project) for factor metadata. Note that this functionality is predicated on
#' the existence of a file containing metadata about the attributes, that that
#' file is in the working directory, and that the file matches the name of the
#' spatial data entity precisely.
#'
#' @note package_vector_shape will look for a package number (packageNum) in
#' config.yaml; this parameter is not passed to the function and it must exist.
#'
#' @param dsn
#' (character) The quoted name of the directory where the shapefile is
#' located. The trailing slash of a directory should not be included.
#' @param layer
#' (character) The quoted name of the shapefile (without a file extension).
#' @param description
#' (character) Description of the vector resource.
#' @param geoDescription
#' (character) A textual description of the geographic coverage of the vector.
#' If not provided, the project-level geographic desciption in the config.yaml
#' will be used.
#' @param coord_sys
#' (character) The quoted EML-compliant name of the coordinate reference
#' system.
#' @param overwrite
#' (logical) A logical indicating whether to overwrite an existing directory
#' bearing the same name as the shapefile.
#' @param projectNaming
#' (logical) Logical indicating if the zipped directory should be renamed per
#' the style used within the capeml ecosystem (default) with the project id +
#' base file name + md5sum + file extension (zip in this case). The name of the
#' shapefile + file extension (zip in this case) will be used if this is set to
#' FALSE. Note that this applies only to the zipped directory, the files that
#' constitute the shapefile are not renamed.
#' @param missing_value_code
#' (character) Quoted character(s) of a flag, in addition to NA or NaN, used to
#' indicate missing values within the data.
#'
#' @import EML
#' @importFrom sf st_read
#' @importFrom yaml yaml.load_file
#' @importFrom purrr map
#' @importFrom capeml read_attributes
#' @importFrom tools md5sum
#'
#' @return An object of type EML spatialVector is returned. Additionally, all
#' files that make up a single shapefile are harvested into a new directory
#' that is zipped for inclusion in the data package.
#'
#' @examples
#' \dontrun{
#' 
#' # read spatial data into R and manipulate as necessary (but minimally as a
#' # new data object is not created so the metadata should reflect the unaltered
#' source data)
#' CORETT <- sf::read_sf(
#'   dsn = "data/",
#'   layer = "CORETT"
#'   ) %>%
#' mutate(
#'   Id = as.character(Id),
#'   Year = as.character(Year)
#' )
#' 
#' # write an attributes metadata template (and factors if relevant (not shown))
#' try(capeml::write_attributes(CORETT, overwrite = TRUE))
#' 
#' # generate a description for the data entity
#' corett_desc <- "polygons of land regularized by the National Agency, CORETT;
#' polygons were georeferenced from 281 paper maps, consolidated into 87 unique
#' regularization degrees of ejidos that became privitzed from 1987-2007;
#' includes the area of each polygon, the date of regularization, the name of
#' the ejido, the delegation, and the 'plane number' that could be used to find
#' the original map file in the CORETT office; it only includes expropriation
#' for the delegations Xochimilco, Magdalena Contreras, Iztapalapa, Tlahuac,
#' Gustavo Madero, Cuajimalpa, Alvaro Obregon, Tlalpan, Coyoacan, and Milpa
#' Alta"
#' 
#' # generate the spatial vector object, which can now be incorporated into our
#' # EML dataset metadata; here we are relying on the geographic description in
#' # the project config.yaml but we could overwrite that by passing an argument
#' # to the geoDescription parameter if appropriate.
#' 
#' corett_SV <- package_vector_shape(
#'   dsn           = "~/Desktop/shapedir/data",
#'   layer         = "CORETT",
#'   description   = corett_desc,
#'   geoDescription,
#'   coord_sys     = "WGS_1984_UTM_Zone_55N",
#'   overwrite     = TRUE,
#'   projectNaming = TRUE
#'   )
#'
#' # The resulting spatialVector entity can be added to a EML dataset
#'
#' }
#'
#' @export
#'
package_vector_shape <- function(
  dsn,
  layer,
  description,
  geoDescription     = NULL,
  coord_sys,
  overwrite          = FALSE,
  projectNaming      = TRUE,
  missing_value_code = NULL
  ) {


  # required parameters -------------------------------------------------------

  # do not proceed if a description is not provided

  if (missing("description")) {

    stop("please provide a description for this vector")

  }

  # do not proceed if config.yaml is not present

  if (!file.exists("config.yaml")) {

    stop("could not locate config.yaml")

  }

  # do not proceed if coord_sys is not present

  if (missing("coord_sys")) {

    stop("please provide an EML-compliant coordinate reference system for this vector")

  }

  # file management

  # create a target directory in the working directory to house shapefiles ----

  if (dir.exists(layer) && overwrite == FALSE) {

    stop("directory to be created (", shQuote(layer, type = "sh"), ") already exists in working directory (set overwrite to TRUE)")

  }

  if (dir.exists(layer) && overwrite == TRUE) {

    system(paste0("rm -r ", shQuote(layer, type = "sh")))

  }

  system(paste0("mkdir ", shQuote(layer, type = "sh")))

  # identify shapefiles and copy to target directory --------------------------

  shape_files <- list.files(
    path = dsn,
    pattern = layer,
    full.names = TRUE
  )

  shape_extensions <- c("cpg","dbf","prj","sbn","sbx","shp","xml","shx")

  excluded_files <- shape_files[!tools::file_ext(shape_files) %in% shape_extensions]
  shape_files <- shape_files[tools::file_ext(shape_files) %in% shape_extensions]
  purrr::map(
    .x = shape_files,
    ~ system(paste0("cp ", shQuote(.x, type = "sh"), " ", shQuote(layer, type = "sh")))
  )

  message("files added to ", shQuote(layer, type = "sh"), " directory")
  purrr::map(
    .x = shape_files,
    ~ message(basename(.x))
  )

  if (length(excluded_files) > 0) {

    message("")
    message("files NOT added to ", shQuote(layer, type = "sh"), " directory")
    purrr::map(
      .x = excluded_files,
      ~ message(basename(.x))
    )
    message("")

  }

  # zip directory housing shapefiles ------------------------------------------

  if (file.exists(paste0(layer, ".zip")) && overwrite == FALSE) {

    stop("zip file to be created (", paste0(layer, ".zip"), ") already exists in working directory (set overwrite to TRUE)")

  }

  if (file.exists(paste0(layer, ".zip")) && overwrite == TRUE) {

    system(paste0("rm ", shQuote(layer, type = "sh"), ".zip"))

  }

  system(
    paste0(
      "zip -jXDr ",
      paste0(shQuote(layer, type = "sh"), ".zip"),
      " ",
      shQuote(layer, type = "sh")
    )
  )

  # project naming ------------------------------------------------------------

  if (projectNaming == TRUE) {

    packageNum <- yaml::yaml.load_file("config.yaml")$packageNum

    zipped_name <- paste0(packageNum, "_", layer, "_", tools::md5sum(paste0(layer, ".zip")), ".zip")

    system(
      paste0(
        "mv ",
        paste0(shQuote(layer, type = "sh"), ".zip"),
        " ",
        shQuote(zipped_name, type = "sh")
      )
    )

  } else {

    zipped_name <- paste0(layer, ".zip")

  }

  # remove (unzipped) target directory ----------------------------------------

  system(paste0("rm -r ", shQuote(layer, type = "sh")))

  #}}}

  # read data -----------------------------------------------------------------

  this_vector <- sf::st_read(
    dsn = dsn,
    layer = layer
  )

  #{{{ construct EML

  # geographic coverage -----------------------------------------------------

  if (missing("geoDescription") | is.null(geoDescription)) {

    geoDescription <- yaml::yaml.load_file("config.yaml")$geographicCoverage$geographicDescription
    message("project-level geographic description used for spatial entity ", layer)

  }

  if (is.na(geoDescription) | is.null(geoDescription) | geoDescription == "") {

    geoDescription <- NULL
    warning("entity ", layer," does not have a geographic description")

  }

  spatialCoverage <- EML::set_coverage(
    geographicDescription = geoDescription,
    westBoundingCoordinate =  sf::st_bbox(this_vector)[["xmin"]],
    eastBoundingCoordinate =  sf::st_bbox(this_vector)[["xmax"]],
    northBoundingCoordinate = sf::st_bbox(this_vector)[["ymax"]],
    southBoundingCoordinate = sf::st_bbox(this_vector)[["ymin"]]
  )


  # attributes ---------------------------------------------------------------

  if (file.exists(paste0(layer, "_attrs.yaml")) | file.exists(paste0(layer, "_attrs.csv"))) {

    attributes <- capeml::read_attributes(
      entity_name = layer,
      missing_value_code = missing_value_code 
    )

  } else {

    warning("missing attributes file: ", paste0(layer, "_attrs.yaml"), " / ", paste0(layer, "_attrs.yaml"))

  }


  # set physical ------------------------------------------------------------

  # distribution

  fileURL <- yaml::yaml.load_file("config.yaml")$baseURL

  fileDistribution <- EML::eml$distribution(
    EML::eml$online(url = paste0(fileURL, zipped_name))
  )

  # data format

  fileDataFormat <- EML::eml$dataFormat(
    externallyDefinedFormat = EML::eml$externallyDefinedFormat(
      formatName = "Esri Shapefile (zipped)")
  )

  # file size

  fileSize <- EML::eml$size(unit = "byte")
  fileSize$size <- deparse(file.size(zipped_name))

  # authentication

  fileAuthentication <- EML::eml$authentication(method = "MD5")
  fileAuthentication$authentication <- md5sum(zipped_name)

  # construct physical

  spatialVectorPhysical <- EML::eml$physical(
    objectName = zipped_name,
    authentication = fileAuthentication,
    size = fileSize,
    dataFormat = fileDataFormat,
    distribution = fileDistribution
  )

  # create spatialVector ----------------------------------------------------

  newSV <- EML::eml$spatialVector(
    entityName = zipped_name,
    entityDescription = description,
    physical = spatialVectorPhysical,
    coverage = spatialCoverage,
    attributeList = attributes,
    geometricObjectCount = nrow(this_vector),
    id = zipped_name
  )

  # add geometry type -------------------------------------------------------

  sfGeometry <- attr(this_vector$geometry, "class")[[1]]

  if (grepl("polygon", sfGeometry, ignore.case = TRUE)) {

    objectGeometry <- "Polygon"

  } else if (grepl("point", sfGeometry, ignore.case = TRUE)) {

    objectGeometry <- "Point"

  } else if (grepl("linestring", sfGeometry, ignore.case = TRUE)) {

    objectGeometry <- "LineString"

  } else {

    stop(paste0("undetermined geometry: ", attr(this_vector$geometry, "class")[[1]]))

  }

  newSV$geometry <- objectGeometry


  # add spatial reference  --------------------------------------------------

  spatial_ref <- EML::eml$spatialReference(
    horizCoordSysName = coord_sys
  )

  newSV$spatialReference <- spatial_ref


  # return ------------------------------------------------------------------

  return(newSV)

}
