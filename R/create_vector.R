#' @title Construct a spatial file of type KML or GeoJSON and create metadata
#' of type spatial vector
#'
#' @description create_vector writes a spatial data file of type KML and
#' generates corresponding metadata of EML entity of type spatialVector
#'
#' @details create_vector creates a EML spatialVector object from a spatial
#' data object that is read into or created within the R environment. If
#' present, the function reads attribute and factor metadata from supporting
#' yaml file(s) generated from the capeml::write_attributes() and
#' capeml::write_factors() functions - create_vector will look for files in the
#' working directory with a name of type "entity name"_attrs.yaml (or "entity
#' name"_attrs.csv if an older project) for attribute metadata, and "entity
#' name"_factors.yaml (or "entity name"_factors.csv if an older project) for
#' factor metadata. Note that this functionality is predicated on the existence
#' of a file containing metadata about the attributes, that that file is in the
#' working directory, and that the file matches the name of the spatial data
#' entity precisely.
#'
#' @note If project naming is TRUE then create_vector will look for a package
#' number (packageNum (deprecated) or identifier) in config.yaml; this
#' parameter is not passed to the function and it must exist.
#' @note All vector objects are transformed to epsg 4326 (WGS 1984)
#'
#' @param vector_name
#'  (character) The unquoted name of the spatial data object in the R
#'  environment.
#' @param description
#'  (character) Quoated description of the vector resource.
#' @param driver
#'  (character) Quoted format of output file: KML or GeoJSON (default).
#' @param geoDescription
#'  (character) A textual description of the geographic study area of the
#'  vector. This parameter allows the user to overwrite the
#'  geographicDesciption value provided in the project config.yaml.
#' @param overwrite
#' (logical) A logical indicating whether to overwrite an existing file bearing
#' the same name as kml file as it is imported.
#' @param projectNaming
#'  (logical) Logical indicating if the resulting file should be renamed per
#'  the style used in the capeml ecosystem with the project id + base file name
#'  + file extension. If set to false, the resulting file will bear the name
#'  the object is assigned in the R environment with the appropriate file
#'  extension.
#' @param missing_value_code
#' (character) Quoted character(s) of a flag, in addition to NA or NaN, used to
#' indicate missing values within the data.
#'
#' @import EML
#' @importFrom sf st_transform st_write
#' @importFrom yaml yaml.load_file
#' @importFrom tools md5sum
#' @importFrom capeml read_attributes
#' @importFrom stringr str_extract
#'
#' @return EML spatialVector object is returned. Additionally, the spatial data
#' entity is written to file as type kml or GeoJSON.
#'
#' @examples
#' \dontrun{
#' 
#' # load spatial vector object; because create_vector will generate a new kml or GeoJSON file, we
#' # have complete flexibility over the resulting file name and manipulating
#' # the data - here we are starting with an existing shapefile named CORETT but
#' # will generate a kml with the name ejido_titles_points_of_decree 
#' 
#' ejido_titles_points_of_decree <- sf::read_sf(
#'   dsn = "data/Regularizacion/ejidal",
#'   layer = "CORETT"
#'   ) %>%
#' select(
#'   -OBJECTID_1,
#'   -FolderNumb,
#'   -Surface
#'   ) %>%
#' mutate(
#'   Id = as.character(Id),
#'   across(where(is.character), ~ gsub(pattern = "\\r\\n", replacement = "", x = .)),
#'   across(where(is.character), ~ gsub(pattern = "--", replacement = NA_character_, x = .)),
#'   Year = as.character(Year)
#' )
#' 
#' # write attributes (and factors if relevant)
#' 
#' try(write_attributes(ejido_titles_points_of_decree, overwrite = FALSE))
#' 
#' # generate a description for the data entity
#' 
#' ejido_titles_points_of_decree_desc <- "polygons of land regularized by the
#' National Agency, CORETT; polygons were georeferenced from 281 paper maps,
#' consolidated into 87 unique regularization degrees of ejidos that became
#' privitzed from 1987-2007; includes the area of each polygon, the date of
#' regularization, the name of the ejido, the delegation, and the 'plane
#' number' that could be used to find the original map file in the CORETT
#' office; it only includes expropriation for the delegations Xochimilco,
#' Magdalena Contreras, Iztapalapa, Tlahuac, Gustavo Madero, Cuajimalpa, Alvaro
#' Obregon, Tlalpan, Coyoacan, and Milpa Alta"
#' 
#' ejido_titles_points_of_decree_SV <- create_vector(
#'   vector_name = ejido_titles_points_of_decree,
#'   description = ejido_titles_points_of_decree_desc,
#'   driver = "kml",
#'   overwrite = TRUE,
#'   projectNaming = TRUE
#'   )
#'
#' # The resulting spatialVector entity can be added to a EML dataset
#'
#' }
#'
#' @export
#'
create_vector <- function(
  vector_name,
  description,
  driver             = "GeoJSON",
  geoDescription     = NULL,
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

    stop("could not locate config.yaml in ", getwd())

  }


  # driver-specific formatting ------------------------------------------------

  if (grepl("geojson", driver, ignore.case = TRUE)) {

    file_extension  <- "geojson"
    data_one_format <- "GeoJSON"

  } else {

    file_extension  <- "kml"
    data_one_format <- "Google Earth Keyhole Markup Language (KML)"

  }


  # stringify vector name -----------------------------------------------------

  vector_name_string <- deparse(substitute(vector_name))


  # ensure epsg4326 -----------------------------------------------------------

  vector_name <- sf::st_transform(
    x   = vector_name,
    crs = 4326
  )


  # retrieve dataset details from config.yaml

  configurations <- yaml::yaml.load_file("config.yaml")


  # geographic coverage -------------------------------------------------------

  if (missing("geoDescription") | is.null(geoDescription)) {

    geoDescription <- yaml::yaml.load_file("config.yaml")$geographicCoverage$geographicDescription
    message("project-level geographic description used for spatial entity ", vector_name_string)

  }

  if (is.na(geoDescription) | is.null(geoDescription) | geoDescription == "") {

    geoDescription <- NULL
    stop("entity ", vector_name_string," does not have a geographic description")

  }

  spatialCoverage <- EML::set_coverage(
    geographicDescription   = geoDescription,
    westBoundingCoordinate  = sf::st_bbox(vector_name)[["xmin"]],
    eastBoundingCoordinate  = sf::st_bbox(vector_name)[["xmax"]],
    northBoundingCoordinate = sf::st_bbox(vector_name)[["ymax"]],
    southBoundingCoordinate = sf::st_bbox(vector_name)[["ymin"]]
  )


  # write to file ----------------------------------------------------------------

  if (file.exists(paste0(vector_name_string, ".", file_extension)) && overwrite == FALSE) {

    stop("file to be created (", paste0(vector_name_string, ".", file_extension), ") already exists in working directory (set overwrite to TRUE)")

  }

  sf::st_write(
    obj          = vector_name,
    dsn          = paste0(vector_name_string, ".", file_extension),
    driver       = file_extension,
    delete_layer = TRUE,
    delete_dsn   = TRUE
  )


  # attributes ---------------------------------------------------------------

  if (file.exists(paste0(vector_name_string, "_attrs.yaml")) | file.exists(paste0(vector_name_string, "_attrs.csv"))) {

    attributes <- capeml::read_attributes(
      entity_name        = vector_name_string,
      missing_value_code = missing_value_code
    )

  } else {

    warning("missing attributes file: ", paste0(vector_name_string, "_attrs.yaml"), " / ", paste0(vector_name_string, "_attrs.csv"))

  }


  # project naming ------------------------------------------------------------

  if (projectNaming == TRUE) {

    if (exists("packageNum", configurations)) {

      this_identifier <- stringr::str_extract(
        string  = configurations[["packageNum"]],
        pattern = "[0-9]+"
      )

    } else if (exists("identifier", configurations)) {

      this_identifier <- configurations[["identifier"]]

    } else {

      stop("could not resolve package identifier (number)")

    }

    this_identifier <- as.integer(this_identifier)

    project_name <- paste0(this_identifier, "_", vector_name_string, ".", file_extension)

    system(
      paste0(
        "mv ",
        paste0(shQuote(vector_name_string, type = "sh"), ".", file_extension),
        " ",
        shQuote(project_name, type = "sh")
      )
    )

  } else {

    project_name <- paste0(vector_name_string, ".", file_extension)

  }


  # set physical ----------------------------------------------------------------

  # distribution

  fileURL <- configurations[["baseURL"]]

  fileDistribution <- EML::eml$distribution(
    EML::eml$online(url = paste0(fileURL, project_name))
  )

  # data format

  fileDataFormat <- EML::eml$dataFormat(
    externallyDefinedFormat = EML::eml$externallyDefinedFormat(
      formatName            = data_one_format
    )
  )

  # file size

  fileSize      <- EML::eml$size(unit = "byte")
  fileSize$size <- deparse(file.size(project_name))

  # authentication

  fileAuthentication                <- EML::eml$authentication(method = "MD5")
  fileAuthentication$authentication <- md5sum(project_name)

  # construct physical

  spatialVectorPhysical <- EML::eml$physical(
    objectName     = project_name,
    authentication = fileAuthentication,
    size           = fileSize,
    dataFormat     = fileDataFormat,
    distribution   = fileDistribution
  )


  # create spatialVector entity ---------------------------------------------

  newSV <- EML::eml$spatialVector(
    entityName           = project_name,
    entityDescription    = description,
    physical             = spatialVectorPhysical,
    coverage             = spatialCoverage,
    attributeList        = attributes,
    geometricObjectCount = nrow(vector_name),
    id                   = project_name
  )


  # add geometry type -------------------------------------------------------

  sfGeometry <- attr(vector_name$geometry, "class")[[1]]

  if (grepl("polygon", sfGeometry, ignore.case = TRUE)) {

    objectGeometry <- "Polygon"

  } else if (grepl("point", sfGeometry, ignore.case = TRUE)) {

    objectGeometry <- "Point"

  } else if (grepl("linestring", sfGeometry, ignore.case = TRUE)) {

    objectGeometry <- "LineString"

  } else {

    stop(paste0("undetermined geometry: ", attr(vector_name$geometry, "class")[[1]]))

  }

  newSV$geometry <- objectGeometry


  # add spatial reference  --------------------------------------------------

  epsg4326 <- EML::eml$spatialReference(
    horizCoordSysName = "GCS_WGS_1984"
  )

  newSV$spatialReference <- epsg4326


  # closing message ---------------------------------------------------------

  message("spatialVector ", project_name,  " created")


  # return ------------------------------------------------------------------

  return(newSV)

} # close create_vector
