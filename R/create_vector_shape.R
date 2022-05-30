#' @title Construct an ESRI shapefile and create metadata of type spatial
#' vector
#'
#' @description create_vector_shape generates a EML entity of type
#' spatialVector. The function allows the user to build a shapefile and
#' accompanying metadata from a spatial object read into or created within the
#' R environment. The resulting shapefile is written to a directory that is
#' then zipped for inclusion in a data package.
#'
#' @details Unlike the package_vector_shape function in the capemlGIS package,
#' create_vector_shape writes a new shapefile to file. This allows the user to
#' produce a shapefile and corresponding metadata by either loading a spatial
#' object into the R environment or creating a spatial object with the R
#' environment. The shapefile that is written to file is packaged within a new
#' directory that is zipped for inclusion in a data package. Because a new
#' spatial object is created, the data can be modified as needed. If present,
#' the function reads attribute and factor metadata from supporting yaml
#' file(s) generated from the capeml::write_attributes() and
#' capeml::write_factors() functions - create_vector_shape will look for files
#' in the working directory with a name of type "entity name"_attrs.yaml (or
#' "entity name"_attrs.csv if an older project) for attribute metadata, and
#' "entity name"_factors.yaml (or "entity name"_factors.csv if an older
#' project) for factor metadata. Note that this functionality is predicated on
#' the existence of a file containing metadata about the attributes, that that
#' file is in the working directory, and that the file matches the name of the
#' spatial data entity precisely.
#'
#' @note If project naming is TRUE then create_vector will look for a package
#' number (packageNum (deprecated) or identifier) in config.yaml; this
#' parameter is not passed to the function and it must exist.
#' @note The shapefile generated from create_vector_shape will have the same
#' coordinate reference system (CRS) as the input spatial object.
#'
#' @param vector_name
#' (character) The unquoted name of the spatial object in the R environment.
#' @param description
#' (character) Description of the vector resource.
#' @param geoDescription
#' (character) A textual description of the geographic coverage of the vector.
#' If not provided, the project-level geographic desciption in the config.yaml
#' will be used.
#' @param coord_sys
#' (character) The quoted EML-compliant name of the coordinate reference
#' system.
#' @param layer_opts
#' (character) Additional options for generating a shapefile that are passed to
#' the sf::st_write function
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
#' @importFrom sf st_write st_transform
#' @importFrom yaml yaml.load_file
#' @importFrom capeml read_attributes read_package_configuration
#' @importFrom tools md5sum
#' @importFrom stringr str_extract
#'
#' @return An object of type EML spatialVector is returned. Additionally, all
#' files that make up a single shapefile are harvested into a new directory
#' that is zipped for inclusion in the data package.
#'
#' @examples
#' \dontrun{
#' 
#' # load spatial vector object; because create_vector_shape will generate a
#' # new shapefile, we have complete flexibility over the shapefile name and
#' # manipulating the data - here we are starting with an existing shapefile
#' # named CORETT but will generate a shapefile with the name
#' # ejido_titles_points_of_decree 
#' 
#' ejido_titles_points_of_decree <- sf::read_sf(
#'   dsn   = "data/Regularizacion/ejidal",
#'   layer = "CORETT"
#'   ) %>%
#' dplyr::select(
#'   -OBJECTID_1,
#'   -FolderNumb,
#'   -Surface
#'   ) %>%
#' dplyr::mutate(
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
#' ejido_titles_points_of_decree_SV <- create_vector_shape(
#'   vector_name = ejido_titles_points_of_decree,
#'   description = ejido_titles_points_of_decree_desc,
#'   coord_sys = "WGS_1984_UTM_Zone_55N",
#'   layer_opts = "SHPT=POLYGON",
#'   overwrite = TRUE,
#'   projectNaming = TRUE,
#'   )
#'
#' # The resulting spatialVector entity can be added to a EML dataset.
#' # Note also in this example that we are passing additional layer options,
#' # which ultimately feed to sf::st_write, necessary here to generate a
#' # multi-polygon shapefile.
#'
#' }
#'
#' @export
#'
create_vector_shape <- function(
  vector_name,
  description,
  geoDescription     = NULL,
  coord_sys,
  layer_opts         = NULL,
  overwrite          = FALSE,
  projectNaming      = TRUE,
  missing_value_code = NULL
  ) {

  # required parameters -------------------------------------------------------

  # do not proceed if a description is not provided

  if (missing("description")) {

    stop("please provide a description for this vector")

  }

  # do not proceed if coord_sys is not present

  if (missing("coord_sys")) {

    stop("please provide an EML-compliant coordinate reference system for this vector")

  }


  # retrieve dataset details from config.yaml

  configurations <- capeml::read_package_configuration()


  # stringify vector name -----------------------------------------------------

  vector_name_string <- deparse(substitute(vector_name))


  # geographic coverage -------------------------------------------------------

  if (missing("geoDescription") | is.null(geoDescription)) {

    geoDescription <- configurations[["geographic_description"]]
    message("project-level geographic description used for spatial entity ", vector_name_string)

  }

  if (is.na(geoDescription) | is.null(geoDescription) | geoDescription == "") {

    geoDescription <- NULL
    stop("entity ", vector_name_string," does not have a geographic description")

  }


  if (sf::st_bbox(vector_name)[["xmin"]] < -180 | sf::st_bbox(vector_name)[["xmin"]] > 180) {

    vector_lat_long <- sf::st_transform(vector_name, crs = 4326)

    spatialCoverage <- EML::set_coverage(
      geographicDescription = geoDescription,
      westBoundingCoordinate =  sf::st_bbox(vector_lat_long)[["xmin"]],
      eastBoundingCoordinate =  sf::st_bbox(vector_lat_long)[["xmax"]],
      northBoundingCoordinate = sf::st_bbox(vector_lat_long)[["ymax"]],
      southBoundingCoordinate = sf::st_bbox(vector_lat_long)[["ymin"]]
    )


  } else {

    spatialCoverage <- EML::set_coverage(
      geographicDescription = geoDescription,
      westBoundingCoordinate =  sf::st_bbox(vector_name)[["xmin"]],
      eastBoundingCoordinate =  sf::st_bbox(vector_name)[["xmax"]],
      northBoundingCoordinate = sf::st_bbox(vector_name)[["ymax"]],
      southBoundingCoordinate = sf::st_bbox(vector_name)[["ymin"]]
    )

  }


  # create a target directory in the working directory to house shapefiles ----

  if (dir.exists(vector_name_string) && overwrite == FALSE) {

    stop("directory to be created (", shQuote(vector_name_string, type = "sh"), ") already exists in working directory (set overwrite to TRUE)")

  }

  if (dir.exists(vector_name_string) && overwrite == TRUE) {

    system(paste0("rm -r ", shQuote(vector_name_string, type = "sh")))

  }

  system(paste0("mkdir ", shQuote(vector_name_string, type = "sh")))


  # write new shape to target directory ---------------------------------------

  shapefile_name <- paste0(vector_name_string, "/", vector_name_string, ".shp")

  sf::st_write(
    obj           = vector_name,
    dsn           = shapefile_name,
    driver        = "ESRI Shapefile",
    append        = !overwrite,
    layer_options = layer_opts
  )


  # zip directory housing shapefiles ------------------------------------------

  if (file.exists(paste0(vector_name_string, ".zip")) && overwrite == FALSE) {

    stop("zip file to be created (", paste0(vector_name_string, ".zip"), ") already exists in working directory (set overwrite to TRUE)")

  }

  if (file.exists(paste0(vector_name_string, ".zip")) && overwrite == TRUE) {

    system(paste0("rm ", shQuote(vector_name_string, type = "sh"), ".zip"))

  }

  system(
    paste0(
      "zip -jXDr ",
      paste0(shQuote(vector_name_string, type = "sh"), ".zip"),
      " ",
      shQuote(vector_name_string, type = "sh")
    )
  )


  # project naming ------------------------------------------------------------

  if (projectNaming == TRUE) {

    zipped_name <- paste0(configurations$identifier, "_", vector_name_string, "_", tools::md5sum(paste0(vector_name_string, ".zip")), ".zip")

    system(
      paste0(
        "mv ",
        paste0(shQuote(vector_name_string, type = "sh"), ".zip"),
        " ",
        shQuote(zipped_name, type = "sh")
      )
    )

  } else {

    zipped_name <- paste0(vector_name_string, ".zip")

  }


  # remove (unzipped) target directory ----------------------------------------

  system(paste0("rm -r ", shQuote(vector_name_string, type = "sh")))


  # attributes ---------------------------------------------------------------

  if (file.exists(paste0(vector_name_string, "_attrs.yaml")) | file.exists(paste0(vector_name_string, "_attrs.csv"))) {

    attributes <- capeml::read_attributes(
      entity_name = vector_name_string,
      missing_value_code = missing_value_code 
    )

  } else {

    warning("missing attributes file: ", paste0(vector_name_string, "_attrs.yaml"), " / ", paste0(layer, "_attrs.yaml"))

  }


  # set physical ------------------------------------------------------------

  # distribution

  fileURL <- configurations$fileURL

  fileDistribution <- EML::eml$distribution(
    EML::eml$online(url = paste0(fileURL, zipped_name))
  )

  # data format

  fileDataFormat <- EML::eml$dataFormat(
    externallyDefinedFormat = EML::eml$externallyDefinedFormat(
      formatName = "Esri Shapefile (zipped)")
  )

  # file size

  fileSize      <- EML::eml$size(unit = "byte")
  fileSize$size <- deparse(file.size(zipped_name))

  # authentication

  fileAuthentication                <- EML::eml$authentication(method = "MD5")
  fileAuthentication$authentication <- md5sum(zipped_name)

  # construct physical

  spatialVectorPhysical <- EML::eml$physical(
    objectName     = zipped_name,
    authentication = fileAuthentication,
    size           = fileSize,
    dataFormat     = fileDataFormat,
    distribution   = fileDistribution
  )

  # create spatialVector ----------------------------------------------------

  newSV <- EML::eml$spatialVector(
    entityName           = zipped_name,
    entityDescription    = description,
    physical             = spatialVectorPhysical,
    coverage             = spatialCoverage,
    attributeList        = attributes,
    geometricObjectCount = nrow(vector_name),
    id                   = zipped_name
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

  spatial_ref <- EML::eml$spatialReference(
    horizCoordSysName = coord_sys
  )

  newSV$spatialReference <- spatial_ref


  # return ------------------------------------------------------------------

  return(newSV)

}
