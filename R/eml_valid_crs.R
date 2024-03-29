#' Tibble of valid EML CRS descriptions and EPSG codes
#'
#' The data here include EML-compliant descriptions of hoorizontal coordinate
#' systems and corresponding EPSG codes for selected coordinate systems.
#' Additional relationships (i.e., making connections between EPSG code and
#' corresponding description needed for EML) can be made by making these
#' connections in the data-raw/eml_valid_crs.R of this pacakge.
#'
#' @format A data frame with 4 rows and 2 variables:
#' \describe{
#'   \item{value}{EML-compliant textual description of projection}
#'   \item{epsg}{four- or five-digit EPSG code corresponding to textual description of the coordinate system}
#' }
#' @source \url{https://raw.githubusercontent.com/NCEAS/eml/main/xsd/eml-spatialReference.xsd}
"eml_valid_crs"
