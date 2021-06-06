#' @title Generate a list of EML-compliant coordinate reference system (CRS)
#'
#' @description list_crs is a helper function that provides convenient access
#' to a list of EML-compliant coordinate references systems (CRSs).
#'
#' @importFrom xml2 read_html xml_contents xml_find_all xml_attr
#'
#' @return A list of EML-compliant coordinate reference systems (CRSs)
#'
#' @export
#'
list_crs <- function() {

  crs_list_raw <- xml2::read_html(
    x = "https://raw.githubusercontent.com/NCEAS/eml/main/xsd/eml-spatialReference.xsd"
  )
  crs_list_contents <- xml2::xml_contents(x = crs_list_raw)

  crs_list <- xml2::xml_find_all(
    x = crs_list_contents,
    xpath = "//element[@name = 'horizCoordSysName']"
    ) |>
    xml2::xml_find_all(xpath = ".//enumeration") |>
    xml2::xml_attr(attr = "value")

  return(crs_list)

}
