## code to prepare `eml_valid_crs` dataset goes here

usethis::use_data(eml_valid_crs, overwrite = TRUE)

eml_spatialReference <- xml2::read_xml(url("https://raw.githubusercontent.com/NCEAS/eml/main/xsd/eml-spatialReference.xsd"))

eml_spatialReference_restriction <- xml2::xml_find_first(
  x     = eml_spatialReference,
  xpath = ".//xs:restriction"
)

eml_spatialReference_enumeration <- xml2::xml_find_all(
  x     = eml_spatialReference_restriction,
  xpath = ".//xs:enumeration"
)

eml_valid_crs <- xml2::xml_attr(
  x    = eml_spatialReference_enumeration,
  attr = "value",
  ns   = character()
) |>
tibble::as_tibble() |>
tibble::add_column(epsg = NA_integer_) |>
dplyr::mutate(
  epsg = dplyr::case_when(
    value == "NAD_1983_UTM_Zone_12N" ~ 26912,
    value == "NAD_1927_UTM_Zone_12N" ~ 26712,
    value == "GCS_WGS_1984" ~ 4326,
    value == "WGS_1984_UTM_Zone_12N" ~ 32612,
    value == "NAD_1983_StatePlane_Arizona_Central_FIPS_0202_Feet" ~ 6404
  ) 
) |>
dplyr::filter(!is.na(epsg))
