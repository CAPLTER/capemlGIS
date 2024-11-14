# need to rebuild test given evolution to create_raster
# testthat::test_that(
#   desc = "create_raster generates attribute details if _factors.yaml is present",
#   code = {

#     groundwater_level <- suppressWarnings(raster::raster("groundwater_level.img"))

#     withr::with_file(
#       file = "groundwater_level_factors.yaml",
#       code = {

#         testthat::expect_no_error(
#           object = capemlGIS::write_raster_factors(
#             raster_entity = groundwater_level,
#             value_name    = "arbitrary_value",
#             overwrite     = TRUE
#           )
#         )

#         groundwater_level_SR <- suppressWarnings(
#           suppressMessages(
#             capemlGIS::create_raster(
#               raster_file              = "groundwater_level.img",
#               description              = "description_from_create",
#               epsg                     = 4326,
#               raster_value_description = "value_desc_from_create",
#               raster_value_units       = "unit_from_create",
#               geographic_description   = NULL,
#               project_naming           = FALSE,
#               file_url                 = NULL
#             )
#           )
#         )

#         testthat::expect_type(
#           object = groundwater_level_SR,
#           type   = "list"
#         )

#         testthat::expect_equal(
#           object   = groundwater_level_SR[["entityDescription"]],
#           expected = "description_from_create"
#         )

#         testthat::expect_equal(
#           object   = groundwater_level_SR[["attributeList"]][["attribute"]][[1]][["attributeName"]],
#           expected = "arbitrary_value"
#         )

#       }
#     )

#   }
# )

# testthat::test_that(
#   desc = "create_spatialRaster uses provided unit if _factors.yaml is not present",
#   code = {

#     groundwater_level_SR <- suppressWarnings(
#       suppressMessages(
#         capemlGIS::create_raster(
#           raster_file              = "groundwater_level.img",
#           description              = "description_from_create",
#           epsg                     = 4326,
#           raster_value_description = "value_desc_from_create",
#           raster_value_units       = "unit_from_create",
#           geographic_description   = NULL,
#           project_naming           = FALSE,
#           file_url                 = NULL
#         )
#       )
#     )

#     testthat::expect_type(
#       object = groundwater_level_SR,
#       type   = "list"
#     )

#     testthat::expect_equal(
#       object   = groundwater_level_SR[["entityDescription"]],
#       expected = "description_from_create"
#     )

#     testthat::expect_equal(
#       object   = groundwater_level_SR[["attributeList"]][["attribute"]][[1]][["attributeName"]],
#       expected = "raster_value"
#     )

#     testthat::expect_equal(
#       object   = groundwater_level_SR[["attributeList"]][["attribute"]][[1]][["attributeDefinition"]],
#       expected = "value_desc_from_create"
#     )

#     testthat::expect_equal(
#       object   = groundwater_level_SR[["attributeList"]][["attribute"]][[1]][["measurementScale"]][["ratio"]][["unit"]][["customUnit"]],
#       expected = "unit_from_create"
#     )

#   }
# )