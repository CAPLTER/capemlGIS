testthat::test_that(
  desc = "write_raster_factors executes whether a file or entity is provided",
  code = {

    groundwater_level <- suppressWarnings(raster::raster("groundwater_level.img"))

    withr::with_file(
      file = "groundwater_level_factors.yaml",
      code = {


        testthat::expect_no_error(
          object = capemlGIS::write_raster_factors(
            raster_entity = groundwater_level,
            value_name    = "arbitrary_value",
            overwrite     = TRUE
          )
        )

        testthat::expect_no_error(
          object = capemlGIS::write_raster_factors(
            raster_file = "groundwater_level.img",
            value_name  = "arbitrary_value",
            overwrite   = TRUE
          )
        )

      }
    )

  }
)


testthat::test_that(
  desc = "write_raster_factors generates a yaml template of expected structure",
  code = {

    groundwater_level <- suppressWarnings(raster::raster("groundwater_level.img"))

    withr::with_file(
      file = "groundwater_level_factors.yaml",
      code = {

        # need to build the yaml
        testthat::expect_no_error(
          object = capemlGIS::write_raster_factors(
            raster_entity = groundwater_level,
            value_name    = "arbitrary_value",
            overwrite     = TRUE
          )
        )

        raster_factors <- yaml.load_file("groundwater_level_factors.yaml") |>
          yaml::yaml.load() |>
          tibble::enframe() |>
          tidyr::unnest_wider(value) |>
          tidyr::unnest_wider(attribute) |>
          tidyr::unnest_longer(levels) |>
          tidyr::unnest_wider(levels) |>
          dplyr::select(-one_of("name"))

          testthat::expect_equal(
            object   = nrow(raster_factors),
            expected = 10L
          )

          testthat::expect_equal(
            object   = ncol(raster_factors),
            expected = 3L
          )

          testthat::expect_named(
            object   = raster_factors,
            expected = c("attributeName", "code", "definition")
          )

      }
    )

  }
)
