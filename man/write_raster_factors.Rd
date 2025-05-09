% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_raster_factors.R
\name{write_raster_factors}
\alias{write_raster_factors}
\title{create a template as a yaml file for supplying code definition
metadata for spatial rasters if raster values are categorical}
\usage{
write_raster_factors(raster_file, raster_entity, value_name, overwrite = FALSE)
}
\arguments{
\item{raster_file}{(character) Quoted full path to raster file.}

\item{raster_entity}{(character)
Unquoted name of the raster object in the R environment.}

\item{value_name}{(character) Quoted name describing the raster value category (category,
code, level would be common examples)}

\item{overwrite}{(logical) Overwrite an existing template if one exists.}
}
\value{
A template for providing code definition metadata as a yaml file
with the file name of the raster + "_factors.yaml" (created in the working
directory).
}
\description{
\code{write_raster_factors} creates a template as a yaml file
for supplying code definition metadata for spatial rasters if raster values
are categorical.
}
\details{
Generate a yaml template file that supports the ability to provide
a definition for each level or categorical type. The resulting yaml file is
written with the name of the raster file + "_factors". The
\code{create_spatialRaster} function will search for this file when creating
a EML spatialRaster entity.
}
\note{
\code{write_raster_factors} will take as input either an object of
class raster that is already in the environment (\code{raster_entity}) or
will read a raster file (\code{raster_entity}) given a path and filename.

Constructing the template can be extremely computationally and time
intensive for large rasters.
}
\examples{

\dontrun{

capemlGIS::write_raster_factors(
 raster_entity = denp3p1,
 value_name    = "lulc_codes"
)

capemlGIS::write_raster_factors(
 raster_file = "file-path-to/denp3p1.img",
 value_name  = "weight_class_bins"
)

}

}
