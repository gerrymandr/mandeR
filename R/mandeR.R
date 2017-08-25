#' @importFrom utils read.csv
#' @importFrom sf st_read st_as_text
#' @useDynLib  mandeR

#roxygen2:::roxygenize()

#' @title Get an example shapefile
#'
#' @description
#'        Returns the path of an example shapefile of the electoral districts of 
#'        Massachusetts.
#'
#' @return A string representing the path to the example shapefile
#'
#' @examples
#' file.exists(mass_cd())
#'
#' @export
mass_cd <- function(){
  file.path(find.package('mandeR'), "extdata", "CONGRESSMA_POLY.shp")
}



#' @title Retrieve a list of available scores
#'
#' @description
#'        mandeR, through compactnesslib, has access to a growing number of
#'        compactness scores. This function provides a list of them. Subsets of
#'        this list can be passed to other functions to restrict which scores
#'        are calculated. Descriptions of scores may be found in the vignettes.
#'
#' @return A list of scores names, which are represented by character strings.
#'
#' @examples
#' mandeR::getListOfScores()
#'
#' @export
getListOfScores <- function(){
  cl_getListOfScores()
}



#' @title Augment an existing shapefile by adding scores
#'
#' @description
#'        Since analysis often happens at a high level, after scores have been
#"        calculated, it is often easiest to add scores to the underlying
#'        shapefile itself. This function does this. The shapefile's projection
#'        must be appropriate: latlong cannot be used.
#'
#' @param filename Filename of the shapefile to be altered.
#' @param scores   List of scores to include, a subset of `getListOfScores()`
#'
#' @return The filename of the modified shapefile.
#'
#' @examples
#' \dontrun{
#' library(mandeR)
#' mandeR::augmentShapefileWithScores("path/to/myshapefile.shp", c("Reock", "area"))
#' }
#'
#' @export
augmentShapefileWithScores <- function(filename, scores=c('all')){
  cl_augmentShapefileWithScores(filename, scores)
  filename
}



#' @title Augment an existing shapefile by adding scores
#'
#' @description
#'        Since analysis often happens at a high level, after scores have been
#"        calculated, it is often easiest to add scores to the underlying
#'        shapefile itself. This function does this. The shapefile's projection
#'        must be appropriate: latlong cannot be used.
#' 
#' @param in_filename  Filename of the shapefile to be scored.
#' @param out_filename Filename of the shapefile that will hold the results.
#' @param scores       List of scores to include, a subset of `getListOfScores()`
#'
#' @return The filename of the new shapefile.
#'
#' @examples
#' library(mandeR)
#' newname <- tempfile()
#' mandeR::addScoresToNewShapefile(mass_cd(), newname)
#'
#' @export
addScoresToNewShapefile <- function(in_filename, out_filename, scores=c('all')){
  cl_addScoresToNewShapefile(in_filename, out_filename, scores)
  out_filename
}



#' @title Get a dataframe of scores from a WKT input
#'
#' @description
#'        WKT is used to pass information between compactnesslib and its
#'        higher level interfaces. This function takes a WKT string of a polygon
#'        or a multipolygons and returns its scores as a data frame. The
#'        coordinates must be appropriate: latlong cannot be used.
#' 
#' @param wkt_str      WKT representing feature to be scored.
#' @param scores       List of scores to include, a subset of `getListOfScores()`
#'
#' @return A data frame with scores as columns, including the id column.
#'
#' @examples
#' library(mandeR)
#' dists   <- sf::st_read(mass_cd())
#' wkt_str <- st_as_text(st_geometry(dists)[[1]])
#' mandeR::getScoresForWKT(wkt_str)
#'
#' @export
getScoresForWKT <- function(wkt_str, scores=c('all')){
  gj <- cl_getScoresForWKT(wkt_str, scores)
  read.csv(text=gj)
}
