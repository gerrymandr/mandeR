#' @importFrom utils read.csv
#' @importFrom sf st_read
#' @importFrom geojsonio geojson_json
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
#' @export
mass_cd <- function(){
  file.path(find.package('gerry'), "extdata", "CONGRESSMA_POLY.shp")
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
#' @export
getListOfScores <- function(){
  cl_getListOfScores()
}



#' @title Augment an existing shapefile by adding scores
#'
#' @description
#'        Since analysis often happens at a high level, after scores have been
#"        calculated, it is often easiest to add scores to the underlying
#'        shapefile itself. This function does this.
#'
#' @param filename Filename of the shapefile to be altered.
#' @param scores   List of scores to include, a subset of `getListOfScores()`
#'
#' @return The filename of the modified shapefile.
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
#'        shapefile itself. This function does this.
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
#' mandeR::augmentShapefileWithScores(mass_cd(), newname)
#'
#' @export
augmentShapefileWithScores <- function(in_filename, out_filename, scores=c('all')){
  cl_addScoresToNewShapefile(in_filename, out_filename, scores)
  out_filename
}



#' @title Get a dataframe of scores from a GeoJSON input
#'
#' @description
#'        GeoJSON is used to pass information between compactnesslib and its
#'        higher level interfaces. (Well-known text could be used, but we don't
#'        have a parser for that, yet. Volunteer?) This function takes a GeoJSON
#'        description of one or more multipolygons and returns their scores as a
#'        data table.
#' 
#' @param geojson_str  GeoJSON representing features to be scored.
#' @param id           Attribute id to key features in the resulting table. If
#'                     omitted, numeric keys are returned based on the input
#'                     order of the features.
#' @param scores       List of scores to include, a subset of `getListOfScores()`
#'
#' @return A data frame with scores as columns, including the id column.
#'
#' @examples
#' library(mandeR)
#' dists <- sf::st_read(mass_cd())
#' gj    <- geojsonio::geojson_json(dists)
#' mandeR::getScoresForGeoJSON(gj, 'DIST_NUM')
#'
#' @export
getScoresForGeoJSON <- function(geojson_str, id='', scores=c('all')){
  gj <- cl_getScoresForGeoJSON(geojson_str, id, scores)
  read.csv(text=gj)
}
