#' @importFrom utils read.csv read.table tail write.table
#' @importFrom rgdal readOGR
#' @importFrom rgdal writeOGR
#' @importFrom ggplot2 fortify
#' @importFrom methods as
#' @import     dplyr
#' @import     sp
#' @useDynLib  dictompact

#' @import rgeos


#' @name dg_exe_path
#' 
#' @title Get path to dggrid executable
#'
#' @description
#'        Returns a path to the dggrid executable. Used for running stuff.
#'
#' @return A string representing the path to the dggrid executable.
#'

dgtransform <- function(dggs, lat, lon){ #TODO: Make sure we're not modifying the original dggs
  dgverify(dggs)

  warning("The 'dgtransform' function has been deprecated. Please use 'dgGEO_to_SEQNUM' instead!")

  dgGEO_to_SEQNUM(dggs, lon, lat)$seqnum
}


allmetricscalc <- function(shape){#Calculates all metrics and adds output metrics to the spatial dataframe attributes
  spdf<-fortify(shape)
  pericol<-Perimeter(spdf)
  polsbypoppercol<-PolsbyPopper(spdf)
  schwartzcol<-Schwartzberg(spdf)
  shape@data["Perimeter"]<-pericol
  shape@data["PolsbyPopper"]<-polsbypoppercol
  shape@data["Schwartzberg"]<-schwartzcol
}

PerimeterCalc<-function(nodevector){
  PerimeterVector<-Perimeter(nodevector)
}

PolsbyPopper<-function(nodevector){

}
