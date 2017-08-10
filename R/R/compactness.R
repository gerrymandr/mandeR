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
mass_cd <- function(){
  file.path(find.package('dictompact'), "extdata", "CONGRESSMA_POLY.shp")
}



dgtransform <- function(dggs, lat, lon){ #TODO: Make sure we're not modifying the original dggs
  dgverify(dggs)

  warning("The 'dgtransform' function has been deprecated. Please use 'dgGEO_to_SEQNUM' instead!")

  dgGEO_to_SEQNUM(dggs, lon, lat)$seqnum
}

allmetricscalc <- function(shape){#Calculates all metrics and adds output metrics to the spatial dataframe attributes
  spdf<-sp::fortify(shape)
  pericol<-Perimeter(spdf)
  polsbypoppercol<-PolsbyPopper(spdf)
  schwartzcol<-Schwartzberg(spdf)
  shape@data["Perimeter"]<-pericol
  shape@data["PolsbyPopper"]<-polsbypoppercol
  shape@data["Schwartzberg"]<-schwartzcol
}


#' @title Calculate the perimeter of all polygons in a spatial data frame
#'
#' @description
#'        Returns a vector of values with the perimeters for all polygons in the Spatial Data Frame.
#'
#' @param nodevector A vector of the XY coordinates for all nodes in a polygon - generated using sp.fortify()
#'
#' @return A vector of the perimeter measurements for each polygon in the Spatial Data Frame.
#'
#' @examples
#'
#'    
#' library(sp)
#' library(ggplot2)
#' library(plyr)
#' shapefilepath<-mass_cd() #identify the path of the sample shapefile of congressional districts
#' shape<-readOGR(shapefilepath) #create a spatial data frame
#' #Specify and equal area coordinate reference system
#' proj=test<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m")
#' reproj<-spTransform(shape,proj) #reproject the data to the specified CRS
#' reproj@data["id"]<-rownames(shape@data) #Create an ID number for each polgyon
#' shape2<-fortify(reproj) #fortify the data, converting it to a vector of XY values
#' shpvector<-fortify(reproj) #create a vector of the nodes in the polygons for the spatial data frame
#' shpperim<-PerimeterCalc(shpvector) #Calculate the perimeter of the polygons and return a list of values
#' shpvector@data["Perimeter"]<-shpperim #Add a column of the perimeter values to the spatial data frame
#' ggplot()+
#'   geom_polygon(data=shape3,aes(x=long,y=lat,group=group,fill=shpvector$Perimeter))     
#'  
#' @export
PerimeterCalc<-function(nodevector){
  Perimeter(nodevector)
}

#' @title Calculate the Polsby Popper metric for each polygon in a spatial data frame
#'
#' @description
#'        Returns a vector of values with the Polsby Popper metric for all polygons in the Spatial Data Frame.
#'
#' @param nodevector A vector of the XY coordinates for all nodes in a polygon - generated using sp.fortify()
#'
#' @return A vector of the perimeter measurements for each polygon in the Spatial Data Frame.
#'
#' @examples
#'
#'    
#' library(sp)
#' library(ggplot2)
#' library(plyr)
#' shapefilepath<-mass_cd() #identify the path of the sample shapefile of congressional districts
#' shape<-readOGR(shapefilepath) #create a spatial data frame
#' #Specify and equal area coordinate reference system
#' proj=test<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m")
#' reproj<-spTransform(shape,proj) #reproject the data to the specified CRS
#' reproj@data["id"]<-rownames(shape@data) #Create an ID number for each polgyon
#' shape2<-fortify(reproj) #fortify the data, converting it to a vector of XY values
#' shpvector<-fortify(reproj) #create a vector of the nodes in the polygons for the spatial data frame
#' polsbypop<-PolsbyPopper(shpvector) #Calculate the Polsby Popper metrics of the polygons and return a list of values
#' shpvector@data["PolsbyPopper"]<-polsbypop #Add a column of the perimeter values to the spatial data frame
#' ggplot()+
#'   geom_polygon(data=shape3,aes(x=long,y=lat,group=group,fill=shpvector$PolsbyPopper))     
#'  
#' @export

PolsbyPopper<-function(nodevector){
  PolsbyPopper(nodevector)
}
