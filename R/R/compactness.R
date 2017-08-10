#' @importFrom utils read.csv read.table tail write.table
#' @importFrom rgdal readOGR
#' @importFrom rgdal writeOGR
#' @importFrom ggplot2 fortify
#' @importFrom methods as
#' @import     dplyr
#' @import     sp
#' @useDynLib  gerry

#' @import rgeos


#' @title Get path to dggrid executable
#'
#' @description
#'        Returns a path to the dggrid executable. Used for running stuff.
#'
#' @return A string representing the path to the dggrid executable.
#'
#' @export
mass_cd <- function(){
  file.path(find.package('gerry'), "extdata", "CONGRESSMA_POLY.shp")
}


#' @title TODO
#'
#' @description
#'        TODO
#'
#' @return TODO
#'
#' @export

#Calculates all metrics and adds output metrics to the spatial dataframe attributes
allmetricscalc <- function(shape){
  spdf<-sp::fortify(shape)
  pericol<-Perimeter(spdf)
  polsbypoppercol<-PolsbyPopper(spdf)
  schwartzcol<-Schwartzberg(spdf)
  shape@data["Perimeter"]<-pericol
  shape@data["PolsbyPopper"]<-polsbypoppercol
  shape@data["Schwartzberg"]<-schwartzcol
}


#' @title Calculate the perimeter one or more polygons
#'
#' @description
#'        Calculates the perimeter of one or more polygons from a spatial data
#'        frame. The frame can be generated with sp::fortify()
#'
#' @param  x   X-coordinates of the polygon nodes
#' @param  y   Y-coordinates of the polygon nodes
#' @param  id  ids which indicate to which polygons each xy-coordinate belongs
#'
#' @return The perimeter of one polygon or a dataframe of id-perimeter pairs
#'
#' @examples
#'
#' library(sp)
#' library(ggplot2)
#' library(plyr)
#' library(gerry)
#' #identify the path of the sample shapefile of congressional districts
#' shapefilepath     <- mass_cd()                    
#' #create a spatial data frame
#' shape             <- readOGR(shapefilepath)       
#' #Specify and equal area coordinate reference system
#' proj              <- test<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 
#'                                 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 
#'                                 +datum=NAD83 +units=m")
#' #reproject the data to the specified CRS
#' reproj            <- spTransform(shape,proj)      
#' #Create an ID number for each polgyon
#' reproj@data["id"] <- rownames(shape@data)                   
#' #create a vector of the nodes in the polygons for the spatial data frame
#' shpvector         <- fortify(reproj)              
#' sdict             <- shpvector %>% filter(group==0.1)
#' #Calculate the perimeter of the polygons and return a list of values
#' shpperim          <- PerimeterCalc(sdict$long, sdict$lat, sdict$id) 
#' #Add a column of the perimeter values to the spatial data frame
#' shpvector@data["Perimeter"]<-shpperim         
#' ggplot()+
#'   geom_polygon(data=shape3,aes(x=long,y=lat,group=group,
#'                fill=shpvector$Perimeter))     
#'  
#' @export

PerimeterCalc<-function(x,y,id=NA){
  if(is.na(id))
    gerry:::gPerimeter(x,y)
  else
    gerry:::gPerimeterMulti(x,y,id)
}


#' @title Calculate the area for one or multiple polygons in a
#'        Shapefile
#'
#' @description
#'        Returns a vector of values with the area for all 
#'        polygons in the Spatial Data Frame.
#'
#' @param  x   X-coordinates of the polygon nodes
#' @param  y   Y-coordinates of the polygon nodes
#' @param  id  ids which indicate to which polygons each xy-coordinate belongs
#' 
#' @return  The area of polygon(s) 
#' @examples
#'   
#' library(sp)
#' library(ggplot2)
#' library(plyr)
#' library(gerry)
#' #identify the path of the sample shapefile of congressional districts
#' shapefilepath     <- mass_cd()                    
#' #create a spatial data frame
#' shape             <- readOGR(shapefilepath)       
#' #Specify and equal area coordinate reference system
#' proj              <- test<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 
#'                                 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 
#'                                 +datum=NAD83 +units=m")
#' #reproject the data to the specified CRS
#' reproj            <- spTransform(shape,proj)      
#' #Create an ID number for each polgyon
#' reproj@data["id"] <- rownames(shape@data)                   
#' #create a vector of the nodes in the polygons for the spatial data frame
#' shpvector         <- fortify(reproj)              
#' sdict             <- shpvector %>% filter(group==0.1)
#' #Calculate the area of the polygons and return a list of values
#' parea        <- AreaCalc(sdict$long, sdict$lat, sdict$id) 
#' #Add a column of the perimeter values to the spatial data frame
#' shpvector@data["Area"]<- parea         
#' ggplot()+
#'   geom_polygon(data=shape3,aes(x=long,y=lat,group=group,
#'                fill=shpvector$Area))     
#'  
#' @export
AreaCalc<-function(x,y,id=NA){
  if(is.na(id))
    gerry:::gPolygonArea(x,y)
  else
    gerry:::gPolygonAreaMulti(x,y,id)
}


#' @title Calculate Polsby Popper Metric for one or multiple polygons in a
#'        Shapefile
#'
#' @description
#'        Returns a vector of values with the Polsby Popper metric for all 
#'        polygons in the Spatial Data Frame.
#'
#' @param  x   X-coordinates of the polygon nodes
#' @param  y   Y-coordinates of the polygon nodes
#' @param  id  ids which indicate to which polygons each xy-coordinate belongs
#' 
#' @return  The Polsby Popper metrics 
#' @examples
#'   
#' library(sp)
#' library(ggplot2)
#' library(plyr)
#' library(gerry)
#' #identify the path of the sample shapefile of congressional districts
#' shapefilepath     <- mass_cd()                    
#' #create a spatial data frame
#' shape             <- readOGR(shapefilepath)       
#' #Specify and equal area coordinate reference system
#' proj              <- test<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 
#'                                 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 
#'                                 +datum=NAD83 +units=m")
#' #reproject the data to the specified CRS
#' reproj            <- spTransform(shape,proj)      
#' #Create an ID number for each polgyon
#' reproj@data["id"] <- rownames(shape@data)                   
#' #create a vector of the nodes in the polygons for the spatial data frame
#' shpvector         <- fortify(reproj)              
#' sdict             <- shpvector %>% filter(group==0.1)
#' #Calculate the Polsby Popper metric of the polygons and return a list of values
#' PolPop        <- PolsbyPopper(sdict$long, sdict$lat, sdict$id) 
#' #Add a column of the perimeter values to the spatial data frame
#' shpvector@data["PolsbyPopper"]<- PolPop         
#' ggplot()+
#'   geom_polygon(data=shape3,aes(x=long,y=lat,group=group,
#'                fill=shpvector$PolsbyPopper))     
#'  
#' @export


PolsbyPopper<-function(x,y,id=NA){
  if(is.na(id))
    gerry:::gPolsbyPopper(x,y)
  else
    gerry:::gPolsbyPopperMulti(x,y,id)
}


#' @title Calculate Schwartzberg Metric for one or multiple polygons in a
#'        Shapefile
#'
#' @description
#'        Returns a vector of values with the Schwartzberg metric for all 
#'        polygons in the Spatial Data Frame.
#'
#' @param  x   X-coordinates of the polygon nodes
#' @param  y   Y-coordinates of the polygon nodes
#' @param  id  ids which indicate to which polygons each xy-coordinate belongs
#' 
#' @return  The Schwartzberg metrics 
#' @examples
#'   
#' library(sp)
#' library(ggplot2)
#' library(plyr)
#' library(gerry)
#' #identify the path of the sample shapefile of congressional districts
#' shapefilepath     <- mass_cd()                    
#' #create a spatial data frame
#' shape             <- readOGR(shapefilepath)       
#' #Specify and equal area coordinate reference system
#' proj              <- test<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 
#'                                 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 
#'                                 +datum=NAD83 +units=m")
#' #reproject the data to the specified CRS
#' reproj            <- spTransform(shape,proj)      
#' #Create an ID number for each polgyon
#' reproj@data["id"] <- rownames(shape@data)                   
#' #create a vector of the nodes in the polygons for the spatial data frame
#' shpvector         <- fortify(reproj)              
#' sdict             <- shpvector %>% filter(group==0.1)
#' #Calculate the Convex Hull metric of the polygons and return a list of values
#' schwartz          <- Schwartzberg(sdict$long, sdict$lat, sdict$id) 
#' #Add a column of the Schwartzburg values to the spatial data frame
#' shpvector@data["Schwartzberg"]<- schwartz        
#' ggplot()+
#'   geom_polygon(data=shape3,aes(x=long,y=lat,group=group,
#'   fill=shpvector$Schwartzberg))     
#'  
#' @export


ConvexHull<-function(x,y,id=NA){
  if(is.na(id)){
    gerry:::gSchwartzberg(x,y)
  }
  else{
    gerry:::gSchwartzbergMulti(x,y,id)
  }
}


#' @title Calculate Convex Hull Metric for one or multiple polygons in a
#'        Shapefile
#'
#' @description
#'        Returns a vector of values with the Convex Hull / Area metric for all 
#'        polygons in the Spatial Data Frame.
#'
#' @param  x   X-coordinates of the polygon nodes
#' @param  y   Y-coordinates of the polygon nodes
#' @param  id  ids which indicate to which polygons each xy-coordinate belongs
#' 
#' @return  The Convex Hull metrics 
#' @examples
#'   
#' library(sp)
#' library(ggplot2)
#' library(plyr)
#' library(gerry)
#' #identify the path of the sample shapefile of congressional districts
#' shapefilepath     <- mass_cd()                    
#' #create a spatial data frame
#' shape             <- readOGR(shapefilepath)       
#' #Specify and equal area coordinate reference system
#' proj              <- test<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 
#'                                 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 
#'                                 +datum=NAD83 +units=m")
#' #reproject the data to the specified CRS
#' reproj            <- spTransform(shape,proj)      
#' #Create an ID number for each polgyon
#' reproj@data["id"] <- rownames(shape@data)                   
#' #create a vector of the nodes in the polygons for the spatial data frame
#' shpvector         <- fortify(reproj)              
#' sdict             <- shpvector %>% filter(group==0.1)
#' #Calculate the Convex Hull metric of the polygons and return a list of values
#' conHull       <- ConvexHull(sdict$long, sdict$lat, sdict$id) 
#' #Add a column of the Convex Hull values to the spatial data frame
#' shpvector@data["ConvexHull"]<- conHull        
#' ggplot()+
#'   geom_polygon(data=shape3,aes(x=long,y=lat,group=group,
#'   fill=shpvector$ConvexHull))     
#'  
#' @export


ConvexHull<-function(x,y,id=NA){
  if(is.na(id)){
    gerry:::gConvexHull(x,y)
  }
  else{
    gerry:::gConvexHullMulti(x,y,id)
  }
}



