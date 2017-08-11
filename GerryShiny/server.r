library(shiny)
library(maptools)
library(sp)
library(gerry)
library(plyr)
library(DT)
library(sf)
library(rgeos)

function(input, output,session){
  #create a spatial data frame
  shape <- readOGR("/Users/jpcc/WorkSpace/gerrymander/out/","conv")
  shape@data$id<-rownames(shape@data)
  #Create Spatial Data Frame for Simplied Polygons
  globalbase<-gSimplify(shape,tol=10000,topologyPreserve=TRUE)
  globalbase<-SpatialPolygonsDataFrame(globalbase,data=shape@data)
  
  pca<-prcomp
  #highlight selected districts on the map
  output$map<-renderPlot({
    s=input$table_rows_selected
    output$table2<-DT::renderDataTable(shape@data[s,],server=TRUE)
    include <- shape@data$id %in% s 
    shape2<-shape[include,]
    sf_poly<-as(shape2,"sf")
    #sf::st_crs(sf_poly)<-4326
    ggplot(sf_poly)+
      geom_sf(aes(fill=as.factor(id)))+
      facet_wrap(~id,scales="free")
  })
  output$globalMap<-renderPlot({
    metricchoice=input$metrics
    s=input$table_rows_selected
    include <- globalbase@data$id %in% s 
    selected<-globalbase[include,]
    sf_poly<-as(globalbase,"sf")
    sf_poly2<-as(selected,"sf")
    #sf::st_crs(sf_poly)<-4326
    ggplot()+
      geom_sf(data=sf_poly,aes_(fill=as.name(metricchoice)),color=NA)+
      geom_sf(data=sf_poly2,color="red",aes_(fill=as.name(metricchoice)))
  })
  output$table<-DT::renderDataTable(shape@data,server=TRUE)
}