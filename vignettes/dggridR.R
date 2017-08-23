## ---- results='hide', warning=FALSE, error=FALSE, message=FALSE, fig.width=10, fig.height=10----
#Include libraries
library(gerry)
library(sp)
library(ggplot2)

#identify the path of the sample shapefile of congressional districts
shapefilepath     <- mass_cd()          

#create a spatial data frame
shape             <- readOGR(shapefilepath)       

#Specify and equal area coordinate reference system
proj              <- test<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 
                                 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 
                                 +datum=NAD83 +units=m")
#reproject the data to the specified CRS
reproj            <- spTransform(shape,proj)      

#Create an ID number for each polgyon
reproj@data["id"] <- rownames(shape@data)

#create a vector of the nodes in the polygons for the spatial data frame
shpvec            <- fortify(reproj)
shpvec$id         <- as.numeric(shpvec$id)

#Calculate the Schwartzberg metric 
schwartz  <- Schwartzberg(shpvec$long, shpvec$lat, as.numeric(shpvec$id))

#Add Schwartzberg metric to data frame
shpvec <- left_join(shpvec,schwartz)

ggplot()+geom_polygon(data=shpvec,aes(x=long,y=lat,group=group,fill=val))

ggplot(shpvec, aes(x=long,y=lat,group=group)) + geom_polygon() + facet_wrap(~val, scales="free")

