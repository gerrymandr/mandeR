---
output:
  html_document: default
  pdf_document: default
---
mandeR: Compactness Calculation Tool
====================================

mandeR calculates a variety of compactness measures, which are useful for
detecting and analyzing gerrymandering of electoral districts.


Installation
------------

mandeR is available from CRAN via:

    install.packages('mandeR')

If you want your code to be as up-to-date as possible, you can install it using:

    library(devtools) #Use `install.packages('devtools')` if need be
    install_github('r-barnes/mandeR', vignette=TRUE)
    
A fix to Issue #1
-----------------

Thanks to @mahrud: 

In terminal:

```
git clone https://github.com/gerrymandr/mandeR.git
cd mandeR

git submodule init
git submodule update

cd ..
tar czf mandeR.tar.gz mandeR
```

Then in R:

```{r}
wd="" ## change to where you have mandeR.tar.gz
setwd(wd)

install.packages("mander.tar.gz", repos = NULL, type = "source")
```



Show me some code
-----------------

Okay.

```{r}
library(mandeR)

#Read a shapefile containing districts
dists  <- sf::st_read(mass_cd())

#Convert the shapefile to WKT 
wkt_str <- lapply(st_geometry(dists),st_as_text)

#Retrieve compactness scores from mandeR
scores <- lapply(wkt_str,mandeR::getScoresForWKT)
scores=do.call(rbind,scores)
scores$id=1:nrow(scores)

#Merge scores back into districts
dists<-merge(dists,scores,by.x="DIST_NUM",by.y="id")

#Plot districts showing each of the scores
plot(dists[mandeR::getListOfScores()])
```

Show me more examples!
----------------------

**Other Functions**

```{r}
augmentShapefileWithScores("path/to/myshapefile.shp",scores=c('all'))

addScoresToNewShapefile(mass_cd(), newname,scores=c('all'))
```

**Available Compactness Metrics**

```{r}
mandeR::getListOfScores()
```

Expanding on: https://github.com/gerrymandr/compactnesslib/blob/master/Scores.md

More info: https://arxiv.org/pdf/1803.02857.pdf

```CvxHullPT```:

The Convex Hull score is a ratio of the area of the district to the area of the minimum convex polygon that can enclose the district's geometry.

PT (polygons together) 

```CvxHullPS```:

PS (polygons separate)

```ReockPT```:

The Reock score is a measure of the ratio of the area of the district to the area of the minimum bounding circle that encloses the district's geometry.

PT (polygons together)

```ReockPS```:

PS (polygons separate)

```Schwartzbe```:

The Schwartzberg score is a ratio of the perimeter of the district to the circumference of a circle whose area is equal to the area of the district.

To generate the Schwartzberg score, first the circumference of a circle with an equal area of the district must be calculated. To do so, use the formula: $r=\sqrt{A/\pi}$. Then use the formula to generate circumference of a circle where $A$ is the area of the district and $r$ is the radius. With the radius calculated, use the following formula to generate the circumference (perimeter): $C=2\pi r$. Finally generate the Schwartzberg score using the following ratio: $\frac{1}{\frac{P}{C}}$ where $P$ is the perimeter of the district and $C$ is the circumeference (perimeter) of the circle with the same area.

```PolsbyPopp```:

The Polsby-Popper measure is a ratio of the area of the district to the area of a circle whose circumference is equal to the perimeter of the district. The formula for calculating the Polsby-Popper score is  $4\pi \frac{A}{P^2}$  where $A$ is the area of the district and $P$ is the perimeter of the district.

```PolyCount```:

```HoleCount```:

```perimSH```:

SH (subtract holes)

```areaAH```:

AH (add holes)



Roadmap
=======

 * A direct interface to sf (simple features) without GeoJSON as an intermediary.
   This will obviously be faster and easier, but represents a time commitment.
   If speed and improved ease of use are important to you, consider helping out!



Credits
=======

This R package was developed by:

 * Richard Barnes (http://rbarnes.org)
 * John Connors

Funding was provided by:

 * The Metric Geometry And Gerrymandering Group at Tufts University ([link](https://sites.tufts.edu/gerrymandr/))



Disclaimer
==========

This package *should* operate in the manner described here and in the package's
main documentation. Unfortunately, none of us are paid enough to make
absolutely, doggone certain that that's the case. Use at your own discretion.
That said, if you find bugs or are seeking enhancements, we want to hear about
them.



Citing this Package
===================

Please cite this package using the text returned by:

    citation('mandeR')
