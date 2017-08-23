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



Show me some code
-----------------

Okay.

    #Load library
    library(mandeR)

    #Read a shapefile containing districts
    dists  <- sf::st_read(mass_cd())

    #Convert the shapefile to GeoJSON
    gj     <- geojsonio::geojson_json(dists)

    #Retrieve compactness scores from mandeR
    scores <- mandeR::getScoresForGeoJSON(gj, 'DIST_NUM', c('all'))

    #Merge scores back into districts
    dists  <- merge(dists, scores, by.x="DIST_NUM", by.y="id")

    #Plot districts showing each of the scores
    plot(dists[mandeR::getListOfScores()])



Show me more examples!
----------------------

In R, typing

    vignette('mandeR')

will bring up many examples.




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
