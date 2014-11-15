#' A front end for the Rcartogram package
#'
#' Duncan Temple Lang's \pkg{Rcartogram} package (\url{http://www.omegahat.org/Rcartogram/}) is a bridge to the Gastner and Newman cartogram algorithm
#' that finds a cartogram transform from a 2-dimensional grid of densities.  This provides a front end to
#' allow cartogram transforms to be applied to \code{SpatialPolygonsDataFrame} objects.
#' Once an initial transform has been computed,  it may be applied to further \code{SpatialPolygons},
#' \code{SpatialLines} and \code{SpatialPoints} objects,  and their \code{Spatial*DataFrame} counterparts.
#'
#' \tabular{ll}{ Package: \tab getcartr\cr Type: \tab Package\cr Version: \tab
#' 0.9\cr Date: \tab 2013-03-09\cr License: \tab GPL 3.0\cr }
#' @name getcartr-package
#' @aliases getcartr-package getcartr
#' @docType package
#' @author Chris Brunsdon
#' 
#' Maintainer: Chris Brunsdon (\email{brunsdon@@liverpool.ac.uk})
#' 
#' @note It is necessary to install the non-CRAN \pkg{Rcartogram} package to use this package,  
#' as well as \pkg{GISTools} (available on CRAN) and its dependencies.
#'
#' @references A Diffusion-based method for producing density equalizing maps, Michael T. Gastner and M. E. J. Newman, Proc. Natl. Acad. Sci. USA 101, 7499-7504 (2004)
#' @keywords cartogram
#' @note
#' The cartogram transforms are applied to the points used to build up polygons or line segments,  but the lines joining the points will
#' still be represented as line segments, rather than curves.  For this reason,  it is important to avoid very long line segments,
#' and split these into smaller subsegments,  even if all of the points on the subsegments are collinear.  I will hopefully add a function to
#' do this in the future.
#' 
#' @examples
#' data(georgia)
#' georgia.carto <- quick.carto(georgia2,georgia2$TotPop90)
#' par(mfrow=c(2,1),mar=c(0.5,0.5,3,0.5))
#' plot(georgia.carto)
#' title('Cartogram')
#' mesh(georgia.carto)
#' title('Transformation Graticule')
#' 
NULL



