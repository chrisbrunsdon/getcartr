#' @title Create a warping function to apply to Spatial* and Spatial*DataFrame objects
#' @description
#' Given an object created by \code{quick.carto},  create a cartogram transform.
#' The returned value is a function that performs the transform.
#'
#' @param poly a \code{SpatialPolygonsDataFrame} created by \code{quick.carto}
#' @return a function taking a \code{Spatial*} or \code{Spatial*DataFrame} object,  returning a warped version of the object
#' 
#' @references A Diffusion-based method for producing density equalizing maps, Michael T. Gastner and M. E. J. Newman, Proc. Natl. Acad. Sci. USA 101, 7499-7504 (2004)
#' @family transformers
#' @keywords cartogram
#' @export
#' @examples
#' 
#' # Make a cartogram for Newhaven census blocks
#' require(GISTools)
#' data(newhaven)
#' blocks.carto <- quick.carto(blocks,blocks$POP1990)
#' # Create the transform function
#' to.carto <- quick.carto.transform(blocks.carto)
#' # Plot the blocks carto
#' plot(blocks.carto)
#' # Add roads,  transformed to cartogram space
#' plot(to.carto(roads),add=TRUE,col='lightgrey')
#' # Add forced entry residential burglaries, transformed to cartogram space
#' plot(to.carto(burgres.f),add=TRUE,col='red',pch=16)
#' 
#' 
#' 
quick.carto.transform <- function(poly) {
  wg <- attr(poly,"warp")
  if (is.null(wg)) stop("Object not created by quick.carto")
  result <- function(obj) {
    if (grepl("SpatialPolygons",class(obj))) return(warp.polys(obj,poly))
    if (grepl("SpatialPoints",class(obj))) return(warp.points(obj,poly))
    if (grepl("SpatialLines",class(obj))) return(warp.lines(obj,poly))
    stop("Could not identify object to be warped as class Spatial*")
  }
  return(result)
}   
