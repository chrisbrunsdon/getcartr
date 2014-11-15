#' @title Warp a SpatialPolygons or SpatialPolygonsDataFrame object according a to cartogram transform
#' @description
#' Given a \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} object,  apply a cartogram transform
#' according to the transform grid of a \code{SpatialPolygonsDataFrame} created via \code{quick.carto}
#' The returned values is the transformed \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame}. 
#'
#' @param spdf a \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} object to be transformed
#' @param warper a \code{SpatialPolygonsDataFrame} or \code{SpatialPolygons} object resulting from a previous \code{quick.carto} evocation
#' @return a \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} object resulting from the transformation.
#' 
#' @references A Diffusion-based method for producing density equalizing maps, Michael T. Gastner and M. E. J. Newman, Proc. Natl. Acad. Sci. USA 101, 7499-7504 (2004)
#' @family transformers
#' @keywords cartogram
#' @export
#' @examples
#' 
#' # Make a cartogram for Georgia counties
#' data(georgia)
#' georgia.carto <- quick.carto(georgia2,georgia2$TotPop90)
#' 
#' # Create a pseudocircle with 100 segments
#' xl <- cos(seq(0,2*pi,l=100))*100000 + 1239348
#' yl <- sin(seq(0,2*pi,l=100))*100000 + 1093155
#' circ <- readWKT(paste("MULTIPOLYGON(((",paste(apply(cbind(xl,yl),1,function(x) sprintf("%7.0f %7.0f",x[1],x[2])),collapse=","),")))"))
#' #' #  Transform it
#' circ.carto <- warp.polys(circ,georgia.carto)
#' 
#' # Plot the original circle
#' par(mfrow=c(2,1),mar=c(0.5,0.5,3,0.5))
#' plot(georgia2,border='grey'); plot(circ, add=T, col=rgb(1,0,0,0.4), border=NA)
#' title('Original projection')
#' 
#' # Plot in cartogram space
#' plot(georgia.carto,border='grey'); plot(circ.carto, add=T, col=rgb(1,0,0,0.4), border=NA)
#' title('Cartogram projection')
#' 
#' 
#' 

warp.polys <- function(spdf,warper) {
  wg <- attr(warper,'warp')
  if (is.null(wg)) stop("Cartogram object not created via 'quick.carto'")
  warpverb <- function(xy,wg) {
    cents = attr(wg,"cents")
    sizes = attr(wg,"sizes")
    xyt = t((t(xy)-cents)/sizes)
    .interpolate(xyt,wg)}
  .apply.polys(spdf,warpverb,wg) }
