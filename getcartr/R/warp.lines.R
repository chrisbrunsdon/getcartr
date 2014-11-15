#' @title Warp a SpatialLines or SpatialLinesDataFrame object according a to cartogram transform
#' @description
#' Given a \code{SpatialLines} or \code{SpatialLinesDataFrame} object,  apply a cartogram transform
#' according to the transform grid of a \code{SpatialPolygonsDataFrame} created via \code{quick.carto}
#' The returned values is the transformed \code{SpatialLines} or \code{SpatialLinesDataFrame}. 
#'
#' @param sldf a \code{SpatialLines} or \code{SpatialLinesDataFrame} object to be transformed
#' @param warper a \code{SpatialPolygonsDataFrame} or \code{SpatialPolygons} object resulting from a previous \code{quick.carto} evocation
#' @return a \code{SpatialLines} or \code{SpatialLinesDataFrame} object resulting from the transformation.
#' 
#' @references A Diffusion-based method for producing density equalizing maps, Michael T. Gastner and M. E. J. Newman, Proc. Natl. Acad. Sci. USA 101, 7499-7504 (2004)
#' 
#' @family transformers
#' @keywords cartogram
#' @export
#' @examples
#' 
#' # Make a cartogram for Georgia counties
#' data(georgia)
#' georgia.carto <- quick.carto(georgia2,georgia2$TotPop90)
#' 
#' # Create a line from southwest to northeast corners,  with 100 segments
#' xl <- seq(939220.7,1419424,l=100)
#' yl <- seq(905508,1405900,l=100)
#' aline <- readWKT(paste("LINESTRING(",paste(apply(cbind(xl,yl),1,function(x) paste(x[1],x[2])),collapse=","),")"))
#' 
#' #  Transform it
#' aline.carto <- warp.lines(aline,georgia.carto)
#' 
#' # Plot the original line
#' par(mfrow=c(2,1),mar=c(0.5,0.5,3,0.5))
#' plot(georgia2,border='grey'); plot(aline, add=T, col='red')
#' title('Original projection')
#' 
#' # Plot in cartogram space
#' plot(georgia.carto,border='grey'); plot(aline.carto, add=T, col='red')
#' title('Cartogram projection')
#' 
#' 
#' 

warp.lines <- function(sldf,warper) {
  wg <- attr(warper,'warp')
  if (is.null(wg)) stop("Cartogram object not created via 'quick.carto'")
  warpverb <- function(xy,wg) {
    cents = attr(wg,"cents")
    sizes = attr(wg,"sizes")
    xyt = t((t(xy)-cents)/sizes)
    .interpolate(xyt,wg)}
  .apply.lines(sldf,warpverb,wg) }
