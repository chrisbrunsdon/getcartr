#' @title Warp a SpatialPoints or SpatialPointsDataFrame object according a to cartogram transform
#' @description
#' Given a \code{SpatialPoints} or \code{SpatialPointsDataFrame} object,  apply a cartogram transform
#' according to the transform grid of a \code{SpatialPolygonsDataFrame} created via \code{quick.carto}
#' The returned values is the transformed \code{SpatialPoints} or \code{SpatialPointsDataFrame}. 
#'
#' @param pts a \code{SpatialPoints} or \code{SpatialPointsDataFrame} object to be transformed
#' @param warper a \code{SpatialPolygonsDataFrame} or \code{SpatialPolygons} object resulting from a previous \code{quick.carto} evocation
#' @return a \code{SpatialPoints} or \code{SpatialPointsDataFrame} object resulting from the transformation.
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
#' # Create a set of 25 points from northwest to southeast corners
#' xl <- seq(1419424,939220.7,l=25)
#' yl <- seq(905508,1405900,l=25)
#' pset <- readWKT(paste("MULTIPOINT(",paste(apply(cbind(xl,yl),1,function(x) paste("(",x[1],x[2],")")),collapse=","),")"))
#' #' #  Transform it
#' pset.carto <- warp.points(pset,georgia.carto)
#' 
#' # Plot the original points
#' par(mfrow=c(2,1),mar=c(0.5,0.5,3,0.5))
#' plot(georgia2,border='grey'); plot(pset, add=T, col='red')
#' title('Original projection')
#' 
#' # Plot in cartogram space
#' plot(georgia.carto,border='grey'); plot(pset.carto, add=T, col='red')
#' title('Cartogram projection')
#' 
#' 
#' 

warp.points <- function(pts,warper) {
  wg <- attr(warper,'warp')
  if (is.null(wg)) stop("Cartogram object not created via 'quick.carto'")
  cents = attr(wg,"cents")
  sizes = attr(wg,"sizes")
  xyt = t((t(coordinates(pts)) - cents)/sizes)
  result = SpatialPoints(.interpolate(xyt,wg))
  if (class(pts) == "SpatialPointsDataFrame") {
    result = SpatialPointsDataFrame(result,slot(pts,'data'))}
  result}
