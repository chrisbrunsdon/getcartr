#' Create a cartogram SpatialPolygonsDataFrame
#'
#' Given a \code{SpatialPolygonsDataFrame} and a value associated with each polygon 
#' creates a new \code{SpatialPolygonsDataFrame} which is a cartogram of the input
#' whose expansion factors are based on the value divided by the polygon area.
#'
#' @export
#' @param spdf A \code{SpatialPolygonsDataFrame}
#' @param v The variable providing the values - typically population counts
#' @param extend The extent beyond the limits of the \code{SpatialPolygonsDataFrame} to estimate the warping as a proportion of span
#' @param res The resolution of the grid for the warping
#' @param index Index of data item to be used for cartogram - only used if \code{v} is missing
#' @param thresh Lowest density value allowed - will replace pixels with \code{max(value,thresh)}
#' @param blur Degree of Gaussian blur to apply to density grid,  prior to computing the cartogram transform - default is 0
#' @return A \code{SpatialPolygonsDataFrame} containing the cartogram.  Also has an 
#' attribute - \code{attr(spdf,'mesh')} - containing information about the cartogram
#' warping grid 
#' @seealso \link{mesh}
#' @references 
#' A Diffusion-based method for producing density equalizing maps, Michael T. Gastner and M. E. J. Newman, Proc. Natl. Acad. Sci. USA 101, 7499-7504 (2004)
#' @keywords cartogram
#' @examples
#' data(georgia)
#' georgia.carto <- quick.carto(georgia,georgia2$TotPop90)
#' par(mfrow=c(1,2))
#' plot(georgia2)
#' plot(georgia.carto)

quick.carto <- function(spdf,v,extend=0.05,res=128,index=1,thresh=0,blur=0) {
  cart.pix <- .poly2grid(spdf,v,extend,res,index,thresh)
  cart.warp <- .cartogram.WarpGrid(cart.pix,blur=blur)
  res <- .direct.warp.polys(spdf,cart.warp)
  attr(res,"warp") <- cart.warp
  return(res)}
