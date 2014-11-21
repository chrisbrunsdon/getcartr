#' Visual representation of cartogram transformation
#'
#' Given a transformed Spatial object draws the warping grid 
#'
#' 
#' @export
#' @param spdf  A \code{SpatialPolygonsDataFrame}  created via the \code{quick.carto} function or similar
#' @param add   Whether to add the graphic to an existing plot
#' @param col  Colour of the graphic
#' @references 
#'     A Diffusion-based method for producing density equalizing maps, Michael T. Gastner and M. E. J. Newman, Proc. Natl. Acad. Sci. USA 101, 7499-7504 (2004)
#' @keywords cartogram
#' @seealso \link{quick.carto} \link{dispersion}
#' @examples
#' # Load an example SpatialPolygonsDataFrame from GISTools
#' data(georgia)
#' # Create the cartogram.  TotPop90 contains 1990 county populations
#' # "georgia2" is used as it has a plane projection coordinate system
#' georgia.carto <- quick.carto(georgia2,georgia2$TotPop90)
#' # Draw the mesh
#' mesh(georgia.carto)
#' # Add the transformed map
#' plot(georgia.carto,add=TRUE)



mesh <- function(sldf,add=FALSE,col='red') {
  grat <- .graticule(sldf)
  wgrat <- warp.lines(grat,sldf)
  if (add) {
    plot(wgrat,add=TRUE,col=col)
  }
  else {
    plot(wgrat,col=col)
  }
}
