#' Visual representation of cartogram transformation
#'
#' Given a transformed Spatial object draws the dispersion cloud
#'
#' 
#' @export
#' @param spdf  A \code{Spatial*} object created via the \code{quick.carto} function, either directly or indirectly via \code{carto.transform}
#' @param add   Whether to add the graphic to an existing plot
#' @param col  Colour of the graphic
#' @references 
#'     A Diffusion-based method for producing density equalizing maps, Michael T. Gastner and M. E. J. Newman, Proc. Natl. Acad. Sci. USA 101, 7499-7504 (2004)
#' @keywords cartogram
#' @seealso \link{quick.carto} \link{mesh}
#' @examples
#' # Load an example SpatialPolygonsDataFrame from GISTools
#' data(georgia)
#' # Create the cartogram.  TotPop90 contains 1990 county populations
#' # "georgia2" is used as it has a plane projection coordinate system
#' georgia.carto <- quick.carto(georgia2,georgia2$TotPop90)
#' # Draw the dispersion cloud
#' dispersion(georgia.carto)
#' # Superimpose the transformed map
#' plot(georgia.carto,add=TRUE)

dispersion <- function(spdf,add=FALSE,col='red') {
  grid <- attr(spdf,'warp')
  if (is.null(grid)) stop("Object not created via 'quick.carto'")
  dims = .grdim(grid)
  steps = min((dims-2)/32)
  x <- grid$x
  y <- grid$y
  if (add) {
    points(x,y,pch='.',col=col)
  } else {
  plot(x,y,axes=FALSE,asp=1,pch='.',xlim=c(0,dims[1]+1),ylim=c(0,dims[2]+1),col=col,xlab='',ylab='')
  }
}
