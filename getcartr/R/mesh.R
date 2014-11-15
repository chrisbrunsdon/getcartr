#' Visual representation of cartogram transformation
#'
#' Given a transformed Spatial object draws the warping grid 
#'
#' 
#' @export
#' @param spdf  A \code{SpatialPolygonsDataFrame}  created via the \code{quick.carto} function
#' @references 
#'     A Diffusion-based method for producing density equalizing maps, Michael T. Gastner and M. E. J. Newman, Proc. Natl. Acad. Sci. USA 101, 7499-7504 (2004)
#' @keywords cartogram
#' @seealso \link{quick.carto}
#' @examples
#' # Load an example SpatialPolygonsDataFrame from GISTools
#' data(georgia)
#' # Create the cartogram.  TotPop90 contains 1990 county populations
#' # "georgia2" is used as it has a plane projection coordinate system
#' georgia.carto <- quick.carto(georgia2,georgia2$TotPop90)
#' # Draw the mesh
#' mesh(georgia.carto)



mesh <- function(spdf) {
  grid <- attr(spdf,'warp')
  if (is.null(grid)) stop("Object not created via 'quick.carto'")
  dims = .grdim(grid)
  steps = min((dims-2)/32)
  plot(grid,asp=1,type='n',xlim=c(-1,dims[1]+1),ylim=c(-1,dims[2]+1),xlab='',ylab='',axes=FALSE)
  for (i in seq(2,dims[1],by=steps)) {
    xin = cbind(rep(i,dims[2]-1),2:(dims[2]))
    lines(.interpolate(xin,grid),col='red') }
  for (i in seq(2,dims[2],by=steps)) {
    xin = cbind(2:(dims[1]),rep(i,dims[1]-1))
    lines(.interpolate(xin,grid),col='red') }  }
