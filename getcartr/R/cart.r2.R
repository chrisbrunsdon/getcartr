#' Create a cartogram SpatialPolygonsDataFrame
#'
#' Given a \code{SpatialPolygonsDataFrame} as a cartogram
#' and the cartogram variable,  computes the R-squared obtained from regressing
#' cartogram zone areas on the cartogram variable,  without an intercept
#'
#' @export
#' @param cart The cartogram \code{SpatialPolygonsDataFrame}
#' @param var The cartogram variable
#' @return The R-squared value
#' @keywords cartogram
#' @examples
#' data(georgia)
#' georgia.carto <- quick.carto(georgia2,georgia2$TotPop90)
#' cart.r2(georgia.carto,georgia2$TotPop90)
#' 
#' # Investigate performance as resolution changes
#' perf <- function(r) cart.r2(quick.carto(georgia2,res=64*r,georgia2$TotPop90),
#'      georgia2$TotPop90)
#' score <- rep(0,8)
#' for (i in 1:8) score[i] <- perf(i)
#' plot(64*(1:8),score,xlab='Resolution',ylab='Cartogram Correlation',type='b')
#' 
#' # Suppose we start with an inappropriate map projection?
#' poor.carto <- quick.carto(georgia,georgia$TotPop90)
#' georgia.carto <- quick.carto(georgia2,georgia2$TotPop90)
#' cart.r2(poor.carto,georgia$TotPop90)
#' cart.r2(georgia.carto,georgia2$TotPop90)


cart.r2 <- function(cart,var) {
  areas <- poly.areas(cart)
  mdl <- lm.fit(matrix(areas,length(areas),1),var)
  f <- mdl$fitted.values
  r <- mdl$residuals
  return(sum(f^2)/(sum(f^2)+sum(r^2)))
}

