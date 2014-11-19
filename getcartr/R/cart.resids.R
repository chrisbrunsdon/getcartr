#' Create a cartogram SpatialPolygonsDataFrame
#'
#' Given a \code{SpatialPolygonsDataFrame} as a cartogram
#' and the cartogram variable,  computes the residual between the 
#' desired area and the actual area for each zone in cartogram area units
#' 
#' @export
#' @param cart The cartogram \code{SpatialPolygonsDataFrame}
#' @param var The cartogram variable
#' @return The residuals (in cartogram area units) for each zone
#' @keywords cartogram
#' @examples
#' data(georgia)
#' georgia.carto <- quick.carto(georgia2,georgia2$TotPop90)
#' discrep <- cart.resids(georgia.carto,georgia2$TotPop90)
#' shs <- auto.shading(c(discrep,-discrep),n=5,col=brewer.pal(5,'RdBu'))
#' choropleth(georgia.carto,discrep,shading=shs)
#' choro.legend(105,105,shs)






cart.resids <- function(cart,var) {
  areas <- poly.areas(cart)
  return(lm.fit(matrix(areas,length(areas),1),var)$residuals)
}

