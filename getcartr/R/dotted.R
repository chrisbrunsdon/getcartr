library(Rcartogram)

.grdim <- function(x) dim(x$x)

#.interpolate <- function(inpts,grid)  do.call(cbind,predict(grid,inpts[,1],inpts[,2]))

# mesh <- function(spdf) {
#   grid <- attr(spdf,'warp')
#   dims = .grdim(grid)
#   steps = min((dims-1)/32)
#   plot(grid,asp=1,type='n',xlim=c(-1,dims[1]+1),ylim=c(-1,dims[2]+1),xlab='',ylab='',axes=FALSE)
#   for (i in seq(1,dims[1],by=steps)) {
#     xin = cbind(rep(i,dims[2]),1:dims[2])
#     lines(.interpolate(xin,grid),col='red') }
#   for (i in seq(1,dims[2],by=steps)) {
#     xin = cbind(1:dims[1],rep(i,dims[1]))
#     lines(.interpolate(xin,grid),col='red') }  }

.covergrid <- function(spdf,extend = 0.05,res=256) {
	bb = bbox(spdf)
	rx = (bb[1,2] - bb[1,1])/2
	mx = (bb[1,2] + bb[1,1])/2
	ry = (bb[2,2] - bb[2,1])/2
	my = (bb[2,2] + bb[2,1])/2
	rb = max(rx,ry)*(1+extend)
	xmin = mx - rb
	xmax = mx + rb
	ymin = my - rb
	ymax = my + rb
	res = SpatialPoints(expand.grid(x=seq(xmin,xmax,l=res),y=seq(ymin,ymax,l=res)))
	proj4string(res) = CRS(proj4string(spdf))
	res}

.poly2grid <- function(spdf,var,extend=0.05,res=128,index=1,thresh=0) {
	spg = .covergrid(spdf,extend=extend,res=res)
	if (missing(var)) {
    lut = data.frame(spdf)[,index]
	}
  else {
    lut = var  
  }
	mapper = spg %over% as(spdf,"SpatialPolygons")
	res = mapper + NA
	unimap = table(mapper)
	for (item in as.numeric(names(unimap))) {
		res[mapper == item] = lut[item]/unimap[item] }
	res[is.na(res)] = sum(res[!is.na(res)])/sum(!is.na(res))
  plateau <- thresh * mean(res)
	res[res < plateau] <- plateau
	SpatialPixelsDataFrame(spg,data.frame(dens=res))}

 
 .poly2testgrid <- function(spdf,extend=0.05,res=256,index=1,thresh=0.1) { 
 rescale <- function(x) (x - min(x))/(max(x) - min(x)) 
 spg <- .covergrid(spdf,extend=extend,res=res) 
 SpatialPixelsDataFrame(coordinates(spg),data.frame(dens=rescale(coordinates(spg)[,2])))}
 

# cartogram.WarpGrid <- function(spg,blur=0) {
# 	densmat = spg$dens
# 	dim(densmat) = slot(getGridTopology(spg),'cells.dim')
#   densmat <- t(densmat)
# 	res = cartogram(densmat,blur=blur)
# 	attr(res,"cents") = slot(getGridTopology(spg),'cellcentre.offset')
# 	attr(res,"sizes") = slot(getGridTopology(spg),'cellsize') 
# 	res}

# warp.polys <- function(spdf,wg) {
# 	warpverb <- function(xy,wg) {
# 		cents = attr(wg,"cents")
# 		sizes = attr(wg,"sizes")
# 		xyt = t((t(xy)-cents)/sizes)
# 		.interpolate(xyt,wg)}
# 	.apply.polys(spdf,warpverb,wg) }
# 	
	
.apply.polys <- function (sp, action, ...) 
{
    IDs = sapply(slot(sp,'polygons'),function (x) slot(x,'ID'))
    Polys = slot(sp,'polygons')
    New.Polys = NULL
    if (class(sp) == "SpatialPolygonsDataFrame") {
        dat = slot(sp,'data')
    }
    for (i in 1:length(IDs)) {
        Poly.list = slot(Polys[[i]],'Polygons')
        New.list = NULL
        for (Poly in Poly.list) {
            acted = action(coordinates(Poly), ...)
            hl = slot(Poly,'hole')
            New.Poly = Polygon(acted, hole = hl)
            New.list = append(New.list, list(New.Poly))
        }
        New.Polys.item = Polygons(New.list, ID = IDs[i])
        New.Polys = append(New.Polys, list(New.Polys.item))
    }
    result = SpatialPolygons(New.Polys)
    if (class(sp) == "SpatialPolygonsDataFrame") {
        result = SpatialPolygonsDataFrame(result, dat, match.ID=FALSE)
    }
    result
}

.apply.lines <- function (sl, action, ...) 
{
    IDs = sapply(slot(sl,'lines'),function (x) slot(x,'ID'))
    Lines = slot(sl,'lines')
    New.Lines = NULL
    if (class(sl) == "SpatialLinesDataFrame") {
        dat = slot(sl,'data')
    }
    for (i in 1:length(IDs)) {
        Line.list = slot(Lines[[i]],'Lines')
        New.list = NULL
        for (Line in Line.list) {
            acted = action(coordinates(Line), ...)
            New.Line = Line(acted)
            New.list = append(New.list, list(New.Line))
        }
        New.Lines.item = Lines(New.list, ID = IDs[i])
        New.Lines = append(New.Lines, list(New.Lines.item))
    }
    result = SpatialLines(New.Lines)
    if (class(sl) == "SpatialLinesDataFrame") {
        result = SpatialLinesDataFrame(result, dat, match.ID=FALSE)
    }
    result
}

# warp.lines <- function(sldf,wg) {
# 	warpverb <- function(xy,wg) {
# 		cents = attr(wg,"cents")
# 		sizes = attr(wg,"sizes")
# 		xyt = t((t(xy)-cents)/sizes)
# 		.interpolate(xyt,wg)}
# 	.apply.lines(sldf,warpverb,wg) }
# 	
# warp.points <- function(pts,wg) {
# 	cents = attr(wg,"cents")
# 	sizes = attr(wg,"sizes")
# 	xyt = t((t(coordinates(pts)) - cents)/sizes)
# 	result = SpatialPoints(.interpolate(xyt,wg))
# 	if (class(pts) == "SpatialPointsDataFrame") {
# 		result = SpatialPointsDataFrame(result,slot(pts,'data'))}
# 	result}
# 
# quick.carto <- function(spdf,v) {
#   cart.pix <- .poly2grid(spdf,v)
#   cart.warp <- cartogram.WarpGrid(cart.pix)
#   res <- warp.polys(spdf,cart.warp)
#   attr(res,"warp") <- cart.warp
#   return(res)}


# .direct.warp.polys <- function(spdf,wg) {
#   warpverb <- function(xy,wg) {
#     cents = attr(wg,"cents")
#     sizes = attr(wg,"sizes")
#     xyt = t((t(xy)-cents)/sizes)
#     xr <- nrow(wg$x):0
#     yr <- ncol(wg$y):0
#     newx <- interp.surface(list(x=xr,y=yr,z=wg$y),xyt)
#     newy <- interp.surface(list(x=xr,y=yr,z=wg$x),xyt)
#     cbind(max(xr)-newx,max(yr)-newy)}
#   .apply.polys(spdf,warpverb,wg) }
   
.direct.warp.polys <- function(spdf,wg,prec) {
  warpverb <- function(xy,wg) {
    cents = attr(wg,"cents")
    sizes = attr(wg,"sizes")
    xyt = t((t(xy)-cents)/sizes)
    xyt <- .decimate(xyt,prec)
    .interpolate(xyt,wg)}
  .apply.polys(spdf,warpverb,wg) }


.cartogram.WarpGrid <- function(spg,blur=0) {
  densmat = spg$dens
  dim(densmat) = slot(getGridTopology(spg),'cells.dim')
  densmat <- t(densmat)
  res = cartogram(densmat,blur=blur)
  attr(res,"cents") = slot(getGridTopology(spg),'cellcentre.offset')
  attr(res,"sizes") = slot(getGridTopology(spg),'cellsize') 
  res}

.interpolate <- function(inpts,grid) {
  xr <- 0:nrow(grid$x)
  yr <- 0:ncol(grid$y)
  newx <- .interp.core(list(x=xr,y=yr,z=grid$x),inpts)
  newy <- .interp.core(list(x=xr,y=yr,z=grid$y),inpts)
  return(cbind(newx,newy))
}


.interp.core <- function (obj, loc) 
{
  x <- obj$x
  y <- obj$y
  z <- obj$z
  nx <- length(x)
  ny <- length(y)
  lx <- approx(x, 1:nx, loc[, 2])$y
  ly <- approx(y, 1:ny, loc[, 1])$y
  lx1 <- floor(lx)
  ly1 <- floor(ly)
  ex <- lx - lx1
  ey <- ly - ly1
  ex[lx1 == nx] <- 1
  ey[ly1 == ny] <- 1
  lx1[lx1 == nx] <- nx - 1
  ly1[ly1 == ny] <- ny - 1
  return(z[cbind(lx1, ly1)] * (1 - ex) * (1 - ey) + 
           z[cbind(lx1 + 1, ly1)] * ex * (1 - ey) + 
           z[cbind(lx1, ly1 + 1)] * (1 - ex) * ey +
           z[cbind(lx1 + 1, ly1 + 1)] * ex * ey)
}

.lengths <- function(x) sqrt((x[-1,1] - x[-nrow(x),1])^2 + (x[-1,2] - x[-nrow(x),2])^2)

.butlast <- function(x) x[-length(x)]

.find.prec <- function(x) {
  bb <- x@bbox
  min(bb[1,2] - bb[1,1],bb[2,2] - bb[2,1])/100
}

.chopper <- function(x) x[-length(x)] + (x[-1] - x[-length(x)])/2
.preciser <- function(x) .butlast(array(rbind(x,c(.chopper(x),0))))
.preciser2D <- function(x) cbind(.preciser(x[,1]),.preciser(x[,2]))

.decimate.core <- function(x,pr) {
  repeat {
    if (max(.lengths(x)) < pr) break
    x <- .preciser2D(x)
  }
  return(x)
}

.decimate <- function(x,pr) {
  nr <- nrow(x)
  result <- .decimate.core(x[1:2,],pr)
  if (nr>2)
    for (i in 3:nrow(x)) result <- rbind(result,.decimate.core(x[c(i-1,i),],pr)[-1,])
  return(result)
}


.linex <- function(a,b,c) {
  sprintf("LINESTRING(%f %f, %f %f)",a,b,a,c)
}

.liney <- function(a,b,c) {
  sprintf("LINESTRING(%f %f, %f %f)",b,a,c,a)
}

.merge.lines <- function(x) sprintf('GEOMETRYCOLLECTION(%s)',paste0(x,collapse=','))

# Base it on cents and sizes of warp grd
.graticule <- function(x) {
  cents <- attr(attr(x,'warp'),'cents')
  sizes <- attr(attr(x,'warp'),'sizes')
  dimn <- dim(attr(x,'warp')$x)
  xmin <- cents[1] + sizes[1]
  ymin <- cents[2] + sizes[1]
  xmax <- cents[1]+sizes[1]*(dimn[1] - 2)
  ymax <- cents[2]+sizes[2]*(dimn[2] - 2)
  xgrat <- seq(xmin,xmax,l=51) 
  ygrat <- seq(ymin,ymax,l=51) 
  xlines <- sapply(xgrat,function(x) .linex(x,ymin,ymax))
  ylines <- sapply(ygrat,function(x) .liney(x,xmin,xmax))
  readWKT(.merge.lines(c(xlines,ylines)))
}