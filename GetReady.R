library(raster)
library(sp)
library(rgeos)
library(rgdal)
library(SHUDtoolbox)
library(xts)
library(lattice)
library(ggplot2)
library(reshape2)
library(rasterVis)
library(doParallel)
library(foreach)
graphics.off()

workdir = '.'

# obs = as.matrix(read.csv('/Users/leleshu/Dropbox/PIHM/Benchmark/Vauclinv2/Ref/v2.csv'))
# saveRDS(obs, file = 'Vauclin1979.RDS')

prjname='vcl'
dir.gis = file.path(workdir,  'GISdata')
dir.create(dir.gis, showWarnings = F, recursive = T)
inpath = file.path(workdir, 'input', prjname)
outpath = file.path(workdir, 'output', paste0(prjname, '.out') )
dir.create(inpath, showWarnings = F, recursive = T)
dir.create(outpath, showWarnings = F, recursive = T)

prj=sp::CRS('+init=EPSG:5070')

q.min = 33;
# tol.riv=50
# tol.wb=50
# tol.len = 50
AqDepth = 2
years = 2000:2001
ny=length(years)
nday = 1

KSAT = 35 #cm/hr
KSAT = KSAT / 100 * 24
PRCP = 14.8 #cm/hr
PRCP = PRCP / 100 *24


nriv=1
lx = 3
lx1 = 0.5
# lx2 = 0.1
ly = .4
dx=min(ly/2, ly)
a.max = lx * ly / 250;

backup = FALSE
xy2sp <- function(x, crs){
  # s.str=paste('GEOMETRYCOLLECTION(', 
  #           paste(paste('LINESTRING((',
  #                       paste(x[,1], x[,2]), '))' )
  #                 , collapse =','),')' )
  s.str = paste("LINESTRING(",paste(paste(x[,1], x[,2]), collapse=','),')')
  s1=rgeos::readWKT(s.str)
  s2= sp::SpatialLinesDataFrame(s1, data=data.frame(1:length(s1)), match.ID = F)
  crs(s2) = crs
  return(s2)
}
# x=xy2sp(cbind(10,  0:10 / 10 * 10), crs=prj)
# plot(x, axes=T)
# points(coordinates(x))


RSQ <- function (x, y){
  cor(x, y) ^ 2
}
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}