library(raster)
library(sp)
library(rgeos)
library(rgdal)
library(xts)
library(lattice)
# library(rgl)
library(ggplot2)
library(rasterVis)
rm(list=ls())
source('GetReady.R')

# prjname='vcl'
# outpath=file.path('output', paste0(prjname,'.out') )
# inpath = file.path('input', prjname)
shud.env(prjname, inpath, outpath)
pngout = file.path(outpath, 'figure')
dir.create(pngout, recursive = T, showWarnings = F)


r=shud.mask(n = 600,proj = prj)

gw=readout('eleygw')
obs = as.matrix(read.csv('/Users/leleshu/Dropbox/PIHM/Benchmark/Vauclin/Ref/v1.csv'))
xo = data.frame(x=obs[,1]/100, z=2-obs[,2]/100, col=obs[,3])
xo$col=paste0('Hr0', xo$col)
xo$col=as.factor(xo$col)

tid=time(gw)[1] + (seq(2, 8, 2)+0)*3600
r = MeshData2Raster(gw[tid, ], stack = T) 
go<-function(tid){
  nt=length(tid)
  # cols=colorspace::heat_hcl(n=nt, c(0, 90), l=c(10, 90)  )
  cols=rainbow(nt)
  nr=dim(r)[1]; nc=dim(r)[2]
  rm=as.matrix(r)
  fx <- function(x, nr, nc){
    m=matrix(x, ncol=nr, nrow=nc)  
    y = apply(m, 1, mean)
  }
  rv=apply(rm, 2, fx, nr=nr, nc=nc)
  ext=extent(r)
  
  nt=length(tid)
  # cols=colorspace::heat_hcl(n=nt, c(0, 90), l=c(10, 90)  )
  cols=rainbow(nt)
  nr=dim(r)[1]; nc=dim(r)[2]
  rm=as.matrix(r)
  fx <- function(x, nr, nc){
    m=matrix(x, ncol=nr, nrow=nc)  
    y = apply(m, 1, mean)
  }
  
  rv=apply(rm, 2, fx, nr=nr, nc=nc)
  colnames(rv)= paste0('Hr', strftime(tid, '%H') )
  
  x=seq(0, 3.1, length.out = nrow(rv))
  xdf=data.frame(x=x, rv)
  # matplot(xx, rv, type='l', col=cols, ylim=c(0.6, 1.3), xlim=c(0, 3), lwd=3, asp=1)
  md= reshape2::melt(xdf, id=c('x') )
  p <-ggplot(data=md, aes(x=x, y=value)) + xlab('X (m) ') + ylab('GWL')+ xlim(0, 3) + ylim(0, 2)+
    theme_gray()+ theme_linedraw()+
    geom_line(aes(color=variable) )  +
    scale_colour_manual(values=heat.colors(5) ) +
    geom_point(data=xo, aes(x=x, y=z, shape=col) )  +
    labs(shape="Vauclin", colour="Simulations")
  # print(p)  
  fn=file.path(pngout, paste0('GW', paste(cfg.calib, collapse= '_') ) )
  message('\t', fn)
  ggsave(p, filename=paste0(fn, '.png'), width=10, height=5)
  r
}; 
cfg.calib=readcalib()
r=go(tid)
