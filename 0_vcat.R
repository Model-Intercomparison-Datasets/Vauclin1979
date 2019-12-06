source('GetReady.R')
ndx=2
r=raster(res=dx, 
         xmn=0 - dx*ndx,
         xmx=lx +dx*ndx, 
         ymn=0 - dx*ndx, 
         ymx=ly + dx*ndx,
         crs=prj)
r[]= 2;
plot(r)
writeRaster(r, file.path(dir.gis, paste0(prjname, '_dem.tif')), overwrite=TRUE)
#========
rx=c(lx, lx); ry=c(ly, 0)  # rivers
r.sl = xy2sp(cbind(rx, ry), crs=prj)
# plot(r.sl, axes=T); plot(add=T, r); plot(r.sl, add=T, lwd=4, col=1:nriv); grid()
# plot(r);plot(r.sl, add=T, lwd=2, col=1:nriv)
writeshape(r.sl, file=file.path(dir.gis, paste0(prjname,'_riv')) )

#============
bc1=fishnet(c(0,lx1, 0,ly), crs=prj, dx=lx1, dy = ly)
# bc2=fishnet(c(lx,lx+lx2, 0,ly), crs=prj, dx=lx2, dy = ly)
bc1.l=xy2sp(cbind(lx1, 0:10 / 10 * ly), crs=prj)
# bc2.l=xy2sp(cbind(lx,  0:10 / 10 * ly), crs=prj)
  # undebug(fishnet)
wbd=fishnet(c(0,lx, 0,ly), crs=prj, dx=lx, dy = ly)
plot(wbd, axes=T); grid()
plot(add=T, r)
plot(wbd, add=T)
plot(bc1, add=T, border=2)
# plot(bc2, add=T, border=3)
writeshape(wbd, file=file.path(dir.gis, paste0(prjname,'_wbd')) )
writeshape(bc1, file=file.path(dir.gis, paste0(prjname,'_bc1')) )
writeshape(bc1.l, file=file.path(dir.gis, paste0(prjname,'_bcl1')) )

#========FORC RASTER ======
r1 =rasterize(bc1, r) * 0+1
r2=r*0+2
r.forc = cover(r1, r2)
# plot(r.forc)
writeRaster(r.forc, filename = file.path(dir.gis, paste0(prjname, '_forc.tif')), overwrite=TRUE)

#========BC RASTER ======
# r1 =rasterize(bc2, r) * 0+1
# r2=r*0
# r.bc = cover(r1, r2)
# writeRaster(r.bc, filename = file.path(dir.gis, paste0(prjname, '_BC.tif')), overwrite=TRUE)

#=======Forcing TSD ====
nt.per.dat= 1440
# nt=nday=1440
nt=nday *2 * nt.per.dat

xt=1:nt - 1
# P=abs(sin(xt/100)) / 100
# P[P < 0.008]=0
P=xt*0
P[] = PRCP

go<-function(xdf, file){
  tsd= as.xts(xdf[, -1], order.by=as.POSIXct('2001-01-01') + xdf[, 1] * (86400/nt.per.dat))
  dir.create(dirname(file), showWarnings = F, recursive = T)
  write.tsd(tsd, file=file, backup = F)
  # plot.zoo(tsd[,1], ylab='Precipitaion', xlab='Time')
}
tsd.fc1=cbind(day = xt, Prcp=P, Temp=25, RH=.5, Wind=1000, Rad=1e5) 
tsd.fc2=cbind(day = xt, Prcp=P*0, Temp=25, RH=.5, Wind=1000, Rad=1e5) 
go(tsd.fc1, file=file.path(inpath, 'forc1.csv'))
go(tsd.fc2, file=file.path(inpath, 'forc2.csv'))

#======Boundary Condition TSD =====
tsd.bc = data.frame(day=xt, 'BC1' = 0.65)
go(tsd.bc, file=file.path(inpath, paste0(prjname, '.tsd.rbc1')) )
# go(tsd.bc, file=file.path(inpath, paste0(prjname, '.tsd.ebc1')) )



