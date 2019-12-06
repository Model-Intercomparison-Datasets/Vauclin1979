
rm(list=ls())
source('GetReady.R')
fin <- shud.filein(prjname, inpath = inpath)
pngout = file.path(inpath, 'fig')
gisout = file.path(inpath, 'gis')

dir.create(pngout, showWarnings = F, recursive = T)
dir.create(gisout, showWarnings = F, recursive = T)

wbd=readOGR(file.path(dir.gis,  paste0(prjname,'_wbd.shp') ) )
riv=readOGR(file.path(dir.gis,  paste0(prjname,'_riv.shp') ) )
dem=raster(file.path(dir.gis,  paste0(prjname,'_dem.tif') ) )
r.forc = raster(file.path(dir.gis,  paste0(prjname,'_forc.tif') ) )
forc.fns=file.path(inpath, paste0('forc', 1:2, '.csv') )
r0=dem * 0 +1

ext=extent(wbd)
ext
dl = ly /3
ys = seq(0, ly, dl)
# pts = rbind(cbind(lx1, ys), cbind(lx, ys))


png(file = file.path(pngout, 'data_0.png'), height=11, width=11, res=100, unit='in')
plot(dem); plot(wbd, add=T, border=2, lwd=2); 
plot(riv, add=T, lwd=2, col=4)
dev.off()



tri = shud.triangle(wb=wbd, pts = cbind(0.5, ys),  q=q.min, a=a.max)
 
# generate  .mesh 
pm=shud.mesh(tri,dem=dem, AqDepth = AqDepth)
spm = sp.mesh2Shape(pm, crs = crs(riv))
writeshape(spm, crs(wbd), file=file.path(gisout, 'domain'))

# generate  .att
pa=shud.att(tri, r.forc = r.forc)

write.forc(basename(forc.fns), path=inpath,
          file=fin['md.forc'], backup = backup)

# generate  .riv
pr=shud.river(riv, dem)
pr@river$BC=1
pr@rivertype$Depth=2
pr@rivertype$KsatH = KSAT
pr@rivertype$BedThick = 0.05
pr@rivertype$Sinuosity = 1
pr

spr = riv
writeshape(spr, crs(wbd), file=file.path(gisout, 'river'))

# Cut the rivers with triangles
# sp.seg = sp.RiverSeg(pm, pr)
sp.seg=sp.RiverSeg(spm, spr)
writeshape(sp.seg, crs(wbd), file=file.path(gisout, 'seg'))

# Generate the River segments table
prs = shud.rivseg(sp.seg)

# Generate initial condition
pic = shud.ic(nrow(pm@mesh), nrow(pr@river), AqD = 2, stage = 0, p1 = 0, p2 = 0.65/2)

# Generate shapefile of river
# spr = sp.riv2shp(pr); 

# Generate shapefile of mesh domain
sp.dm = sp.mesh2Shape(pm)
png(file = file.path(pngout, 'data_2.png'), height=11, width=11, res=100, unit='in')
zz = sp.dm@data[,'Zsurf']
ord=order(zz)
col=terrain.colors(length(sp.dm))
plot(sp.dm[ord, ], col = col)
plot(spr, add=T, lwd=3)
dev.off()

# model configuration, parameter
cfg.para = shud.para(nday = nday)
cfg.para
# calibration
cfg.calib = shud.calib()
#soil/geol/landcover
lc = 42
para.lc = PTF.lc(lc=lc)
para.soil = PTF.soil()
para.geol = PTF.geol()

para.lc[,'IMPAF']=1
id=which(grepl('dt_', names(cfg.para) ))
# stop()
# 43-mixed forest in NLCD classification
# 23-developed, medium           
# 81-crop land
# 11-water
lr=fun.lairl(lc, years=years)
png(file = file.path(pngout, 'data_lairl.png'), height=11, width=11, res=100, unit='in')
par(mfrow=c(2,1))
col=1:length(lc)
plot(lr$LAI, col=col, main='LAI'); legend('top', paste0(lc), col=col, lwd=1)
plot(lr$RL, col=col, main='Roughness Length'); legend('top', paste0(lc), col=col, lwd=1)
dev.off()
write.tsd(lr$LAI, file = fin['md.lai'], backup = backup)
write.tsd(lr$RL, file = fin['md.rl'], backup = backup)

#MeltFactor
mf = MeltFactor(years = years)
write.tsd(mf, file=fin['md.mf'], backup = backup)

# write input files.
write.mesh(pm, file = fin['md.mesh'], backup = backup)
write.riv(pr, file=fin['md.riv'], backup = backup)
write.ic(pic, file=fin['md.ic'], backup = backup)

write.df(pa, file=fin['md.att'], backup = backup)
write.df(prs, file=fin['md.rivseg'], backup = backup)
write.df(para.lc, file=fin['md.lc'], backup = backup)
write.df(para.soil, file=fin['md.soil'], backup = backup)
write.df(para.geol, file=fin['md.geol'], backup = backup)

write.config(cfg.para, fin['md.para'], backup = backup)
write.config(cfg.calib, fin['md.calib'], backup = backup)
print(nrow(pm@mesh))

source('2_parameters.R')
print(nrow(pm@mesh))
plot(spm)
