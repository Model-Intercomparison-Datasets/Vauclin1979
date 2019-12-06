pre.files <- function(istep, pop, CV){
  prjname=CV[['prjname']]
  ext = CV[['extname']]
  calib=CV[['calib']]
  calibrange = CV[['range']]
  inpath =CV[['inpath']]
  outpath =CV[['outpath']]

  njob =ncol(pop)
  pngpath = file.path(outpath, 'png.out')
  dir.create(outpath, showWarnings = FALSE, recursive = TRUE)
  dir.create(pngpath, showWarnings = FALSE, recursive = TRUE)

  fn.calib =file.path(inpath, paste0('calib', 1:njob) )
  fn.log = file.path(outpath, paste0(prjname, '_', 1:njob),
                     paste0('calib', 1:njob, '_',istep,'.log'))
  fn.sim = file.path(outpath, paste0(prjname, '_', 1:njob),
                     paste0(prjname, '.', ext, '.dat') )
  subdir = file.path(outpath, paste0(prjname, '_', 1:njob) )
  fn.png = file.path(pngpath, paste0(prjname, '_Step', istep, '_Job',1:njob,'.png') )

  dir.create(dirname(pngpath), showWarnings = FALSE, recursive = TRUE)
  dir.create(dirname(outpath), showWarnings = FALSE, recursive = TRUE)

  calibmat = apply(pop, 2, function(x) x2calib(x, calib, calibrange))
  print(calibmat)
  fn.mat = file.path(outpath, 'CalibStep.csv')
  write.table(calibmat, file=fn.mat, quote = FALSE, append = FALSE, row.names = TRUE, col.names = FALSE, sep = '\t')

  for(i in 1:njob){
    icb = calibmat[,i]
    write.pc(icb, file = fn.calib[i])
  }

  fn.pop = file.path(outpath, 'DataPop.csv')
  fn.cal = file.path(outpath, 'DataCalib.csv')
  x1 = cbind('step'=istep, t(pop))
  x2 = cbind('step'=istep, t(calibmat))
  if(istep <= 1){
    write.table(x1, file=fn.pop, append = FALSE, col.names = TRUE, row.names = FALSE, quote=FALSE, sep = '\t')
    write.table(x2, file=fn.cal, append = FALSE, col.names = TRUE, row.names = FALSE, quote=FALSE, sep = '\t')
  }else{
    write.table(x1, file=fn.pop, append = TRUE, col.names = FALSE, row.names = FALSE, quote=FALSE, sep = '\t')
    write.table(x2, file=fn.cal, append = TRUE, col.names = FALSE, row.names = FALSE, quote=FALSE, sep = '\t')
  }
  ret <- list('fn.calib' = fn.calib,
               'fn.log'= fn.log,
               'fn.sim' = fn.sim,
               'fn.png' = fn.png,
               'subdir' = subdir,
               'fn.cmat' = fn.mat,
               'calibmat' = calibmat)
}
QvsPrep <- function(p, q, area){
  tmp = readforc.csv('input/sac10/sac10.tsd.forc', id=1)[[1]]
  pm=readmesh(file='input/sac10/sac10.sp.mesh')
  ia=getArea(pm)
  area=sum(ia)
  px = as.xts(tmp[,1])
  qx = qo
  pd = apply.daily(px, mean)
  time(pd) = as.Date(time(pd))
  
  qd = apply.daily(qx, mean)
  time(qd) = as.Date(time(qd))
  
  tp = time(pd)
  tq = time(qd)
  ct= tp[tp %in% tq]
  pq = cbind(pd[ct], qd[ct]/area)
  
  xyr = apply.yearly(pq, mean)
  Qr = xyr[,2] / xyr[,1]
  plot(Qr)
  xp=ad.numeric(pd)
  
}
