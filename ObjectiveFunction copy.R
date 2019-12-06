
Obj.plot <- function(jobid=0, iGen=0,
                     inpath, outpath, 
                     CV, vlist,  outdir = CV$method$path_out){
  pngout = file.path(outdir, 'figure')
  rdsout = file.path(outdir, 'RDS')
  dir.create(pngout, recursive = T, showWarnings = F)
  dir.create(rdsout, recursive = T, showWarnings = F)
  
  fn.prefix = paste0('GW', '_Gen',iGen, '_Ch', jobid)
  fn=file.path(rdsout,  paste0(fn.prefix, '.RDS' ) )
  dat=readRDS(fn)
  nt=ncol(dat$sim)
  
  fn=file.path(pngout, fn.prefix )
  # cols=colorspace::heat_hcl(n=nt, c(0, 90), l=c(10, 90)  )
  cols=rainbow(nt)
  # png.control(fn=paste0(fn.prefix, '_plot.png'), wd=9, ht=5, path = pngout)
  # matplot(dat$sim[, 1], dat$sim[, -1], type='l', col=cols, lwd=2, ylim=c(0,2), xlab='X', ylab='Z'); grid()
  # points(dat$obs[,1], dat$obs[,2], col=cols[obs[,3]/2])
  # legend('topright', colnames(rv),lty=1:4, col=cols, lwd=2, pch=1)
  # dev.off()
  xdf=data.frame(dat$sim)
  md= reshape2::melt(xdf, id=c('x') )
  p <-ggplot(data=md, aes(x=x, y=value)) +
    xlab('X (m) ') + ylab('GWL')+ xlim(0, 3) + ylim(0, 2)+
    theme_gray()+ theme_linedraw()+
    geom_line(aes(color=variable) )  +
    scale_colour_manual(values=heat.colors(5) ) +
    geom_point(data=dat$obs, aes(x=x, y=z, shape=col) )  +
    labs(shape="Vauclin (1979)", colour="Simulations")
  # print(p)
  fn=file.path(pngout, fn.prefix )
  ggsave(p, filename=paste0(fn, '_plot.png'), width=10, height=5)
  
  xx=dat$vs
  # p= ggplot(data=data.frame(xx), aes(x=obs, y=sim))+geom_point()+
  #   geom_smooth(method = 'lm')
  # tmp=file.path(pngout, 'OS'); dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  # ggsave(p, filename=file.path(tmp, paste0(fn.prefix, '_OS.png')), width=10, height=5)
  
  obs=xx[,2]; sim=xx[,3]
  png.control(fn=paste0(fn.prefix, '_gof.png'), wd=8, ht=5, path=file.path(pngout, 'GOF') )
  hydroGOF::ggof(obs=obs, sim=sim)
  dev.off()
}
Obj.Func <- function(jobid=0, iGen=0,
                     inpath, outpath, 
                     CV, vlist,  outdir = CV$method$path_out){
  # library(ggplot2)
  inpath=as.character(inpath)
  outpath=as.character(outpath)
  tmp=PIHM(prjname = CV$prjname, outpath = outpath, inpath = inpath)
  pngout = file.path(outdir, 'figure')
  rdsout = file.path(outdir, 'RDS')
  dir.create(pngout, recursive = T, showWarnings = F)
  dir.create(rdsout, recursive = T, showWarnings = F)
  
  obs = as.matrix(read.csv('/Users/leleshu/Dropbox/PIHM/Benchmark/Vauclinv2/Ref/v2.csv'))
  xobs = data.frame(x=obs[,1], z=2-obs[,2], col=obs[,3])
  fun.interp <- function(xo, xs){
    grp = sort(unique(xo[,3]))
    ng=length(grp)
    ret = cbind(xo[, 1:2], 0)
    for(i in 1:ng){
      id=which(xo[,3] == grp[i])
      tmp=approx(xs[,1], xs[,1+i], xout = xo[id,1], method = 'linear')
      ret[id,3] = tmp$y
    }
    colnames(ret) = c('x', 'obs', 'sim')
    return(ret)
  }
  gw=readout('eleygw')
  tid=time(gw)[1] + (seq(2, 8, 2)+0)*3600
  if(max(time(gw)) < max(time(gw))){
    return(10)
  }
  
  xo=xobs
  xo$col=paste0('Hr0', xo$col)
  xo$col=as.factor(xo$col)
  xyz=getCentroid(); xlim=range(xyz[,1], na.rm = TRUE)
  x0 = seq(xlim[1], xlim[2],  length.out = 200)
  rv=NULL
  for(i in 1:length(tid)){
    tmp = approx(x=xyz[,1], y=gw[tid[i], ], xo=x0)
    rv=cbind(rv, tmp$y)
  }
  v.sim = data.frame(x0, rv); colnames(v.sim)=c('x', paste0('Hr0', 1:4 * 2))
  v.obs = xo
  # r = MeshData2Raster(gw[tid, ], stack = T)
  nt=length(tid)
  colnames(rv)= paste0('Hr', strftime(tid, '%H') )
  fn.prefix = paste0('GW', '_Gen',iGen, '_Ch', jobid)
  fn=file.path(rdsout,  paste0(fn.prefix, '.RDS' ) )
  xx=fun.interp(xo = v.obs, xs=v.sim)
  dat=list('sim'= data.frame(v.sim),  'obs' = data.frame(v.obs), 'vs'=data.frame(xx) )
  saveRDS(dat, fn)
  
  # xgof= hydroGOF::gof(xx[,2], xx[,3])
  # rr = xgof['NRMSE %',]
  rr=max(abs(xx$sim-xx$obs), na.rm=T)
  # message('GOF ', iGen, '-', jobid, ':', rr)
  return(rr)
}
# source('NewFunction.R')
# library(ggplot2)
# library(doParallel)
# library(PIHMgisR)
# pop=NULL; N=4
# for(i in 1:N){
#   pop = rbind(pop, rep(1, 48))
# }
# # undebug(pre.files)
# CV=readRDS('cv.RDS')
# vlist = pre.files(iGen = 0, pop = pop, CV=CV);
# inpath=file.path('input', CV$prjname)
# outpath=vlist$att$dir.out[1]
# registerDoParallel(cores=2)
# # foreach(i =1:2) %dopar% {
# for( i in 1:2){
#   objv <- Obj.Func(jobid=i, iGen=0, inpath=inpath, outpath=outpath, CV=CV)
#   Obj.plot(jobid=i, iGen=0, inpath=inpath, outpath=outpath, CV=CV)
# }
