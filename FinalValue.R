clib=c('adagio','hydroGOF', 'doParallel', 'parallel', 'xts')
library(ggplot2)
x=lapply(clib, library, character.only=TRUE)
library(PIHMgisR)
rm(list=ls())
prjname='vcl'
odir='.'
inpath = file.path(odir, 'input', prjname)
outpath = file.path(odir, 'output', paste0(prjname, '.out') )
dir.create(inpath, showWarnings = F, recursive = T)
dir.create(outpath, showWarnings = F, recursive = T)

PIHM(prjname, inpath, outpath)

x = readcalib(file = 'cmaes_out/calib_Gen.9.calib')
xg=readgeol()
xs=readsoil()

ct = 1/24 * 100
y=cbind(
xg$KsatH.m_d. * x$GEOL_KSATH * ct,  
xg$KsatV.m_d. * x$GEOL_KSATV * ct, 
xg$ThetaS.m3_m3. * x$GEOL_THETAS,
xg$ThetaR.m3_m3. * x$GEOL_THETAR,
xs$KsatV.m_d. * x$SOIL_KINF * ct,
xs$Alpha.1_m. * x$SOIL_ALPHA,
xs$Beta * x$SOIL_BETA
)[1,]
z=(c(x$GEOL_KSATH, x$GEOL_KSATV, x$GEOL_THETAS, x$GEOL_THETAR, 
    x$SOIL_KINF, x$SOIL_ALPHA, x$SOIL_BETA
  )-1 ) * 100
names(y)=c('ksath', 'ksatv', 'thetaS', 'thetaR', 
              'Kinf', 'alpha', 'beta')
names(z)=c('ksath', 'ksatv', 'thetaS', 'thetaR', 
              'Kinf', 'alpha', 'beta')

cbind('Value'=y,'Change %'=z)

