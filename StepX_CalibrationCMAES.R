clib=c('adagio','hydroGOF', 'doParallel', 'parallel', 'xts', 'SHUDtoolbox')
library(ggplot2)
x=lapply(clib, library, character.only=TRUE)
rm(list=ls())
# source('GetReady.R')
prjname='vcl'
odir='.'
inpath = file.path(odir, 'input', prjname)
outpath = file.path(odir, 'output', paste0(prjname, '.out') )
dir.create(inpath, showWarnings = F, recursive = T)
dir.create(outpath, showWarnings = F, recursive = T)

shud.env(prjname, inpath, outpath)
# source('NewFunction.R')
source('ObjectiveFunction.R')
fn.cmaes = file.path('.', paste0(prjname, '.calib.cmaes'))
# undebug(write.cmaes)
# write.cmaes(file= fn.cmaes, backup = TRUE)
x.cmaes = readconfig(file = fn.cmaes)

fn.calib=shud.filein()['md.calib']
calib=readcalib(file=fn.calib)

fn.range=file.path('.', paste0(prjname, '.calib.range'))
# x=rbind(calib*0, 0, 0, 0); x
# write.config(x, file=fn.range)
calset = readconfig(file=fn.range)
calset
rownames(calset) = c('onoff', 'log', 'min', 'max')


para=readconfig(file = shud.filein()['md.para'] )
t0 = para['START'];
t1 = t0 + x.cmaes$nspinup; 
t2 = para['END']
# xdf = cbind('skip'=qo[1:t0, ], 'spinup'=qo[t0:t1,], 'calib'=qo[t1:t2,])
# debug(mycmaes)
unlink(as.character( x.cmaes$PATH_OUT ), recursive = FALSE)

CV = list('prjname' =prjname,
          'calib'=calib,
          'range'=calset,
          'method' = type.convert(x.cmaes)
          # 'obs' = qo,
          # 'extname' = extname,
          # 'colIndex' = colIndex,
          # 'inpath' = file.path('input', prjname),
          # 'outpath' = file.path('output', paste0(prjname, '.out') ),
          # 'gofname' = 'NSE', 
          # 'walltime' = walltime,
          # 'updateinit'=updateinit,
          # 'fn.gof' = file.path('output', paste0(prjname, '.out'), paste0(prjname, '.gof.csv') )
)
saveRDS(CV, 'cv.RDS')
dir.create(as.character(CV$method$PATH_OUT), showWarnings = FALSE, recursive = TRUE)

file.copy(from='/Users/leleshu/Dropbox/SHUD/github/SHUD/Build/Products/Debug/shud', 
          to='./shud', overwrite=T)

# debug(CMAES)
# debug(Call_Model)
# undebug(EXEC)
sol1 <- CMAES(CV = CV, cmd ='./shud', Call_Model = Call_Model,  
              objfunc = Obj.Func, debug = FALSE)


