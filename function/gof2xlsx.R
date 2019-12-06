#install.packages('WriteXLS')
library(WriteXLS)
pj = 'sac'
np = 1:11

gofdir = paste0('GOF_',pj)
dir.create(gofdir, showWarnings = F, recursive = T)

xl = list()
prj = paste0(pj, np)
for(i in np){
  prjname = prj[i]
  cdir = file.path('output', paste0(prjname, '.out')) 
  message(i, '/', max(np), '\t', prjname)
  fn.gof = file.path(cdir, paste0(prjname, '.gof.csv') )
  if(file.exists(fn.gof)){
    x=as.data.frame(read.table(fn.gof), header=TRUE, sep = '\t') 
    ord=order(x[,11], decreasing = TRUE)
    y=x[ord, ]
    xl[[i]]=y
    
    pngfiles = file.path(cdir, 'png.out',
                         paste0(prjname, '_', 'Step', y[,1], '_Job', y[,2], '.png') )
    if(length(pngfiles)>3){
      file.copy(pngfiles[1:3], to = gofdir)
    }else{
      file.copy(pngfiles, to = gofdir)
    }
  }else{
    xl[[i]] = data.frame(1)
  }
}
names(xl) = prj
WriteXLS(xl, file.path(gofdir, paste0(pj, '.gof.xls') ) )
