id = c(1:3, 5:6,  8:11)

timewall = 1440 * 10
ncores=48
cdir = 'submission'
dir.create(cdir, showWarnings = FALSE, recursive = TRUE)
fn.r = file.path(cdir, paste0('s', id, '.R'))
fn.sh = file.path(cdir, paste0('s', id, '.sh'))
tmp.r = readLines('function/template.R')
# tmp.sh = readLines('input/template.sh')
nid = length(id)
for(i in 1:nid){
  ik = id[i]
  message(i, '/', nid, '\t', ik)
  rs = c('rm(list=ls())',paste('sid =', ik), tmp.r )
  write(rs, fn.r[i])
  
  sh = c('#!/bin/bash -l' ,
         paste0('#SBATCH -J s',ik,'calib'),
         # paste0('#SBATCH -o s',i,'calib%j.output'),
         # paste0('#SBATCH -e s',i,'calib%j.output'),
         paste0('R CMD BATCH ', fn.r[i]) )
  write(sh, fn.sh[i])
}
cmd = paste('sbatch', 
            '-t',timewall, 
            '-n', ncores, 
            fn.sh)
write(cmd, file = 'sub.sh')