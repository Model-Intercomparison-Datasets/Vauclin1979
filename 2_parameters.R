source('GetReady.R')
go.calib <- function(){
  cfg.calib = shud.calib(); cfg.calib
  cfg.calib['LC_ROUGH'] = 1e10
  cfg.calib['TS_PRCP']=1
  cfg.calib['ET_IC']=0
  cfg.calib['ET_TR']=0
  cfg.calib['ET_SOIL']=0
  
  cfg.calib['SOIL_ALPHA'] = 1
  cfg.calib['SOIL_BETA']=1
  cfg.calib['GEOL_THETAR']=1
  
  cfg.calib['GEOL_MACVF']=0
  cfg.calib['SOIL_MACHF']=0
  
  cfg.calib['GEOL_KSATH']=1
  cfg.calib['GEOL_KSATV']=1
  cfg.calib['SOIL_KINF']=1
  cfg.calib['GEOL_KMACSATH']=0
  cfg.calib['SOIL_KMACSATV']=0
  cfg.calib['RIV_KH']=1
  write.config(cfg.calib, file=shud.filein()['md.calib'], backup = F )
  cfg.calib
}
go.para<- function(dt.model =1){
  # debug(readpara)
  cfg.para=shud.para()
  cfg.para
  dt.out = max(dt.model, 1)
  cfg.para['MAX_SOLVER_STEP'] = dt.model
  cfg.para['END'] = 0.4
  id=which(grepl('DT_', names(cfg.para))) 
  cfg.para[id]=0;
  cfg.para['DT_YE_GW']=dt.out
  cfg.para['DT_YE_SURF']=dt.out
  cfg.para['RELTOL']=1e-4
  cfg.para['ABSTOL']=1e-4
  cfg.para['INIT_SOLVER_STEP']=1
  cfg.para['MAX_SOLVER_STEP']=dt.model
  cfg.para['INIT_MODE']=3
  print(cfg.para)
  write.config(cfg.para, file=shud.filein()['md.para'], backup = F )
  cfg.para
}
go.init<-function(){
  x=readic()
  x$minit[, 5] =  0
  x$minit[, 6] = 0.65
  x$rinit = 0
  write.ic(x, file = shud.filein()['md.ic'], backup=FALSE)
  x
}
go.geol <- function(KSAT){
  x=readgeol(); x
  x[, 2] = 35/100*24  # 35 cm / hr  ; Vauclin
  x[, 3] = x[, 2]
  x[, 4] = 0.3 # theta_s Shen2010
  x[, 5] = 0.01 # theta_r Shen2010
  x[, 6:8] = 0 #vAreaF.m2_m2. macKsatH.m_d. Dmac.m.
  write.df(x,  shud.filein()['md.geol'], backup = F)
  x
}
go.soil <- function(KSAT){
  x=readsoil(); x;dim(x)
  x[, 2] = 35/100*24  # 35 cm / hr ; Vauclin
  x[, 3] = 0.3 # theta_s Shen2010
  x[, 4] = 0.01 # theta_r Shen2010
  x[, 6] = 3.3 # alpha Shen2010
  x[, 7] = 4.1 # beta Shen2010
  x[, 8:9] = 0 # hAreaF.m2_m2, macKsatV.m_d
  write.df(x,  shud.filein()['md.soil'], backup = F)
  x
}
go.riv<-function(KSAT){
  pr=readriv()
  pr@river$BC=1
  pr@rivertype$Depth=2
  pr@rivertype$KsatH = KSAT
  pr@rivertype$BedThick = 0.01
  pr@rivertype$Sinuosity = 1
  write.riv(pr, shud.filein()['md.riv'], backup = F)
  pr
}
go.mesh <- function(){
  pm=readmesh()
  pm@point$AqDepth=2
  write.mesh(pm, shud.filein()['md.mesh'], backup = F)
  pm
}
shud.env(prjname = prjname, inpath =file.path(workdir, 'input', prjname),
               outpath = file.path(workdir, 'output', paste0(prjname, '.out')))
xc=go.calib()
xp=go.para()
xi=go.init()
xs=go.soil(KSAT)
xg=go.geol(KSAT)
xr=go.riv(KSAT)

