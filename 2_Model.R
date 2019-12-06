source('GetReady.R')
outpath=file.path('output', paste0(prjname,'.out') )
inpath = file.path('input', prjname)
shud.env(prjname, inpath, outpath)

go.calib <- function(){
  cfg.calib = shud.calib()
  cfg.calib$TS_PRCP = 1
  names(cfg.calib)
  # [1] "GEOL_KSATH"    "GEOL_KSATV"    "GEOL_KMACSATH" "GEOL_MACVF"    "GEOL_THETAS"  
  # [6] "GEOL_THETAR"   "GEOL_DMAC"     "SOIL_KINF"     "SOIL_KMACSATV" "SOIL_DINF"    
  # [11] "SOIL_ALPHA"    "SOIL_BETA"     "SOIL_MACHF"    "LC_VEGFRAC"    "LC_ALBEDO"    
  # [16] "LC_ROUGH"      "LC_DROOT"      "LC_ISMAX"      "LC_IMPAF"      "LC_SOILDGD"   
  # [21] "TS_PRCP"       "TS_LAI"        "TS_SFCTMP+"    "ET_ETP"        "ET_IC"        
  # [26] "ET_TR"         "ET_SOIL"       "RIV_ROUGH"     "RIV_KH"        "RIV_SINU"     
  # [31] "RIV_CWR"       "RIV_BEDTHICK"  "RIV_BSLOPE+"   "RIV_DPTH+"     "RIV_WDTH+"    
  # [36] "IC_GW+"        "IC_RIV+"       "AQ_DEPTH+"   
  cfg.calib['ET_ETP']=0
  cfg.calib['ET_TR']=0
  cfg.calib['ET_IC']=0
  cfg.calib['ET_SOIL']=0
  # cfg.calib['KSATH']=0
  # cfg.calib['KSATV']=0
  # cfg.calib['KINF']=0
  # cfg.calib['KMACSATH']=000
  # cfg.calib['KMACSATV']=0
  # cfg.calib['RIV_KH']=0
  write.config(cfg.calib, file=shud.filein()['md.calib'], backup = F )
}
go.para<- function(){
  cfg.para=shud.para()
  print(cfg.para)
  dt.model = 1
  dt.out = max(dt.model, 1)
  cfg.para['MAX_SOLVER_STEP'] = dt.model
  cfg.para['END'] = 1
  id=which(grepl('DT_', names(cfg.para)))
  cfg.para[id]=dt.out;
  write.config(cfg.para, file=shud.filein()['md.para'], backup = F )
}
go.init<-function(){
  x=readic()
  x$minit
  x$rinit = x$rinit *0
  write.ic(x, file = shud.filein()['md.ic'], backup=FALSE)
}
go.calib()
go.para()
go.init()

file.copy(from='/Users/leleshu/Dropbox/SHUD/github/SHUD/Build/Products/Debug/shud', 
          to='./shud', overwrite=T)
system(paste('./shud ', prjname), wait = TRUE, ignore.stdout = F)
