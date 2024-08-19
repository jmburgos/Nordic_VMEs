
for(VME_i in unique(vme$vme)){#unique(vme$vme)){
  cat(paste0(VME_i, "\n"))
  vme_p<-vme[vme$vme == VME_i, ] # Presences
  
  # VMEs
  ## Terrain and CNRM model in vme locations
  ve1 <-  terra::extract(env1, vmec$cell) %>%
    mutate(vme = vmec$vme,
           occ = 1
    ) %>%
    relocate(vme, occ) %>%
    write_rds("C:/Users/a39495/Documents/Projects/NEMO/data/vme_terrain_cnrm_present.rds")
  
  PredVars<-c('depth', 'planc3', 'profc3', 'slope21', 'slope3', 'tpi21', 'tpi3', 'vrm21', 'vrm3', 'salt_scen', 'SDsalt_scen', 'temper_scen', 'SDtemper_scen', 'c_spd_scen', 'SDc_spd_scen')
  
  env_df<-rbind(ve1[ve1$vme == VME_i,-1], be1)
  
  
  cat("maxent on NEMO start---")
  mod <- maxent(x=env_df[,PredVars], ## env conditions
                p=env_df$occ,
                #a=ve1,## 1:presence or 0:absence
                path=paste0("../output/maxent_outputs"), ## folder for maxent output;
                # if we do not specify a folder R will put the results in a temp file,
                # and it gets messy to read those. . .
                args=c("responsecurves") ## parameter specification
  )
  cat("finish---predicting...")

  #### predict
  #Map current distribution of species
  tic()
  NEMO_pred_current <- predict(mod, stack(env1[[PredVars]]))
  toc()
  writeRaster(NEMO_pred_current, paste0(VME_i,'_NEMO_pred_current.tif'))
  cat("predictions saved\n")
  
  #Maps under future climates based on NEMO 
  
  ### Pending...
  
  
  
  ## MissionAtlantic environment

  # CNRM
  PredVars<-c('depth', 'planc3', 'profc3', 'slope21', 'slope3', 'tpi21', 'tpi3', 'vrm21', 'vrm3',"CNRM_cs", "CNRM_phos", "CNRM_nitrate","CNRM_ammonium","CNRM_silicate","CNRM_POC_sed","CNRM_oxygen","CNRM_arag_sat","CNRM_pH","CNRM_sal","CNRM_temp")
  
  cat("maxent on CNRM start---")
  mod2 <- maxent(x=env_df[,PredVars], ## env conditions
                 p=env_df$occ,
                 #a=ve1,## 1:presence or 0:absence
                 path=paste0("../output/maxent_outputs2", VME_i), ## folder for maxent output;
                 # if we do not specify a folder R will put the results in a temp file,
                 # and it gets messy to read those. . .
                 args=c("responsecurves") ## parameter specification
  )
  cat("finish---predicting...")
  MA_CNRM_pred_current <- predict(mod2, stack(env1[[PredVars]]))
  writeRaster(MA_CNRM_pred_current, paste0(VME_i,'_MA_CNRM_pred_current.tif'))
  cat("predictions saved\n")
  
  #  GFDL
  PredVars<-c('depth', 'planc3', 'profc3', 'slope21', 'slope3', 'tpi21', 'tpi3', 'vrm21', 'vrm3',"GFDL_cs", "GFDL_phos", "GFDL_nitrate","GFDL_ammonium","GFDL_silicate","GFDL_POC_sed","GFDL_oxygen","GFDL_arag_sat","GFDL_pH","GFDL_sal","GFDL_temp")
  
  cat("maxent on GFDL start---")
  
  mod3 <- maxent(x=env_df[,PredVars], ## env conditions
                 p=env_df$occ,
                 #a=ve1,## 1:presence or 0:absence
                 path=paste0("../output/maxent_outputs3_",VME_i), ## folder for maxent output;
                 # if we do not specify a folder R will put the results in a temp file,
                 # and it gets messy to read those. . .
                 args=c("responsecurves") ## parameter specification
  )
  cat("finish---predicting...")
  MA_GFDL_pred_current <- predict(mod3, stack(env1[[PredVars]]))
  writeRaster(MA_GFDL_pred_current, paste0(VME_i,'_MA_GFDL_pred_current.tif'))
  cat("predictions saved\n")
  
}
