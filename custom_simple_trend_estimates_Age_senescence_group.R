
### custom simple trend estimates for specific time-periods and species

library(tidyverse)
library(bbsBayes)


load("c:/Senescence/data/full_database_hyperparameter_trends_All_Species.RData")

species_inc <- BETA_nat$species

# start and end years (matching)
Y_start = c(2004)
Y_end = c(2019)

region_types = c("national")

regions_keep = c("Canada")


stored_files = "C:/BBS_Summaries/output/"


strat_data = stratify(by = "bbs_cws")



allspecies.eng = strat_data$species_strat$english
allspecies.fre = strat_data$species_strat$french
allspecies.num = strat_data$species_strat$sp.bbs

allspecies.file = str_replace_all(str_replace_all(allspecies.eng,"[:punct:]",replacement = ""),
                                  "\\s",replacement = "_")

y1 = Y_start
y2 = Y_end

trends = NULL
for(sp in species_inc){
  

    if(sp == ""){next}
    ws = which(allspecies.eng == sp)
if(length(ws) == 0){
  sp_fl_2 = str_replace_all(str_replace_all(sp,"[:punct:]",replacement = ""),
                  "\\s",replacement = "_")
  
  ws = which(allspecies.file == sp_fl_2)
  
  if(length(ws) == 0){
   sp_fl_2 = paste0(sp_fl_2,"_all_forms") 
   ws = which(allspecies.file == sp_fl_2)
   
  }
}
    if(length(ws) == 0){    stop("ERROR species name not recognized")}
    
  sp_fl = allspecies.file[ws] 
  spo = allspecies.eng[ws]
    
  if(file.exists(paste0(stored_files,sp_fl,"/jags_mod_full.RData"))){
   load(paste0(stored_files,sp_fl,"/jags_mod_full.RData"))
    
    inds = generate_indices(jags_mod = jags_mod,
                     jags_data = jags_data,
                     regions = region_types,
                     quantiles = c(0.025,0.5,0.975),
                     alternate_n = "n3",
                     startyear = y1)
    
    trs = generate_trends(indices = inds,
                          quantiles = c(0.025,0.5,0.975),
                          Min_year = y1,
                          Max_year = y2)
    trs$species <- spo
    
    trends = bind_rows(trends,trs)
    
  }
  
  }



trends <- trends %>% filter(Region_alt %in% regions_keep)
trends <- trends %>% select(Trend,Trend_Q0.025,Trend_Q0.975,species) %>% 
  mutate(.variable = "BETA",
         model = "Published") %>% 
  rename(lci_Trend = Trend_Q0.025,
         uci_Trend = Trend_Q0.975)
full_out <- bind_rows(BETA_nat,trends)

save(list = c("full_out","BETA_nat"),
     file = "c:/Senescence/data/full_database_hyperparameter_trends_All_Species.RData")
#write.csv(trends,paste("output/trends",Sys.Date(),".csv"))


