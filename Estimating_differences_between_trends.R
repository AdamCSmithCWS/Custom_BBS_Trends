
### custom simple trend estimates for specific time-periods and species

library(tidyverse)
library(bbsBayes)
library(tidybayes)

species_inc <- c("Eastern Whip-poor-will")

# start and end years 
Y_start = c(1997,2009)
Y_end = c(2007,2019)

region_types = c("national") #types of regions to calculate
      # options include "continental","stratum", "national", "prov_state", "bcr", and "bcr_by_country" 

regions_keep = c("Canada")
      # selection of specific regions of the region_types above

#stored_files = "C:/BBS_Summaries/output/" #modify to identify folder that holds species specific output folders

stored_files = "D:/BBS_Summaries/output/" #modify to identify folder that holds species specific output folders

strat_data = stratify(by = "bbs_cws")



allspecies.eng = strat_data$species_strat$english #database english names
allspecies.fre = strat_data$species_strat$french #database french names
allspecies.num = strat_data$species_strat$sp.bbs #databaset species numbers

  # these are species file names with no special characters
allspecies.file = str_replace_all(str_replace_all(allspecies.eng,"[:punct:]",replacement = ""),
                                  "\\s",replacement = "_")



y1 = Y_start[i]
y2 = Y_end[i]

trends = NULL
sp = species_inc 

     ws = which(allspecies.eng == sp)

# # following loop just corrects species names for some common miss-matches ---------------------------------------
# if(length(ws) == 0){
#   sp_fl_2 = str_replace_all(str_replace_all(sp,"[:punct:]",replacement = ""),
#                   "\\s",replacement = "_")
#   
#   ws = which(allspecies.file == sp_fl_2)
#   
#   if(length(ws) == 0){
#    sp_fl_2 = paste0(sp_fl_2,"_all_forms") # relevant for YEWA, RTHA, DEJU etc.
#    ws = which(allspecies.file == sp_fl_2)
#    
#   }
# }
#     if(length(ws) == 0){    stop("ERROR species name not recognized")}
#     
  sp_fl = allspecies.file[ws] 
  spo = allspecies.eng[ws]
    
# 
# # Loads saved model output ------------------------------------------------
#   if(file.exists(paste0(stored_files,sp_fl,"/jags_mod_full.RData"))){
#    
#     

# Load original model output ----------------------------------------------
    load(paste0(stored_files,sp_fl,"/jags_mod_full.RData"))
    
    
  #percent per year trend function
  p_py <- function(i1,i2,ny = nyear){
    ppy <- 100*((i2/i1)^(1/(ny))-1)
    return(ppy)
  }
  #percent change function
  p_ch <- function(i1,i2){
    pch <- 100*((i2/i1)-1)
    return(pch)
  }
  

# gather full posterior of smoothed indices for trend estimation ----------

 inds = gather_draws(jags_mod,n3[s,y])
  
  strats_full = get_composite_regions("bbs_cws")
  strats_keep = strats_full[which(strats_full$Country == "Canada"),]
  
  ### this drops all strata not included in strats_keep
  strat_df = data.frame(s = jags_data$strat,
                        strat_name = jags_data$strat_name) %>% 
    distinct() %>% 
    inner_join(.,strats_keep,by = c("strat_name" = "region")) %>% 
    mutate(area_weight = area_sq_km/sum(area_sq_km))
  
  years_df = data.frame(y = jags_data$year,
                        year = jags_data$r_year) %>% 
    distinct()
    
  
  ### this drops all strata not included in strats_keep
  ## and creates a long-form df of composite indices
  inds_comp = inner_join(inds,strat_df,by = "s") %>% 
    left_join(.,years_df,by = "y") %>% 
    mutate(wind = .value*area_weight) %>% 
    group_by(.draw,year) %>% 
    summarise(ind = sum(wind)) %>% 
    rename(draw = .draw)
   
  
  
  for(i in 1:length(Y_start)){
    start_year = Y_start[i]
    end_year = Y_end[i]
    
  comp_trt = inds_comp %>% 
    filter(year %in% c(start_year,end_year)) %>% #drop unnecessary years
    pivot_wider(.,id_cols = any_of(c("draw")),
                names_from = year,
                values_from = ind,
                names_prefix = "Y") %>% 
    rename_with(.,~gsub(pattern = paste0("Y",start_year),"Y1",.x,fixed = TRUE))%>% 
    rename_with(.,~gsub(pattern = paste0("Y",end_year),"Y2",.x,fixed = TRUE)) %>% 
    mutate(tr = p_py(Y1,Y2,ny = (end_year-start_year)),
           pch = p_ch(Y1,Y2)) %>% 
    select(draw,tr,pch) %>% 
    rename_with(.,~gsub(pattern = "tr",paste0("tr",i),.x,fixed = TRUE)) %>% 
    rename_with(.,~gsub(pattern = "pch",paste0("pch",i),.x,fixed = TRUE))

    
  if(i == 1){
    tr_comp_wide = comp_trt
  }else{
    tr_comp_wide = left_join(tr_comp_wide,comp_trt,by = "draw")
    
  }
  
    
  }
  
  tr_comp_wide <- tr_comp_wide %>% 
    mutate(dif_trend = tr2-tr1,
           dif_percent_change = pch2-pch1)
  
  diff_summary = tr_comp_wide %>%
    ungroup() %>% 
    summarise(trend_97_07 = mean(tr1),
              trend_97_07_lci = quantile(tr1,0.025),
              trend_97_07_uci = quantile(tr1,0.975),
              trend_09_19 = mean(tr2),
              trend_09_19_lci = quantile(tr2,0.025),
              trend_09_19_uci = quantile(tr2,0.975),
              pch_97_07 = mean(pch1),
              pch_97_07_lci = quantile(pch1,0.025),
              pch_97_07_uci = quantile(pch1,0.975),
              pch_09_19 = mean(pch2),
              pch_09_19_lci = quantile(pch2,0.025),
              pch_09_19_uci = quantile(pch2,0.975),
              difference_trend = mean(dif_trend),
              dif_trend_lci = quantile(dif_trend,0.025),
              dif_trend_uci = quantile(dif_trend,0.975),
              difference_percent_change = mean(dif_percent_change),
              dif_percent_change_lci = quantile(dif_percent_change,0.025),
              dif_percent_change_uci = quantile(dif_percent_change,0.975))
  
  write.csv(diff_summary,paste0("output/",sp_fl,"trends_and_differences_in_trends.csv"))
  
  
