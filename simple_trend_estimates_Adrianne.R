
### custom simple trend estimates for specific time-periods and species

library(tidyverse)
library(bbsBayes)

species_list = read.csv("inputLists/forest_species_list_Adrianne.csv")
species_inc <- species_list$common_name

# start and end years 
Y_start = c(1970)
Y_end = c(2019)

region_types = c("national","stratum", "prov_state") #types of regions to calculate
      # options include "continental","stratum", "national", "prov_state", "bcr", and "bcr_by_country" 

custom_region <- c("CA-ON-13","CA-ON-12","CA-QC-13",
                   "CA-QC-12","US-PA-13","US-NY-13",
                   "US-OH-13","US-VT-13") #ON-13 plus all neighbouring regions
regions_keep = c("Canada", "Ontario",custom_region[1],"ON_13_and_Neighbours")
      # selection of specific regions of the region_types above



### AM: creating the custom regions

st_comp_regions <- get_composite_regions(strata_type = "bbs_cws")

st_comp_regions$custom_region <- ifelse(st_comp_regions$region %in% custom_region,"ON_13_and_Neighbours","Other")




stored_files = "D:/BBS_Summaries/output/" #modify to identify folder that holds species specific output folders


strat_data = stratify(by = "bbs_cws")



allspecies.eng = strat_data$species_strat$english #database english names
allspecies.fre = strat_data$species_strat$french #database french names
allspecies.num = strat_data$species_strat$sp.bbs #databaset species numbers

  # these are species file names with no special characters
allspecies.file = str_replace_all(str_replace_all(allspecies.eng,"[:punct:]",replacement = ""),
                                  "\\s",replacement = "_")

y1 = Y_start
y2 = Y_end

trends = NULL

for(sp in species_inc){
  

    if(sp == ""){next}
    ws = which(allspecies.eng == sp)

# following loop just corrects species names for some common miss-matches ---------------------------------------
if(length(ws) == 0){
  sp_fl_2 = str_replace_all(str_replace_all(sp,"[:punct:]",replacement = ""),
                  "\\s",replacement = "_")
  
  ws = which(allspecies.file == sp_fl_2)
  
  if(length(ws) == 0){
   sp_fl_2 = paste0(sp_fl_2,"_all_forms") # relevant for YEWA, RTHA, DEJU etc.
   ws = which(allspecies.file == sp_fl_2)
   
  }
}
    if(length(ws) == 0){    stop("ERROR species name not recognized")}
    
  sp_fl = allspecies.file[ws] 
  spo = allspecies.eng[ws]
    

# Loads saved model output ------------------------------------------------
  if(file.exists(paste0(stored_files,sp_fl,"/jags_mod_full.RData"))){
   load(paste0(stored_files,sp_fl,"/jags_mod_full.RData"))
    
    inds = generate_indices(jags_mod = jags_mod,
                     jags_data = jags_data,
                     regions = region_types,
                     quantiles = c(0.025,0.5,0.975),
                     alternate_n = "n3",
                     startyear = y1)
    
    for(y1t in c(Y_start,Y_end-10)){
      
    trs = generate_trends(indices = inds,
                          quantiles = c(0.025,0.5,0.975),
                          Min_year = y1t,
                          Max_year = y2)
    trs$species <- spo # adds species name to file
    trends = bind_rows(trends,trs)
    
    }
    
    
    inds = generate_indices(jags_mod = jags_mod,
                            jags_data = jags_data,
                            alt_region_names = st_comp_regions,	
                            regions = "custom_region",				
                            quantiles = c(0.025,0.5,0.975),
                            alternate_n = "n3",
                            startyear = y1)
    
    for(y1t in c(Y_start,Y_end-10)){
      
    trs = generate_trends(indices = inds,
                          quantiles = c(0.025,0.5,0.975),
                          Min_year = y1t,
                          Max_year = y2)
    trs$species <- spo # adds species name to file
    
    
    trends = bind_rows(trends,trs)
    }
    
    # similar thing could be done to save annual indices
    
  }
  
  }



trends2 <- trends %>% filter(Region_alt %in% regions_keep)


write.csv(trends2,paste("output/trends",Sys.Date(),".csv"))



trends3 = trends2 %>% filter(Start_year == 1970,
                             Region_type %in% c("stratum","custom_region"))


CI_comp = ggplot(data = trends3,aes(x = species,y = Width_of_95_percent_Credible_Interval))+
  geom_point(aes(colour = Region))+
  coord_flip()
print(CI_comp)

T_comp = ggplot(data = trends3,aes(x = species,y = Trend))+
  geom_errorbar(aes(ymin = Trend_Q0.025,ymax = Trend_Q0.975,colour = Region),width = 0,
                position = position_dodge(width = 0.25))+
  geom_point(aes(colour = Region),
             position = position_dodge(width = 0.25))+
  coord_flip()
print(T_comp)


