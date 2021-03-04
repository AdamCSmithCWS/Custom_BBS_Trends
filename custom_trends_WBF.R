## March 2, 2021
## Ann's attempted script for custom strata, species and years for WBF species
## modifying from github Ontario example (https://github.com/AdamCSmithCWS/Custom_BBS_Trends/blob/main/project_specific_applications/custom_simple_trend_estimates.R) to include WBF species and BCR-province strata of interest


library(tidyverse)
library(bbsBayes)


species_times <- read.csv("inputLists/WBF_species_lists.csv")	## AM: modified from Ontario example - I created two species columns for the same list of species, one to generate 2009-2019 trends, one for 1970-2019

# start and end years (matching)
Y_start = c(2009,1970)			## AM: modified from Ontario example - I'm assuming this will then calculate 2 trends, one 2009-2019, one 1970-2019, for columns 1 and 2 of the .csv file
Y_end = c(2019,2019)



### AM: I don't think these 2 lines are required, because instead we're using the st_comp_regions to create province-BCR regions

region_types = c("national")

#regions_keep = c("Ontario","Canada")


stored_files = "C:/BBS_Summaries/output/" #modify to identify folder that holds species specific output folders


### AM: creating the custom regions

st_comp_regions <- get_composite_regions(strata_type = "bbs_cws")


## AM: I don't know entirely what these allspecies lines are doing but I assume it has something to do with fixing up species names to the correct format in the first part of the loop, so I assume we do need these lines...

strat_data = stratify(by = "bbs_cws")

allspecies.eng = strat_data$species_strat$english
allspecies.fre = strat_data$species_strat$french
allspecies.num = strat_data$species_strat$sp.bbs

allspecies.file = str_replace_all(str_replace_all(allspecies.eng,"[:punct:]",replacement = ""),
                                  "\\s",replacement = "_")



## AM: option 1 - include all BCR 7; this is what's used in the trend analysis below but we'll want to repeat with option 2 as well
st_comp_regions$WBF7 <- ifelse(st_comp_regions$region %in% c("CA-AB-6","CA-AB-8","CA-BC-6","CA-BC-4","CA-SK-6","CA-SK-8","CA-MB-6","CA-MB-8","CA-YT-4","CA-YT-6","CA-NT-4","CA-NT-6","CA-BCR7-7"),"WBF7","Other")

## AM: option 2 - don't include any of BCR 7
st_comp_regions$WBFno7 <- ifelse(st_comp_regions$region %in% c("CA-AB-6","CA-AB-8","CA-BC-6","CA-BC-4","CA-SK-6","CA-SK-8","CA-MB-6","CA-MB-8","CA-YT-4","CA-YT-6","CA-NT-4","CA-NT-6"),"WBFno7","Otherno7")

trends = NULL
species = species_times[,1]


for(reg_header in c("WBF7","WBFno7")){
  
  
for(sp in species){
  if(sp == ""){next}
  if(sp == "Gray Jay"){sp <- "Canada Jay"}
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
    
for(j in 1:length(Y_start)){
  
  y1 = Y_start[j]
  y2 = Y_end[j]
  
     
    inds = generate_indices(jags_mod = jags_mod,
                     jags_data = jags_data,
                     #regions = region_types,			## AM: here I've replaced the regions line with the custom regions
				alt_region_names = st_comp_regions,	
                        regions = reg_header,				## AM: repeat with regions = "WBFno7" in a separate run; or is it possible to use regions = c("WBF7","WBFno7") and their results would show up in separate columns?
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
}

}#end of reg_header loop

#trends <- trends %>% filter(Region_alt %in% regions_keep)	## AM: I don't think we need this line since we've already specified the regions..?

# reliability category cut-off definitions ----------------------------------------

prec_cuts = c(abs(2*((0.7^(1/20))-1)),
              abs(2*((0.5^(1/20))-1)))*100 
names(prec_cuts) <- c("High","Medium")

## no coverage possible because calculating coverage for these custom regions is not currently possible
# cov_cuts = c(0.5,0.25)
# names(cov_cuts) <- c("High","Medium")

## not relevant with this model
# pool_cuts = c(0.33,0.1)
# names(pool_cuts) <- c("High","Medium")

backcast_cuts = c(0.90,0.75)
names(backcast_cuts) <- c("High","Medium")

source("Functions/reliability.R")

trends_out <- trends %>% 
  mutate(precision_reliability = reliab_func_prec(Width_of_95_percent_Credible_Interval),
         local_data_reliability = reliab_func_backcast(backcast_flag)) %>% 
  rowwise() %>% 
  mutate(overall_reliability_without_coverage = min(precision_reliability,local_data_reliability)) %>%  #add the reliability information.
  select(species,Start_year,End_year,Region,Region_type,
                     Trend,
                     Trend_Q0.025,
                     Trend_Q0.5,
                     Trend_Q0.975,
                     Percent_Change,
                     Percent_Change_Q0.025,
                     Percent_Change_Q0.5,
                     Percent_Change_Q0.975,
         precision_reliability,
         local_data_reliability,
         overall_reliability_without_coverage,
                     Relative_Abundance,
                     Observed_Relative_Abundance,
                     Number_of_strata,
                     Width_of_95_percent_Credible_Interval,
                     Number_of_Routes,
                     Mean_Number_of_Routes) %>% 
  arrange(species,Region_type,Region,Start_year)  #adding a more useful column order for the sake of simplicity.


write.csv(trends_out,paste("output/trends",Sys.Date(),".csv"))
