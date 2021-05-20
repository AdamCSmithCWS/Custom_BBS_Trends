## building a Stan version of the bbsBayes models

library(bbsBayes)
library(tidyverse)
library(sf)



# spatial data load -------------------------------------------------------

laea = st_crs("+proj=laea +lat_0=40 +lon_0=-95") # Lambert equal area coord reference system

locat = system.file("maps",
                    package = "bbsBayes")
map.file = "BBS_CWS_strata" # the most standard BBS stratification, used here just to provide some spatial limits

# reading in the strata spatial file (GIS shapefile)
strata_map = read_sf(dsn = locat,
                     layer = map.file)
strata_map = st_transform(strata_map,crs = laea) #reprojecting the geographic coordinate file to an equal area projection



# BBS strata map ----------------------------------------------------------

strata_detail <- bbsBayes::get_composite_regions("bbs_cws")
strata_map_plot <- inner_join(strata_map,strata_detail,
                              by = c("ST_12" = "region")) %>% 
  mutate(bcr = factor(bcr))
st_demo = ggplot(data = strata_map_plot)+
  geom_sf(alpha = 0.1, aes(fill = bcr))+
  geom_sf_text(aes(label = ST_12,colour = bcr),
               size = 1)+
  theme_void()+
  theme(legend.position = "none")+
  scale_colour_viridis_d(end = 1,option = "turbo",
                         aesthetics = c("colour","fill"))

pdf(file = "BBS_strata_map.pdf",
    height = 8.5,
    width = 11)
print(st_demo)
dev.off()

all_strata = as.character(strata_map_plot$ST_12)
north_strata = all_strata[1:37] #includes Canada and Alaska
south_strata = all_strata[38:length(all_strata)]
northern_strata_map <- filter(strata_map_plot,ST_12 %in% north_strata)
southern_strata_map <- filter(strata_map_plot,ST_12 %in% south_strata)

st_demoN = ggplot(data = northern_strata_map)+
  geom_sf(alpha = 0.1, aes(fill = bcr))+
  geom_sf_text(aes(label = ST_12,colour = bcr),
               size = 2)+
  theme_void()+
  theme(legend.position = "none")+
  scale_colour_viridis_d(end = 1,option = "turbo",
                         aesthetics = c("colour","fill"))

pdf(file = "BBS_northern_strata_map.pdf",
    height = 8.5,
    width = 11)
print(st_demoN)
dev.off()


st_demoS = ggplot(data = southern_strata_map)+
  geom_sf(alpha = 0.1, aes(fill = bcr))+
  geom_sf_text(aes(label = ST_12,colour = bcr),
               size = 2)+
  theme_void()+
  theme(legend.position = "none")+
  scale_colour_viridis_d(end = 1,option = "turbo",
                         aesthetics = c("colour","fill"))

pdf(file = "BBS_southern_strata_map.pdf",
    height = 8.5,
    width = 11)
print(st_demoS)
dev.off()


st_demoS2 = ggplot(data = southern_strata_map)+
  geom_sf(alpha = 0.4, aes(fill = bcr))+
  geom_sf_label(aes(label = ST_12),
                colour = grey(0.2),
                size = 2,
                label.size = 0.15,
                label.padding = unit(0.1,"lines"))+
  theme_void()+
  theme(legend.position = "none")+
  scale_colour_viridis_d(end = 1,option = "turbo",
                         aesthetics = c("colour","fill"))

pdf(file = "BBS_southern_strata_map2.pdf",
    height = 8.5,
    width = 11)
print(st_demoS2)
dev.off()



