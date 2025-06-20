library(sf)
library(lwgeom)
library(ggthemes)
library(ggplot2)
ras <- read_sf('~/Downloads/State_Responsibility_Areas.geojson')
ras_valid <- st_make_valid(ras)
ras_valid <- st_transform(ras_valid,st_crs(lra_plus_sra))
library(maps)

ras_plot <- ggplot() + 
  geom_sf(data=ras_valid,aes(fill = as.factor(SRA))) + 
  theme_map() +
  scale_fill_tableau(name = 'Responsibility\nArea',
                     palette = 'Classic Area Green',
                     type = 'ordered-sequential')
ggsave(plot = ras_plot,filename = 'figures/sra_map.png',dpi = 450,units = 'in',height = 11,width = 10)

tableau_color_pal(type = 'ordered-sequential') 

zones <- read_sf('~/Downloads/FHSZ_SRA_LRA_Combined_-2127032115786874601.geojson')
zones_valid <- st_make_valid(zones)
zones_valid$H_or_VH <- zones_valid$FHSZ_Description %in% c('High','Very High')
theme_map

zones_plot <- ggplot() + 
  geom_sf(data=zones_valid,aes(fill = as.factor(FHSZ))) + 
  scale_fill_viridis_d(option = 'magma',direction = -1,name = 'FHSV',labels = c('Moderate','High','Very High'))+ 
  theme_map()
ggsave(plot = zones_plot,filename = 'figures/zones_map.png',dpi = 450,units = 'in',height = 11,width = 10)


ggplot() + geom_sf(data=ras_valid,aes(fill = SRA)) + theme_map()



