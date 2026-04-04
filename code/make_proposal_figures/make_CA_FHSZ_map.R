library(ggplot2)
library(sf)
library(lwgeom)
library(ggthemes)
library(ggplot2)


lra1 <- st_read('data/feather_large_files/Fire Hazard Severity Zones in Local Responsibility Area/FHSZLRA25_Phase1_v1/geodatabase/FHSZLRA25_Phase1_v1.gdb')
lra2 <- st_read('data/feather_large_files/Fire Hazard Severity Zones in Local Responsibility Area/FHSZLRA25_Phase2_v1/Geodatabase/FHSZLRA25_Phase2_v1.gdb')
lra3 <- st_read('data/feather_large_files/Fire Hazard Severity Zones in Local Responsibility Area/FHSZLRA25_Phase3_v1/FHSZLRA25_Phase3_v1.gdb')
lra4 <- st_read('data/feather_large_files/Fire Hazard Severity Zones in Local Responsibility Area/FHSZLRA25_Phase4_v1/FHSZLRA25_1_Phase4_v1.gdb')
lra <- do.call(rbind,list(lra1,lra2,lra3,lra4))
sra <- st_read('data/feather_large_files/Fire Hazard Severity Zones in Local Responsibility Area/FHSZSRA_23_3/Geodatabase/FHSZSRA_23_3.gdb')
lra_plus_sra <- rbind(lra,sra)
lra_sra_h_vh <- lra_plus_sra[lra_plus_sra$FHSZ %in% c(1,2,3),]
validity <- st_is_valid(lra_sra_h_vh)
lra_sra_h_vh <- lra_sra_h_vh[!is.na(validity),]
lra_sra_h_vh <- st_make_valid(lra_sra_h_vh)

library(ggnewscale)

ras <- read_sf('data/feather_large_files/Responsibility_Areas.geojson')
ras_valid <- st_make_valid(ras)
ras_valid <- st_transform(ras_valid,st_crs(lra_sra_h_vh))

california <- tigris::counties(state = 'CA',cb = T,year = 2020,class= 'sf')
california <- st_transform(california,st_crs(ras_valid))

blue_cols <- tableau_color_pal(
  palette = "Blue",
  type = c("ordered-sequential"),
  direction = 1
)(20)[c(2,11,20)]
orange_cols <- tableau_color_pal(
  palette = "Orange",
  type = c("ordered-sequential"),
  direction = 1
)(20)[c(2,11,20)]

lra_sra_h_vh$SRA_FHSZ <- paste(lra_sra_h_vh$SRA,lra_sra_h_vh$FHSZ_Description,sep = '_')
library(forcats)
lra_sra_h_vh$SRA_FHSZ <- fct_relevel(lra_sra_h_vh$SRA_FHSZ,  'LRA_Moderate','LRA_High','LRA_Very High','SRA_Moderate','SRA_High','SRA_Very High')
library(ggtext)
library(tidyverse)
ras_plot <- ggplot() + 
  geom_sf(data = california,col = NA,fill = 'grey80') + 
  #geom_sf(data=ras_valid[ras_valid$SRA=='FRA',],aes(fill = 'grey75')) + 
  geom_sf(data = lra_sra_h_vh,aes(fill = SRA_FHSZ),col = NA) + 
  theme_map() +
  scale_fill_manual(
    name = c('LRA          SRA    '),
    labels=c(rep(c('Mod.','High','V. High'),2)),
    values = c(orange_cols,blue_cols),
    guide = guide_legend(#reverse = T,
                         nrow = 3)) + 
  theme(
    text = element_text(family = 'Arial'),
  #  plot.title = element_text(size = 12,
  #    margin = margin(b = -10) # Adjust the negative value to decrease space
  #  ),
 #   plot.caption = element_text(size = 10,
  #    margin = margin(t = -10) # Adjust the negative value to decrease space
  #  ),
    legend.position= 'inside',
 plot.tag.location = 'margin',
 plot.tag.position = 'bottom',
 plot.tag = element_text(hjust = 0,face = 'italic',family = 'Arial'),
    legend.position.inside = c(0.45,0.65)) + 
  labs(#title = 'FSHZs by responsibility area in California, 2025',
       tag = str_wrap("Figure 5: FSHZs (2025) by local (LRA) and state (SRA) responsibility area in California. Unshaded areas include FRA and LRA/SRA areas not in a FSHZ.",width = 80))
ggsave(plot = ras_plot,filename = 'figures/sra_map_2026.png',dpi = 600,units = 'in',height = 5.5,width = 5)

 
plot.margin = unit(rep(0,4), "cm"), 
  scale_fill_tableau(name = 'Responsibility\nArea',
                     palette = 'Classic Area Green',
                     type = 'ordered-sequential') +
  new_scale_fill() + 
  geom_sf(data = lra_sra_h_vh,aes(fill = as.factor(FHSZ_Description))) + 
  scale_fill_manual(values = c("#51127CFF" ,"#000004FF"))




head(lra_sra_h_vh)
ras_plot <- ggplot() + 
  #geom_sf(data = california) + 
  geom_sf(data=ras_valid[ras_valid$SRA=='FRA',],fill = 'grey50')) + 
  theme_map() +
  scale_fill_tableau(name = 'Responsibility\nArea',
                     palette = 'Classic Area Green',
                     type = 'ordered-sequential') +
  new_scale_fill() + 
  geom_sf(data = lra_sra_h_vh,aes(fill = as.factor(FHSZ_Description))) + 
  scale_fill_manual(values = c("#51127CFF" ,"#000004FF"))
ggsave(plot = ras_plot,filename = 'figures/sra_map.png',dpi = 450,units = 'in',height = 11,width = 10)

tableau_div_gradient_pal(palette = 'Orange-Blue Diverging')
?tableau_div_gradient_pal




"#b3e0a6" "#a5db96" "#98d687" "#8ed07f" "#85ca77" "#7dc370"
[7] "#75bc69" "#6eb663" "#67af5c" "#61a956" "#59a253" "#519c51"
[13] "#49964f" "#428f4d" "#398949" "#308344" "#2b7c40" "#27763d"
[19] "#256f3d" "#24693d"

"#000004FF" "#51127CFF" "#B63679FF" "#FB8861FF" "#FCFDBFFF"
scales::viridis_pal(option = 'turbo')(7)
"#30123BFF" "#4686FBFF" "#1AE4B6FF" "#A2FC3CFF" "#FABA39FF" "#E4460AFF"
[7] "#7A0403FF"
ggplot() + 
  theme_map() + 
  geom_sf(data = lra_sra_h_vh,aes(fill = FHSZ_Description)) +
  scale_fill_gradient_tableau()