library(ggplot2)
library(sf)
library(lwgeom)
library(ggthemes)
library(ggplot2)


lra <- st_read('~/Downloads/FHSZLRA25_1_All.gdb')
sra <- st_read('~/Downloads/FHSZSRA_23_3/FHSZSRA_23_3.gdb')
lra_plus_sra <- rbind(lra,sra)
lra_sra_h_vh <- lra_plus_sra[lra_plus_sra$FHSZ %in% c(1,2,3),]
validity <- st_is_valid(lra_sra_h_vh)
lra_sra_h_vh <- lra_sra_h_vh[!is.na(validity),]
lra_sra_h_vh <- st_make_valid(lra_sra_h_vh)

library(ggnewscale)

ras <- read_sf('~/Downloads/State_Responsibility_Areas.geojson')
ras_valid <- st_make_valid(ras)
ras_valid <- st_transform(ras_valid,st_crs(lra_sra_h_vh))

california <- tigris::counties(state = 'CA',cb = T,year = 2020,class= 'sf')
california <- st_transform(california,st_crs(ras_valid))

green_cols <- tableau_color_pal(
  palette = "Green",
  type = c("ordered-sequential"),
  direction = 1
)(20)[c(2,11,20)]
orange_cols <- tableau_color_pal(
  palette = "Orange",
  type = c("ordered-sequential"),
  direction = 1
)(20)[c(2,11,20)]

lra_sra_h_vh$SRA_FHSZ <- paste(lra_sra_h_vh$SRA,lra_sra_h_vh$FHSZ_Description,sep = '_')
library(ggtext)
library(tidyverse)
ras_plot <- ggplot() + 
  geom_sf(data = california,aes(fill = 'grey60'),col = NA) + 
  #geom_sf(data=ras_valid[ras_valid$SRA=='FRA',],aes(fill = 'grey75')) + 
  geom_sf(data = lra_sra_h_vh,aes(fill = SRA_FHSZ),col = NA) + 
  theme_map() +
  scale_fill_manual(
    name = c('SRA          LRA    '),
    labels=c('other',rep(c('V. High','High','Mod.'),2)),
    values = c('grey75',green_cols,orange_cols),
    guide = guide_legend(reverse = TRUE,nrow = 3)) + 
  theme(
    text = element_text(family = 'Times'),
  #  plot.title = element_text(size = 12,
  #    margin = margin(b = -10) # Adjust the negative value to decrease space
  #  ),
 #   plot.caption = element_text(size = 10,
  #    margin = margin(t = -10) # Adjust the negative value to decrease space
  #  ),
    legend.position= 'inside',
 plot.tag.location = 'margin',
 plot.tag.position = 'bottom',
 plot.tag = element_text(hjust = 0),
    legend.position.inside = c(0.45,0.65)) + 
  labs(#title = 'FSHZs by responsibility area in California, 2025',
       tag = str_wrap("Figure 3: FSHZs by responsibility area in California based on current (2025) maps. 'other' = FRA or SRA/LRA area not in a FSHZ",width = 80))
ggsave(plot = ras_plot,filename = 'figures/sra_map.png',dpi = 450,units = 'in',height = 5.5,width = 5)

 
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