library(sf)
sf_use_s2(F)
load('grid_2012_mammals_real.Rdata')
load('grid_2023_mammals_real.Rdata')

load('/Users/harith/Dropbox/conservation_priorities_time/r_grid_mammals_extinction_risk.Rdata')
load('/Users/harith/Dropbox/conservation_priorities_time/r_grid_mammals_extinction_risk_2012.Rdata')


grid_2012 = grid_2012_mammals
grid_2012$ER = r_grid_mammals_ER_2012$extinction_risk
grid_2023$ER = r_grid_mammals_ER$extinction_risk

load('r_grid_mammals_richness_2012.Rdata')
load('r_grid_mammals_richness_2023.Rdata')


grid_2012$Richness = r_grid_mammals_richness_2012$richness_m_2012
grid_2023$Richness =  r_grid_mammals_richness$richness_m_2023

grid_2022 = grid_2023

plot(grid_2012$Richness,grid_2022$Richness)

library(ggplot2)
library(terra)
library(raster)
library(rnaturalearth)
worldmap = ne_countries(scale = 10,returnclass = 'sf')
worldmap = st_transform(worldmap,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")


sf_land = st_read('/Users/harith/Dropbox/african_snakes/land-polygons-complete-4326/land_polygons.shp')

sf_land = st_transform(sf_land,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")

r = rast(extent = extent(sf_land), resolution = 50000,crs = st_crs(sf_land)[[1]])
rgrid = st_as_sf(as.polygons(r,dissolve=F))

sf_land_list = st_intersects(sf_land,rgrid)
land_cells=unique(unlist(sf_land_list))

grid_2012$layer = 1:nrow(grid_2012)
grid_2012$land = 0
grid_2012[grid_2012$layer %in% land_cells,]$land = 1

grid_2022$layer = 1:nrow(grid_2022)
grid_2022$land = 0
grid_2022[grid_2022$layer %in% land_cells,]$land = 1

grid_2012[grid_2012$WEGE%in% 0, ]$WEGE = NA
grid_2012[grid_2012$WE%in% 0, ]$WE = NA

grid_2022[grid_2022$WEGE%in% 0, ]$WEGE = NA
grid_2022[grid_2022$WE%in% 0, ]$WE = NA

grid_2012[grid_2012$Richness %in% 0, ]$Richness = NA
grid_2022[grid_2022$Richness %in% 0, ]$Richness = NA



quant_2012_wege=quantile(grid_2012$WEGE,probs = 0.75,na.rm = T)
quant_2012_we=quantile(grid_2012$WE,probs = 0.75,na.rm = T)
quant_2012_er=quantile(grid_2012$ER,probs = 0.75,na.rm = T)
quant_2012_ri=quantile(grid_2012$Richness,probs = 0.75,na.rm = T)

quant_2022_wege=quantile(grid_2022$WEGE,probs = 0.75,na.rm = T)
quant_2022_we=quantile(grid_2022$WE,probs = 0.75,na.rm = T)
quant_2022_er=quantile(grid_2022$ER,probs = 0.75,na.rm = T)
quant_2022_ri=quantile(grid_2022$Richness,probs = 0.75,na.rm = T)

grid_2012$quant8 = ifelse(grid_2012$WEGE>quant_2012_wege,1,0)
grid_2022$quant8 = ifelse(grid_2022$WEGE>quant_2022_wege,2,0)
grid_2022$quant8_dif_wege = grid_2022$quant8 + grid_2012$quant8


grid_2012$quant8_we = ifelse(grid_2012$WE>quant_2012_we,1,0)
grid_2022$quant8_we = ifelse(grid_2022$WE>quant_2022_we,2,0)
grid_2022$quant8_dif_we = grid_2022$quant8_we + grid_2012$quant8_we

grid_2012$quant8_er = ifelse(grid_2012$ER>quant_2012_er,1,0)
grid_2022$quant8_er = ifelse(grid_2022$ER>quant_2022_er,2,0)
grid_2022$quant8_dif_er = grid_2022$quant8_er + grid_2012$quant8_er

grid_2012$quant8_ri = ifelse(grid_2012$Richness > quant_2012_ri,1,0)
grid_2022$quant8_ri = ifelse(grid_2022$Richness > quant_2022_ri,2,0)
grid_2022$quant8_dif_ri = grid_2022$quant8_ri + grid_2012$quant8_ri

table(grid_2022$quant8_dif_ri)

quant_2012_wege=quantile(grid_2012$WEGE,probs = 0.9,na.rm = T)
quant_2012_we=quantile(grid_2012$WE,probs = 0.9,na.rm = T)
quant_2012_er=quantile(grid_2012$ER,probs = 0.9,na.rm = T)
quant_2012_ri=quantile(grid_2012$Richness,probs = 0.9,na.rm = T)


quant_2022_wege=quantile(grid_2022$WEGE,probs = 0.9,na.rm = T)
quant_2022_we=quantile(grid_2022$WE,probs = 0.9,na.rm = T)
quant_2022_er=quantile(grid_2022$ER,probs = 0.9,na.rm = T)
quant_2022_ri=quantile(grid_2022$Richness,probs = 0.9,na.rm = T)


grid_2012$quant9_we = ifelse(grid_2012$WE>quant_2012_we,1,0)
grid_2022$quant9_we = ifelse(grid_2022$WE>quant_2022_we,2,0)
grid_2022$quant9_dif_we = grid_2022$quant9_we + grid_2012$quant9_we

grid_2012$quant9 = ifelse(grid_2012$WEGE>quant_2012_wege,1,0)
grid_2022$quant9 = ifelse(grid_2022$WEGE>quant_2022_wege,2,0)
grid_2022$quant9_dif_wege = grid_2022$quant9 + grid_2012$quant9

table(grid_2022$quant9_dif_wege)

grid_2012$quant9 = ifelse(grid_2012$ER>quant_2012_er,1,0)
grid_2022$quant9 = ifelse(grid_2022$ER>quant_2022_er,2,0)
grid_2022$quant9_dif_er = grid_2022$quant9 + grid_2012$quant9

table(grid_2022$quant9_dif_er)


grid_2012$quant9 = ifelse(grid_2012$Richness>quant_2012_ri,1,0)
grid_2022$quant9 = ifelse(grid_2022$Richness>quant_2022_ri,2,0)
grid_2022$quant9_dif_ri = grid_2022$quant9 + grid_2012$quant9

table(grid_2022$quant9_dif_ri)

coords_cancer <- matrix(c(-180, 23.5, 180, 23.5), ncol = 2, byrow = TRUE)
coords_capricorn <- matrix(c(-180, -23.5, 180, -23.5), ncol = 2, byrow = TRUE)

# Create linestring objects
line_cancer <- st_linestring(coords_cancer)
line_capricorn <- st_linestring(coords_capricorn)

# Convert to sf objects
tropic_of_cancer_sf <- st_sf(geometry = st_sfc(line_cancer, crs = 4326))
tropic_of_capricorn_sf <- st_sf(geometry = st_sfc(line_capricorn, crs = 4326))


tropic_of_cancer_sf = st_transform(tropic_of_cancer_sf,crs = st_crs(worldmap)[1]$input)
tropic_of_capricorn_sf = st_transform(tropic_of_capricorn_sf,crs = st_crs(worldmap)[1]$input)


theme_harith=theme(legend.key.height = unit(0.15,'cm'),
                   legend.key.width  = unit(0.15,'cm'),
                   legend.background = element_blank(),
                   legend.text = element_text(size = 4),
                   legend.position = c(0.92,0.9),
                   panel.background = element_blank(),
                   panel.grid.major = element_line(colour = 'grey20',linewidth = 0.05),
                   plot.margin = unit(c(0,0,0,0), "cm"),
                   legend.spacing.x = unit(0.1, "cm"))



circle_plot = function(x){
  data <- data.frame(
    category = c(0, 1, 2, 3),
    count = table(x)
  )
  data$count.x = NULL
  colnames(data)[2] = 'count'
  
  data$fraction <- data$count / sum(data$count)
  data$ymax <- cumsum(data$fraction)
  data$ymin <- c(0, head(data$ymax, n = -1))
  data = data[-1,]
  p_inset = ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = factor(category))) +
    geom_rect(col = 'black',linewidth = 0.1) +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    theme_void() +
    scale_fill_manual(values = pal)+
    theme(legend.position = 'none')
  
  return(ggplotGrob(p_inset))
}
pal = c(hcl.colors(5,'RdBu')[4],'khaki',hcl.colors(5,'RdBu')[2])

grob1 <- circle_plot(grid_2022[grid_2022$land == 1,]$quant9_dif_wege)
grob2 <- circle_plot(grid_2022[grid_2022$land == 1,]$quant8_dif_wege)
grob3 <- circle_plot(grid_2022[grid_2022$land == 1,]$quant9_dif_we)
grob4 <- circle_plot(grid_2022[grid_2022$land == 1,]$quant8_dif_we)
grob5 <- circle_plot(grid_2022[grid_2022$land == 1,]$quant9_dif_er)
grob6 <- circle_plot(grid_2022[grid_2022$land == 1,]$quant8_dif_er)
grob7 <- circle_plot(grid_2022[grid_2022$land == 1,]$quant9_dif_ri)
grob8 <- circle_plot(grid_2022[grid_2022$land == 1,]$quant8_dif_ri)

library(gridExtra)

p1 = ggplot()+
  geom_sf(data = worldmap,bg = 'grey80',col = 'grey80')+
  geom_sf(data = grid_2022[grid_2022$land == 1 & grid_2022$quant9_dif_wege>0,],aes(col = as.factor(quant9_dif_wege),fill=as.factor(quant9_dif_wege)),linewidth = 0)+
  scale_fill_manual('',values = pal,labels = c('2012','2023','Both'),na.translate = F)+
  scale_colour_manual('',values = pal,labels = c('2012','2023','Both'),na.translate = F)+
  theme_void()+
  theme_harith+
  #geom_sf(data = tropic_of_cancer_sf, color = "black", linewidth = 0.5) +
  #geom_sf(data = tropic_of_capricorn_sf, color = "black", linewidth = 0.5)
  #labs(caption = 'WEGE 9th decile')
  annotate(geom="text", x=-15702330, y=-8020048, label="WEGE(10%)",size = 2)

p1 <- p1 + annotation_custom(grob = grob1, xmin = 13009400, xmax = 17899400, ymin = -Inf, ymax = -4000000)

p2 = ggplot()+
  geom_sf(data = worldmap,bg = 'grey80',col = 'grey80')+
  geom_sf(data = grid_2022[grid_2022$land == 1 & grid_2022$quant8_dif_wege>0,],aes(col = as.factor(quant8_dif_wege),fill=as.factor(quant8_dif_wege)),linewidth = 0)+
  scale_fill_manual('',values = pal,labels = c('2012','2023','Both'),na.translate = F)+
  scale_colour_manual('',values = pal,labels = c('2012','2023','Both'),na.translate = F)+
  theme_void()+
  theme_harith+
  #geom_sf(data = tropic_of_cancer_sf, color = "black", linewidth = 0.5) +
  #geom_sf(data = tropic_of_capricorn_sf, color = "black", linewidth = 0.5)
  #labs(caption = 'WEGE 9th decile')
  annotate(geom="text", x=-15702330, y=-8020048, label="WEGE(25%)",size = 2)

p2 <- p2 + annotation_custom(grob = grob2, xmin = 13009400, xmax = 17899400, ymin = -Inf, ymax = -4000000)

p3 = ggplot()+
  geom_sf(data = worldmap,bg = 'grey80',col = 'grey80')+
  geom_sf(data = grid_2022[grid_2022$land == 1 & grid_2022$quant9_dif_we>0,],aes(col = as.factor(quant9_dif_we),fill=as.factor(quant9_dif_we)),linewidth = 0)+
  scale_fill_manual('',values = pal,labels = c('2012','2023','Both'),na.translate = F)+
  scale_colour_manual('',values = pal,labels = c('2012','2023','Both'),na.translate = F)+
  theme_void()+
  theme_harith+
  #geom_sf(data = tropic_of_cancer_sf, color = "black", linewidth = 0.5) +
  #geom_sf(data = tropic_of_capricorn_sf, color = "black", linewidth = 0.5)
  #labs(caption = 'WE 9th decile')
  annotate(geom="text", x=-15702330, y=-8020048, label="WE(10%)",size = 2)

p3 <- p3 + annotation_custom(grob = grob3, xmin = 13009400, xmax = 17899400, ymin = -Inf, ymax = -4000000)

p4 = ggplot()+
  geom_sf(data = worldmap,bg = 'grey80',col = 'grey80')+
  geom_sf(data = grid_2022[grid_2022$land == 1 & grid_2022$quant8_dif_we>0,],aes(col = as.factor(quant8_dif_we),fill=as.factor(quant8_dif_we)),linewidth = 0)+
  scale_fill_manual('',values = pal,labels = c('2012','2023','Both'),na.translate = F)+
  scale_colour_manual('',values = pal,labels = c('2012','2023','Both'),na.translate = F)+
  theme_void()+
  theme_harith+
  #geom_sf(data = tropic_of_cancer_sf, color = "black", linewidth = 0.5) +
  #geom_sf(data = tropic_of_capricorn_sf, color = "black", linewidth = 0.5)
  #labs(caption = 'WE 9th decile')
  annotate(geom="text", x=-15702330, y=-8020048, label="WE(25%)",size = 2)

p4 <- p4 + annotation_custom(grob = grob4, xmin = 13009400, xmax = 17899400, ymin = -Inf, ymax = -4000000)

p5 = ggplot()+
  geom_sf(data = worldmap,bg = 'grey80',col = 'grey80')+
  geom_sf(data = grid_2022[grid_2022$land == 1 & grid_2022$quant9_dif_er>0,],aes(col = as.factor(quant9_dif_er),fill=as.factor(quant9_dif_er)),linewidth = 0)+
  scale_fill_manual('',values = pal,labels = c('2012','2023','Both'),na.translate = F)+
  scale_colour_manual('',values = pal,labels = c('2012','2023','Both'),na.translate = F)+
  theme_void()+
  theme_harith+
  #geom_sf(data = tropic_of_cancer_sf, color = "black", linewidth = 0.5) +
  #geom_sf(data = tropic_of_capricorn_sf, color = "black", linewidth = 0.5)
  #labs(caption = 'WE 9th decile')
  annotate(geom="text", x=-15702330, y=-8020048, label="ER(10%)",size = 2)

p5 <- p5 + annotation_custom(grob = grob5, xmin = 13009400, xmax = 17899400, ymin = -Inf, ymax = -4000000)

p6 = ggplot()+
  geom_sf(data = worldmap,bg = 'grey80',col = 'grey80')+
  geom_sf(data = grid_2022[grid_2022$land == 1 & grid_2022$quant8_dif_er>0,],aes(col = as.factor(quant8_dif_er),fill=as.factor(quant8_dif_er)),linewidth = 0)+
  scale_fill_manual('',values = pal,labels = c('2012','2023','Both'),na.translate = F)+
  scale_colour_manual('',values = pal,labels = c('2012','2023','Both'),na.translate = F)+
  theme_void()+
  theme_harith+
  #geom_sf(data = tropic_of_cancer_sf, color = "black", linewidth = 0.5) +
  #geom_sf(data = tropic_of_capricorn_sf, color = "black", linewidth = 0.5)
  #labs(caption = 'WE 9th decile')
  annotate(geom="text", x=-15702330, y=-8020048, label="ER(25%)",size = 2)

p6 <- p6 + annotation_custom(grob = grob6, xmin = 13009400, xmax = 17899400, ymin = -Inf, ymax = -4000000)

p_rich9 = ggplot()+
  geom_sf(data = worldmap,bg = 'grey80',col = 'grey80')+
  geom_sf(data = grid_2022[grid_2022$land == 1 & grid_2022$quant9_dif_ri>0,],aes(col = as.factor(quant9_dif_ri),fill=as.factor(quant9_dif_ri)),linewidth = 0)+
  scale_fill_manual('',values = pal,labels = c('2012','2023','Both'),na.translate = F)+
  scale_colour_manual('',values = pal,labels = c('2012','2023','Both'),na.translate = F)+
  theme_void()+
  theme_harith+
  #geom_sf(data = tropic_of_cancer_sf, color = "black", linewidth = 0.5) +
  #geom_sf(data = tropic_of_capricorn_sf, color = "black", linewidth = 0.5)
  #labs(caption = 'WE 9th decile')
  annotate(geom="text", x=-15702330, y=-8020048, label="Richness(10%)",size = 2)

p_rich9 <- p_rich9 + annotation_custom(grob = grob7, xmin = 13009400, xmax = 17899400, ymin = -Inf, ymax = -4000000)

p_rich8 = ggplot()+
  geom_sf(data = worldmap,bg = 'grey80',col = 'grey80')+
  geom_sf(data = grid_2022[grid_2022$land == 1 & grid_2022$quant8_dif_ri>0,],aes(col = as.factor(quant8_dif_ri),fill=as.factor(quant8_dif_ri)),linewidth = 0)+
  scale_fill_manual('',values = pal,labels = c('2012','2023','Both'),na.translate = F)+
  scale_colour_manual('',values = pal,labels = c('2012','2023','Both'),na.translate = F)+
  theme_void()+
  theme_harith+
  #geom_sf(data = tropic_of_cancer_sf, color = "black", linewidth = 0.5) +
  #geom_sf(data = tropic_of_capricorn_sf, color = "black", linewidth = 0.5)
  #labs(caption = 'WE 9th decile')
  annotate(geom="text", x=-15702330, y=-8020048, label="Richness(25%)",size = 2)

p_rich8 <- p_rich8 + annotation_custom(grob = grob8, xmin = 13009400, xmax = 17899400, ymin = -Inf, ymax = -4000000)

library(gridExtra)

grid_mammals = arrangeGrob(p_rich9,p_rich8,p1,p2,p3,p4,p5,p6,ncol=2)

ggsave(plot = grid_mammals,filename = 'grid_mammals_25_10_we_wege_Sep_19.png',width = 6,height = 6,dpi = 2000)

grid_2022$WEGE_2012 = grid_2012$WEGE
grid_2022$WE_2012 = grid_2012$WE
grid_2022$ER_2012 = r_grid_mammals_ER_2012$extinction_risk
colnames(grid_2012)
grid_2022$Richness_2012 = grid_2012$Richness


load('/Users/harith/Dropbox/Postdoc_socioeconomic/r_grid_soc_eco_May27v2_2024.Rdata')
colnames(r_grid_soc_eco)

grid_2022$realms = r_grid_soc_eco$NEW_REALM

save(grid_2022,file = 'grid_2022_mammals_25_10.Rdata')

pal = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99')
p1_points = ggplot(data = grid_2022,aes(x = log10(WEGE_2012),y = log10(WEGE),col = realms))+
  geom_point(size = 0.5,alpha = 0.25)+
  #geom_smooth(method = 'lm')+
  #geom_smooth()+
  theme_minimal()+
  theme(legend.position = 'none',axis.title = element_text(size = 6),
        axis.text = element_text(size = 5))+  geom_abline(intercept = 0, slope = 1,col = 'black')+
  scale_colour_manual(values = pal)+
  annotate(geom="text", x=-1, y=-6.3, label="WEGE(log10)",size = 2)+
  ylab('2023')+
  xlab('2012')

p2_points = ggplot(data = grid_2022,aes(x = log10(WE_2012),y = log10(WE),col = realms))+
  geom_point(size = 0.5,alpha = 0.25)+
  #geom_smooth(method = 'lm')+
  #geom_smooth()+
  theme_minimal()+
  theme(legend.position = 'none',axis.title = element_text(size = 6),
        axis.text = element_text(size = 5))+  geom_abline(intercept = 0, slope = 1,col = 'black')+
  scale_colour_manual(values = pal)+
  annotate(geom="text", x=-1.5, y=-6.8, label="WE(log10)",size = 2)+
  ylab('2023')+
  xlab('2012')

p3_points = ggplot(data = grid_2022,aes(x = ER_2012,y = ER,col = realms))+
  geom_point(size = 0.5,alpha = 0.25)+
  #geom_smooth(method = 'lm')+
  #geom_smooth()+
  theme_minimal()+
  theme(legend.position = 'none',axis.title = element_text(size = 6),
        axis.text = element_text(size = 5))+
  geom_abline(intercept = 0, slope = 1,col = 'black')+
  scale_colour_manual(values = pal)+
  annotate(geom="text", x=0.95, y=0.05, label="ER",size = 2)+
  ylab('2023')+
  xlab('2012')

p0_points = ggplot(data = grid_2022,aes(x = Richness_2012,y = Richness,col = realms))+
  geom_point(size = 0.5,alpha = 0.25)+
  #geom_smooth(method = 'lm')+
  #geom_smooth()+
  theme_minimal()+
  theme(legend.position = 'none',axis.title = element_text(size = 6),
        axis.text = element_text(size = 5))+
  geom_abline(intercept = 0, slope = 1,col = 'black')+
  scale_colour_manual(values = pal)+
  annotate(geom="text", x=210, y=10, label="Richness",size = 2)+
  ylab('2023')+
  xlab('2012')


grid_mammals = arrangeGrob(p_rich9,p_rich8,p0_points,p1,p2,p1_points,p3,p4,p2_points,p5,p6,p3_points,ncol=3)

ggsave(plot = grid_mammals,filename = 'grid_grid_mammals_complete_Sep_19_2024.png',width = 9,height = 7,dpi = 2000)


nrow(grid_2022[grid_2022$quant9_dif_wege %in% c(1,2,3),])/nrow(grid_2022[grid_2022$land>0,])
#11.6% of land for wege priorities in both years

