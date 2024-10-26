library(sf)
sf_use_s2(F)

library(ggplot2)
library(terra)
library(raster)
library(rnaturalearth)
worldmap = ne_countries(scale = 10,returnclass = 'sf')
worldmap = st_transform(worldmap,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")


sf_land = st_read('/Users/harith/Dropbox/african_snakes/land-polygons-complete-4326/land_polygons.shp')

sf_land = st_transform(sf_land,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")

r = rast(extent = extent(sf_land), resolution = 50000,crs = st_crs(sf_land)[[1]])
library(gridExtra)


load('grid_2022_mammals.Rdata')

grid_2022_mammals = grid_2022

load('grid_2022_amphibians.Rdata')

grid_2022_amphibians = grid_2022

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

theme_harith=theme(legend.key.height = unit(0.15,'cm'),
                   legend.key.width  = unit(0.15,'cm'),
                   legend.background = element_blank(),
                   legend.text = element_text(size = 4),
                   legend.position = c(0.92,0.9),
                   panel.background = element_blank(),
                   panel.grid.major = element_line(colour = 'grey20',linewidth = 0.05),
                   plot.margin = unit(c(0,0,0,0), "cm"),
                   legend.spacing.x = unit(0.1, "cm"))
#mammals

grob1 <- circle_plot(grid_2022_amphibians[grid_2022_amphibians$land == 1,]$quant9_dif_wege)
pal = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99')

p1_points_a = ggplot(data = grid_2022_amphibians,aes(x = log10(WEGE_2012),y = log10(WEGE),col = realms))+
  geom_point(size = 0.5,alpha = 0.25)+
  theme_minimal()+
  theme(legend.position = 'none',axis.title = element_text(size = 6),
        axis.text = element_text(size = 5))+  geom_abline(intercept = 0, slope = 1,col = 'black')+
  theme(plot.subtitle = element_text(family = 'bold'))+
  scale_colour_manual(values = pal)+
  annotate(geom="text", x=0, y=-6.3, label="WEGE(log10)",size = 2)+
  ylab('2023')+
  xlab('2012') +labs(subtitle = 'B')

pal = c(hcl.colors(5,'RdBu')[4],'khaki',hcl.colors(5,'RdBu')[2])

p1_a = ggplot()+
  geom_sf(data = worldmap,bg = 'grey80',col = 'grey80')+
  geom_sf(data = grid_2022_amphibians[grid_2022_amphibians$land == 1 & grid_2022_amphibians$quant9_dif_wege>0,],aes(col = as.factor(quant9_dif_wege),fill=as.factor(quant9_dif_wege)),linewidth = 0)+
  scale_fill_manual('',values = pal,labels = c('2012','2022','Both'),na.translate = F)+
  scale_colour_manual('',values = pal,labels = c('2012','2022','Both'),na.translate = F)+
  theme_void()+
  theme_harith+
  theme(plot.subtitle = element_text(family = 'bold'))+
  #geom_sf(data = tropic_of_cancer_sf, color = "black", linewidth = 0.5) +
  #geom_sf(data = tropic_of_capricorn_sf, color = "black", linewidth = 0.5)
  #labs(caption = 'WEGE 9th decile')
  annotate(geom="text", x=-15702330, y=-8020048, label="WEGE(10th quantile)",size = 2)+
  labs(subtitle = 'A')

p1_a <- p1_a + annotation_custom(grob = grob1, xmin = 13009400, xmax = 17899400, ymin = -Inf, ymax = -4000000)

#mammals
grob1 <- circle_plot(grid_2022_mammals[grid_2022_mammals$land == 1,]$quant9_dif_wege)
pal = c(hcl.colors(5,'RdBu')[4],'khaki',hcl.colors(5,'RdBu')[2])

p1_m = ggplot()+
  geom_sf(data = worldmap,bg = 'grey80',col = 'grey80')+
  geom_sf(data = grid_2022_mammals[grid_2022_mammals$land == 1 & grid_2022_mammals$quant9_dif_wege>0,],aes(col = as.factor(quant9_dif_wege),fill=as.factor(quant9_dif_wege)),linewidth = 0)+
  scale_fill_manual('',values = pal,labels = c('2012','2022','Both'),na.translate = F)+
  scale_colour_manual('',values = pal,labels = c('2012','2022','Both'),na.translate = F)+
  theme_void()+
  theme_harith+
  theme(plot.subtitle = element_text(family = 'bold'))+
  #geom_sf(data = tropic_of_cancer_sf, color = "black", linewidth = 0.5) +
  #geom_sf(data = tropic_of_capricorn_sf, color = "black", linewidth = 0.5)
  #labs(caption = 'WEGE 9th decile')
  annotate(geom="text", x=-15702330, y=-8020048, label="WEGE(10th quantile)",size = 2)+
  labs(subtitle = 'C')


p1_m <- p1_m + annotation_custom(grob = grob1, xmin = 13009400, xmax = 17899400, ymin = -Inf, ymax = -4000000)
pal = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99')

p1_points_m = ggplot(data = grid_2022_mammals,aes(x = log10(WEGE_2012),y = log10(WEGE),col = realms))+
  geom_point(size = 0.5,alpha = 0.25)+
  #geom_smooth(method = 'lm')+
  #geom_smooth()+
  theme_minimal()+
  theme(legend.position = 'none',axis.title = element_text(size = 6),
        axis.text = element_text(size = 5),
        plot.subtitle = element_text(family = 'bold'))+  geom_abline(intercept = 0, slope = 1,col = 'black')+
  scale_colour_manual(values = pal)+
  annotate(geom="text", x=-0.75, y=-6.3, label="WEGE(log10)",size = 2)+
  ylab('2023')+
  xlab('2012')+
  labs(subtitle = 'D')

grid_all = arrangeGrob(p1_a,p1_points_a,p1_m,p1_points_m, ncol=2)

ggsave(plot = grid_all,filename = 'grid_all_complete.png',width = 9,height = 5,dpi = 2000)


# load('/Users/harith/Dropbox/Postdoc_socioeconomic/r_grid_soc_eco_DEC4.Rdata')
# colnames(r_grid_soc_eco)
# unique(r_grid_soc_eco$REALM)
# 
# sf_new_realms = st_read('/Users/harith/Dropbox/postdoc_KU_paper_2/CMEC regions & realms/newRealms.shp')
# 
# p = ggplot()+
#   geom_sf(data =sf_new_realms,aes(fill = Realm,col = Realm),size = 0)+
#   theme_void()+
#   scale_fill_manual(values = pal)+
#   scale_colour_manual(values = pal)+
#   theme(legend.position = 'none')
# 
# ggsave(plot = p,filename = 'legend_plot.png',width = 9,height = 5,dpi = 2000)
# 
# 
# test = ggplot(data = grid_2022_mammals,aes(x = log10(WEGE_2012),y = log10(WEGE),col = realms))+
#   geom_point(size = 1)+
#   #geom_smooth(method = 'lm')+
#   #geom_smooth()+
#   theme_minimal()+
#   theme(legend.position = 'bottom',axis.title = element_text(size = 6),
#         axis.text = element_text(size = 5))+  geom_abline(intercept = 0, slope = 1,col = 'black')+
#   scale_colour_manual(values = pal)+
#   annotate(geom="text", x=0.2, y=-6.3, label="WEGE(log10)",size = 2)+
#   ylab('2023')+
#   xlab('2012')
# 
# ggsave(plot = test,filename = 'test.png',width = 9,height = 5,dpi = 2000)

colnames(grid_2022_amphibians)
lm_richness_a = lm(data = grid_2022_amphibians,Richness~Richness_2012)
summary(lm_richness_a)

lm_we_a = lm(data = grid_2022_amphibians,WE~WE_2012)
summary(lm_we_a)

lm_ER_a = lm(data = grid_2022_amphibians,ER~ER_2012)
summary(lm_ER_a)

lm_WEGE_a = lm(data = grid_2022_amphibians,WEGE~WEGE_2012)
summary(lm_WEGE_a)


lm_richness_m = lm(data = grid_2022_mammals,Richness~Richness_2012)
summary(lm_richness_m)

lm_we_m = lm(data = grid_2022_mammals,WE~WE_2012)
summary(lm_we_m)

lm_WEGE_m = lm(data = grid_2022_mammals,WEGE~WEGE_2012)
summary(lm_WEGE_m)
