##2014 microplastic in Lake Superior

##library
library(ggplot2)
library(rgdal)
library(tidyverse)
library(lubridate)
library(doBy)
library(here)
library(readr)
library(scatterpie)
library(scales)
library(qwraps2)
library(png)
library(magick)
library(readxl)


###########################################################################################################
##set default themes for all plots and maps
map_theme<-theme(axis.text=element_text(size=24, family='serif'), 
                 axis.title=element_text(size=24, family='serif'), 
                 plot.title=element_text(size=24, family='serif'),
                 plot.subtitle=element_text(size=20, family='serif'),
                 plot.caption=element_text(size=20, family='serif'), 
                 legend.position=c(0.08,0.7),
                 legend.text=element_text(size=18, family='serif'), 
                 legend.title=element_text(size=18, family='serif'),
                 strip.text=element_text(size=20, family='serif'))



plot_theme<-theme(axis.line=element_line(size=1, color='black'),
                  panel.background = element_rect(NA),
                  axis.text=element_text(size=24, family='serif'),
                  axis.title=element_text(size=24, family='serif'),
                  plot.margin = margin(.5,.5,.5,.5,"cm"),
                  legend.text=element_text(size=24, family='serif'),
                  axis.ticks=element_line(size=1, color='black'),
                  plot.title=element_text(size=24, family='serif'),
                  plot.subtitle=element_text(size=20, family='serif'),
                  plot.caption=element_text(size=20, family='serif'),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  legend.title=element_text(size=24, family='serif'),
                  strip.text=element_text(size=20, family='serif'))

ann_data_access<-'Data: U.S. Geological Survey, Lake Superior Biological Station
       Cox, K., 2018. Masters thesis, University of Waterloo'

###########################################################################################################
##load data tables
effort<-read_excel(here('Data','SuperiorPlastic2014.xlsx'), sheet = 'Effort')

plastic<-read_excel(here('Data','SuperiorPlastic2014.xlsx'), sheet = 'PCount')

##change date format into usable form
effort$Date <- as.character(effort$Date)

##Reformat plastic data to a tidy long version and calculate plastic density
pdata.1 <- plastic %>%
  pivot_longer(5:9, names_to = "Type", values_to = "Count") %>%
  left_join(effort) %>%
  mutate(perKM = Count/(TowLength_km_2digits * (1/1000)), na.rm = TRUE) %>%
  subset(Serial != 679)


##Sum particle densities across particle sizes then average the two nets at each site
pdata.2 <- pdata.1 %>%
  group_by(Serial, Type) %>%
  summarize(Psum = sum(perKM)) %>%
  left_join(pdata.1) %>%
  distinct(Serial, Type, .keep_all = TRUE) %>%
  select(1:5, 8, 10:12)

pdata.3 <- pdata.2 %>%
  group_by(Location, Type) %>%
  summarize(Pmean = mean(Psum)) %>% 
  left_join(pdata.1) %>%
  distinct(Location, Type, .keep_all = TRUE) %>%
  select(1:3, 8, 10:12)


##Sum across types to get total plastic at each site
pdata.4 <- pdata.3 %>%
  group_by(Location) %>%
  summarize(Ptotal = sum(Pmean, na.rm = TRUE)) %>% 
  left_join(pdata.1) %>%
  distinct(Location, .keep_all = TRUE) %>%
  select(1:2, 8, 10:12)

  
############################################################################################################
##Superior Map of sampling locations
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)


ggplot(plastic, aes(Longitude, Latitude)) +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude')+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=effort, aes(Longitude, Latitude), size=6, shape=21, fill = 'black', alpha=0.6) +
  theme_bw() +
  map_theme  +
  labs(title='Lake Superior Plastic Debris Sampling Locations' , 
       subtitle='Surface water samples collected made May-July 2014', 
       caption = 'U.S. Geological Survey, Lake Superior Biological Station 
       Cox, K., 2018. Masters thesis, University of Waterloo') 

ggsave(here('Plots and Tables/LS2014_PlasticSites.png'), dpi = 300, width = 35, height = 16, units = "cm")


##map of average total plastic abundance ---------------------------------------------------------------------

my_breaks = c(1000, 10000, 100000, 500000) 

ggplot(pdata.4, aes(Longitude, Latitude)) +
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = pretty_breaks())+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=pdata.4, mapping=aes(Longitude, Latitude, color=Ptotal, size = Ptotal), stroke=1.5)+
  theme_bw() +
  map_theme+
  theme(legend.position=c(0.15,0.75)) +
  scale_size_continuous(name=expression(underline(Pieces~per~km^2)), breaks=pretty_breaks()) +
  scale_color_continuous(high='red',low='deepskyblue2', name=expression(underline(Pieces~per~km^2)), breaks=pretty_breaks()) + 
  scale_fill_distiller(direction = -1, palette="RdYlBu", breaks=pretty_breaks(5)) +
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs( x='Longitude', y='Latitude',
        title='Lake Superior Surface Plastic Density',
        subtitle='Collections made May-July 2014',
        caption=ann_data_access)

ggsave(here('Plots and Tables/LS2014_TotalPlasticDensity.png'), dpi = 300, width = 40, height = 20, units = "cm")



#############################
##Map plastic densities by type

ggplot(pdata.3, aes(Longitude, Latitude)) +
  scale_y_continuous(breaks = pretty_breaks())+
  scale_x_continuous(breaks = pretty_breaks())+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=pdata.3, mapping=aes(Longitude, Latitude, color=Pmean, size = Pmean), stroke=1.5)+
  theme_bw() +
  map_theme+
  theme(legend.position=c(0.75,0.10)) +
  scale_size_continuous(name=expression(underline(Pieces~per~km^2)), breaks=pretty_breaks()) +
  scale_color_continuous(high='red',low='deepskyblue2', name=expression(underline(Pieces~per~km^2)), breaks=pretty_breaks()) + 
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs( x='Longitude', y='Latitude',
        title='Lake Superior Surface Plastic Density',
        subtitle='Collections made May-July 2014',
        caption=ann_data_access) +
  facet_wrap(vars(Type), ncol=2) 

ggsave(here('Plots and Tables/LS2014_TotalPlasticDensity_byType.png'), dpi = 300, width = 60, height = 40, units = "cm")



##Total plastic density by distance from shore

ggplot(pdata.4, aes(x=DistShore_m/1000, y=Ptotal)) +
  geom_point()+
  geom_smooth() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous()+
  plot_theme +
  theme(legend.position=c(0.8, 0.8)) + 
  labs( x='Sampling distance from shore (km)', y= expression(Pieces~per~km^2),
        title='Lake Superior Surface Plastic Density',
        subtitle='Collections made May-July 2014',
        caption=ann_data_access)

ggsave(here('Plots and Tables/LS_CiscoeLarvae_Density_Distance.png'), dpi = 300, width = 40, height = 20, units = "cm")




