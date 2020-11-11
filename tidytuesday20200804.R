library(tidyverse)
library(ggmap)
library(rnaturalearth)

## GREENENERGY

tuesdata <- tidytuesdayR::tt_load('2020-08-04')

energy_types <- tuesdata$energy_types

dat <- energy_types %>% 
    group_by(country) %>% 
    mutate(share2016 = 1-`2016`/sum(`2016`)) %>% 
    mutate(share2017 = 1-`2017`/sum(`2017`)) %>% 
    mutate(share2018 = 1-`2018`/sum(`2018`)) %>% 
    filter(type == "Conventional thermal")
    
world <- st_as_sf(rnaturalearth::countries110)
europe <- dplyr::filter(world, region_un=="Europe" & name!='Russia' & name != 'Iceland')

countrycode::guess_field(dat$country)

dat$country <- countrycode::countrycode(dat$country, "eurostat", "iso2c")
dat$iso_a2 <- dat$country
dat <- left_join(europe, dat, by = "iso_a2")


# A bounding box for continental Europe.
europe.bbox <- st_polygon(list(
    matrix(c(-25,29,45,29,45,75,-25,75,-25,29),byrow = T,ncol = 2)))

europe.clipped <- suppressWarnings(st_intersection(europe, st_sfc(europe.bbox, crs=st_crs(europe))))

ggplot(europe.clipped, aes(fill=dat$share2018)) +
    geom_sf(alpha=0.9,col='gray') +
    coord_sf(crs="+proj=eqdc +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
    viridis::scale_fill_viridis(
        option="magma",
        name="Green Energy (%)", 
        direction = -1,
        labels=scales::percent) +
    labs(x=NULL, 
         y=NULL, 
         title="Green Energy as share of Total Energy, 2018",
         caption="#tidytuesday, @mattdobra") +
    ggthemes::theme_map() +
    theme(legend.position = "bottom",
          text=element_text(family = "sans"))

  

