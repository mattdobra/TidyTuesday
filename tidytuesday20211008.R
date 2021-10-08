library(tidyverse)

library(tidytuesdayR)
library(sf)
library(maps)
library(tools)
library(scales)

tuesdata <- tidytuesdayR::tt_load('2021-10-05')
nurses <- tuesdata$nurses

nurses2020 <- nurses %>% 
    filter(Year == 2020)

nurses2020 %>% ggplot(aes(y = `Hourly Wage Avg`, x = `Location Quotient`)) +
    geom_point(color = "darkgreen") +
    geom_smooth(method = lm, se = FALSE, color = "green4") +
    theme_classic() +
    labs(title = "Nurse Scarcity and Nurse Wages",
         subtitle = "2020 Bureau of Labor Statistics data",
         x = "Relative abundance of Nurses",
         y = "Average Hourly Wages",
         caption = "#TidyTuesday Project by @mattdobra")



states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
states$ID <- toTitleCase(states$ID)

dat <- left_join(states, nurses2020, by = c("ID" = "State"))
dat <- dat %>% 
    mutate(avgwage = `Hourly Wage Avg`)

dat %>% ggplot() +
    geom_sf(aes(fill = avgwage)) +
    coord_sf() +
    theme_void() +
    labs(title = "Wages of Registered Nurses by State",
         subtitle = "2020 Bureau of Labor Statistics data",
         fill = "Average Dollars per Hour",
         caption = "#TidyTuesday Project by @mattdobra") +
    theme(legend.position="bottom") +
    scale_fill_gradient(low = "cornflowerblue", high = "midnightblue", labels = dollar)

dat <- dat %>% 
    mutate(pct90 = `Hourly 90th Percentile`) %>% 
    mutate(pct10 = `Hourly 10th Percentile`) %>% 
    mutate(ineq = pct90/pct10)

dat %>% ggplot() +
    geom_sf(aes(fill = ineq)) +
    coord_sf() +
    theme_void() +
    labs(title = "Wage Inequality of Registered Nurses",
         subtitle = "Ratio of 90th Percentile to 10th Percentile Wages",
         fill = "Ratio",
         caption = "#TidyTuesday Project by @mattdobra") +
    theme(legend.position="bottom") +
    scale_fill_gradient(low = "navajowhite", high = "indianred")


nurses10 <- nurses %>% 
    filter(Year == 2020 | Year == 2010) %>% 
    group_by(State) %>% 
    arrange(Year) %>% 
    mutate(avgwage = `Hourly Wage Avg`) %>% 
    mutate(wagegrowth=(avgwage-lag(avgwage))/lag(avgwage)) %>% 
    filter(Year == 2020)

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
states$ID <- toTitleCase(states$ID)

dat2 <- left_join(states, nurses10, by = c("ID" = "State"))

dat2 %>% ggplot() +
    geom_sf(aes(fill = wagegrowth)) +
    coord_sf(crs = st_crs(5070)) + # EPSG code for projection: Others to try 4326 (google), 4269 (govt), 3857 (mercator)
    theme_void() +
    labs(title = "Wage growth of Registered Nurses",
         subtitle = "Change in average hourly wages from 2010 to 2020",
         fill = "Wage growth",
         caption = "#TidyTuesday Project by @mattdobra") +
    theme(legend.position="bottom") +
    scale_fill_gradient(low = "darkslategray1", high = "darkslateblue", labels = percent)
