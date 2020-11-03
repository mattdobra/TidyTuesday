library(tidyverse)
library(gridExtra)
library(tvthemes)
# library(extrafont)
extrafont::loadfonts()
extrafont::loadfonts(device = "win")

extrafont::font_import()

tuesdata <- tidytuesdayR::tt_load('2020-08-11')
avatar <- tuesdata$avatar

avatar$imdb_rating[avatar$book_num==1 & avatar$chapter_num==20] <- 9.7

avatar$bookchap <- paste(avatar$book_num,avatar$chapter_num, sep = " ")

dat1 <- avatar %>% 
    group_by(bookchap) %>% 
    slice(1)

diravg <- dat1 %>% 
    group_by(director) %>% 
    mutate(avgrat = mean(imdb_rating)) %>% 
    mutate(eps = n()) %>% 
    slice(1) %>% 
    select(director, avgrat, eps) %>% 
    arrange(-avgrat) %>% 
    mutate(episodes = ifelse(eps==1, paste(eps, "", "Episode"), paste(eps, "", "Episodes")))

import_avatar()

diravg %>% ggplot() +
    geom_bar(aes(x = reorder(director, avgrat), y = avgrat, fill=director),
              position = "dodge", stat = "identity", show.legend = FALSE) +
    geom_text(aes(x = reorder(director, avgrat), y = 0, label = episodes), 
              position = position_nudge(y = 2),
              family = "Slayer") +
    labs(y = "Average IMDB Rating",
         x = "Director") +
    scale_color_avatar(palette = "EarthKingdom") +
    scale_fill_avatar(palette = "EarthKingdom") +
    scale_y_continuous(expand = c(0,0)) +
    coord_flip() +
    labs(title = "Avatar Directors") +
    theme_avatar(title.font = "Slayer",
                  text.font = "Slayer") 


