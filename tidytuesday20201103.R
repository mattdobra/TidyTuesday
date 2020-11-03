library(tidyverse)
library(broom)

tuesdata <- tidytuesdayR::tt_load('2020-11-03')
ikea <- tuesdata$ikea

ikea2 <- ikea %>% 
    mutate(volume=depth*width*height*.000001) %>% 
    drop_na(volume) %>% 
    select(-sellable_online, -other_colors, -short_description, -item_id, -name, -link, -old_price, -designer)
 
    group_by(category) %>% 
    do(fit = tidy(lm(price ~ volume, data=.))) %>% 
    unnest(fit)

fit2 <- fit2 %>% 
    filter(term == "volume") %>% 
    select(category, estimate) %>% 
    arrange(desc(estimate)) %>% 
    mutate(estimate = floor(estimate))

fit2$posneg <- ifelse(fit2$estimate<0, "neg", "pos")

theme_set(theme_bw())

ikeaplot <- ggplot(data = fit2, aes(y = reorder(category, estimate), x = estimate, label = estimate)) +
    geom_point(stat = "identity", color = "#ffcc00", size = 12) +
    geom_segment(aes(x = 0,
                     y = category,
                     xend = estimate,
                     yend = category),
                 size = 1.5,
                 color = "#ffcc00") +
    geom_vline(xintercept = 0,
               size = 1.5,
               color = "#ffcc00") +
    geom_text(color = "#003399") +
    labs(y = "",
         x = "Additional Cost (in Saudi Riyals) Per Cubic Meter of Item Volume",
         title = "Does Size Matter (For Ikea Pricing)?",
         subtitle = "An Analysis of the Relationship Between Price and Item Size at Ikea",
         caption = "Ridiculous #TidyTuesday plot by @mattdobra") +
    theme(panel.border=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(color = "#ffcc00"),
          axis.text = element_text(color = "#ffcc00"),
          panel.background = element_rect(fill = "#003399"),
          plot.background = element_rect(fill = "#003399")) 
ggsave("ikeaplot.png", height = 8, width = 8)

    