library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2020-11-10')

mobile <- tuesdata$mobile
landline <- tuesdata$landline

mobile <- mobile %>% 
    select(code, year, mobile_subs)

dat1 <- left_join(landline, mobile, c("code", "year"))
dat2 <- na.omit(dat1)
dat3 <- dat2 %>% filter(year == 2010 | year == 2017)

dat4 <- dat3 %>% 
    group_by(code) %>% 
    mutate(gdp_per_cap = log(gdp_per_cap)) %>% 
    mutate(growth = gdp_per_cap-lag(gdp_per_cap)) %>% 
    mutate(growth = growth/7) %>% 
    mutate(mobile_subs = log(mobile_subs)) %>% 
    mutate(cellchange = mobile_subs-lag(mobile_subs)) %>% 
    mutate(cellchange = cellchange/7) %>% 
    mutate(landline_subs = log(landline_subs)) %>% 
    mutate(landchange = landline_subs - lag(landline_subs)) %>% 
    mutate(landchange = landchange/7) %>% 
    mutate(cellswitch = cellchange/landchange) %>% 
    ungroup() %>% 
    filter(code != "COD") # No DRC data for 2017 landlines

dat5 <- na.omit(dat4)

plot(x = dat5$growth, y = dat5$landchange, data = dat5)

reg1 <- lm(cellchange ~ growth, data = dat5)
reg2 <- lm(landchange ~ growth, data = dat5)
reg3 <- lm(cellswitch ~ growth, data = dat5)
stargazer::stargazer(reg1, reg2, reg3, type = "text")


# This plot works, but has a significant outlier
caption <- paste(strwrap("On average, a 1 percentage point increase in economic growth rate
                         corresponds with a 1.08 percentage point increase in the growth of cell phone 
                         subscriptions per capita (p < .01)", 60), collapse = "\n")


ggplot(data = dat5, aes(x = growth, y = cellchange)) +
    geom_point() +
    geom_smooth(method = lm, color = "blue", fill = "blue") +
    annotate(geom = "text",
             x = -.035,
             y = .65,
             hjust = 0,
             vjust = 1,
             color = "slategray",
             size = 5,
             label = caption) +
    labs(
        title = "Cell Phone Adoption Rates, 2010-2017",
        subtitle = "Faster growing economies had significantly higher cell phone adoption rates",
        x = "Annual Economic Growth Rate (Real GDP per Capita)",
        y = "Annual Growth in Cell Phone Use (Subscriptions per Capita)",
        caption = "Data from Our World in Data and #TidyTuesday, created by Matt Dobra @mattdobra"
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    theme_classic()
ggsave("cellphone.png", height = 4, width = 8)

# Same as above, rescales y axis so myanmar is not visible
ggplot(data = dat5, aes(x = growth, y = cellchange)) +
    geom_point() +
    geom_smooth(method = lm, color = "blue", fill = "blue") +
    labs(
        title = "Cell Phone Adoption Rates, 2010-2017",
        subtitle = "Faster growing economies had significantly higher cell phone adoption rates",
        x = "Annual Economic Growth Rate (Real GDP per Capita)",
        y = "Annual Growth in Cell Phone Use (Subscriptions per Capita)",
        caption = "Data from Our World in Data and #TidyTuesday, created by Matt Dobra @mattdobra"
    ) +
    scale_y_continuous(labels = scales::percent,
                       limits=c(-.05,.3)) +
    scale_x_continuous(labels = scales::percent) +
    theme_classic()
ggsave("cellphone2.png", height = 8, width = 8)
