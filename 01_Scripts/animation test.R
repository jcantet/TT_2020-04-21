options(scipen = 999999)

# Packages ====
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(patchwork)
library(gganimate)
library(tidyr)
library(jcan) # custom theme for ggplot and custom functions

# Data ====
gdpr_violations <- readr::read_tsv('00_Inputs/gdpr_violations.tsv.txt')
gdpr_text <- readr::read_tsv('00_Inputs/gdpr_text.tsv.txt')

# Passage de la date au bon format + suppression des doublons
gdpr_violations <- gdpr_violations %>% mutate(date = mdy(date))
gdpr_violations <- unique(gdpr_violations)







# 2e tentative : en tidy : on met number et sum dans la même colonne : OK
data_animate2 <-     
  gdpr_violations %>% 
  group_by(name) %>% 
  summarize(number = n(),
            sum = sum(price)) %>% 
  arrange(desc(number)) %>% 
  mutate(number_order = row_number()) %>% # création du rang sur le nombre d'amendes
  arrange(desc(sum)) %>% 
  mutate(sum_order = row_number()) %>% # création du range sur le montant total des amendes
  pivot_longer(cols = c("number_order","sum_order"),names_to = "type") %>% 
  mutate(name = as.factor(name),
         type = as.factor(type),
         lab_sum = as.character(ifelse(type == "number_order", number,sum)), # pour ne pas voir les décimales lors des transitions (pas de solution au probleme avec du numérique)
         sum = ifelse(type == "number_order", number*1000000,sum), # fois 1M pour avoir un graphique avec les mêmes échelles. Si l'axe X soit d'adapter, on ne voit pas de modification de la taille des barres
         type = ifelse(type == "number_order", "Number","Amount")) %>% 
  arrange(type)

# Changer le format des étiquettes
# Prévoir une colonne pour la taille fictive
(static_plot <- 
    ggplot(data = data_animate2, aes(x = value, group = name, fill = sum))+
    geom_text(aes(y = 0, label = paste(name, " ")), vjust = 0.2, hjust = 1) +
    geom_text(aes(y = sum/2 ,label =  lab_sum), hjust=0)+
    scale_fill_jcan(discrete = FALSE, palette = "progressive rouge", reverse = TRUE)+
    coord_flip(clip = "off", expand = FALSE)+
    geom_tile(aes(y = sum/2,
                  height = sum,
                  width = 0.9), alpha = 0.8, color = NA)+
    scale_y_continuous(labels = scales::comma) +
    scale_x_reverse() +
    guides(color = FALSE, fill = FALSE) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.x = element_line( size=.1, color="grey90" ),
          panel.grid.minor.x = element_line( size=.1, color="grey90" ),
          plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
          plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
          plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
          plot.background=element_blank(),
          plot.margin = margin(2,2, 2, 4, "cm")))


(anim <- static_plot + transition_states(states = type, transition_length = 8, state_length = 1,wrap = TRUE) +
    # view_follow(fixed_x = TRUE) +
    labs(title = 'Countries ranking : {closest_state}',  
         subtitle  =  "25 european coutries",
         caption  = "Data Source : TidyTuesday 2020-04-21 | @j_cantet"))



animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("02_Outputs/gganim2.gif"))  






# Test repris d'un article de blog pour tester GGanimate =====

gdp_tidy <- read_csv("./00_inputs/gdp_tidy.csv")
gdp_formatted <- gdp_tidy %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",round(value/1e9))) %>%
  group_by(country_name) %>% 
  filter(rank <=10) %>%
  ungroup()    


staticplot = ggplot(gdp_formatted, aes(rank, group = country_name, 
                                       fill = as.factor(country_name), color = as.factor(country_name))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country_name, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))  

anim = staticplot + transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'GDP per Year : {closest_state}',  
       subtitle  =  "Top 10 Countries",
       caption  = "GDP in Billions USD | Data Source: World Bank Data")  

animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("example.gif"))  
