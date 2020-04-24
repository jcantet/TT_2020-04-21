options(scipen = 999999)

# Packages ====
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(patchwork)
library(tidyr)
library(jcan) # custom theme for ggplot and custom functions

# Data ====
gdpr_violations <- readr::read_tsv('00_Inputs/gdpr_violations.tsv.txt')
gdpr_text <- readr::read_tsv('00_Inputs/gdpr_text.tsv.txt')

# Passage de la date au bon format + suppression des doublons
gdpr_violations <- gdpr_violations %>% mutate(date = mdy(date))
gdpr_violations <- unique(gdpr_violations)

# Exploration ====

  # Nombre d'amendes par année
  ggplot(data = gdpr_violations %>% filter(date >= "2010-01-01"),aes(x = year(date)))+
  geom_bar()+
  labs(title = "Nombre d'amendes liées au RGPD par années",subtitle = "TidyTuesday 2020-04-21",caption = "@j_cantet",
       x = 'Année', y = "Nombre")+
  theme_jcan()


  # Distribution des amendes infligées
  ggplot(data = gdpr_violations, aes(x= price))+
    geom_histogram(bins = 30, na.rm = TRUE)+
    scale_x_log10(labels = scales:::dollar_format(prefix = NULL, suffix = "€", big.mark = " "))+
    labs(title = "Distribution du montant des amendes liées au RGPD par années",subtitle = "TidyTuesday 2020-04-21",caption = "@j_cantet",
         x = 'Nombre', y = "Montants")+
    theme_jcan()

  
  # Pays les plus concernés par le montant des amendes
  (g3 <- 
    gdpr_violations %>% 
    group_by(name) %>% 
    summarize(price = sum(price)) %>% 
    ggplot( aes(x = price, y = reorder(name, price),fill = price))+
      geom_col(show.legend = FALSE)+
      labs(title = "Classement des pays en fonction du montant des amendes ",subtitle = "TidyTuesday 2020-04-21",caption = "@j_cantet",
           x = 'Cumul du montant des amendes', y = "Pays")+
      theme_jcan()+
      theme(axis.ticks.y = element_blank())+
      scale_x_continuous(expand = c(0,0,0,1500000), labels = scales:::dollar_format(prefix = NULL, suffix = "€", big.mark = " "))+
      scale_fill_jcan(discrete = FALSE, palette = "progressive rouge",reverse = TRUE))
    
  
  # Pays les plus concernés par le nombre d'amendes
  (g4 <- 
    gdpr_violations %>% 
    group_by(name) %>% 
    summarize(number = n()) %>% 
    ggplot(aes(x = number, y = reorder(name, number),fill = number))+
    geom_col(show.legend = FALSE)+
    labs(title = "Classement des pays en fonction du nombre d'amendes ",subtitle = "TidyTuesday 2020-04-21",caption = "@j_cantet",
         x = "Cumul du nombre d'amendes", y = "Pays")+
    theme_jcan()+
    theme(axis.ticks.y = element_blank())+
    scale_x_continuous(expand = c(0,0))+
    scale_fill_jcan(discrete = FALSE, palette = "progressive rouge",reverse = TRUE))

  # Comparaison des classements
  g3+labs(caption = NULL) + g4 +labs(y = NULL)
  # Export
  ggsave("TT_20200421.png",path = "02_Outputs/",width = 16, height = 8)
  
  
  
  
  # Graphique avec bulle pour les types de violation au RGPD pour la couleur, la position en fonction du nombre, et la taille en fonction du montant des amendes
  (g5 <- 
    gdpr_violations %>% 
      group_by(type) %>% 
      summarize(number = n(),
                sum = sum(price)) %>% 
      ggplot(aes(x = number, y =  sum, color = type, size = sum))+
      geom_point(show.legend = FALSE)+
      scale_size(trans = "log10")+
      scale_y_log10(labels = function(x) paste0(x/1000000, "M€"))+
      scale_x_log10()+
      labs(title = "Par type, montants et nombre des amendes ",subtitle = "TidyTuesday 2020-04-21",caption = "@j_cantet",
           y = "Cumul du nombre d'amendes", x = "Nombre d'amendes")+
      theme_jcan()+
      scale_color_jcan())

  
  