# data analysis of the variavbles to display to get an idea what the thresholds should look like

#########################################################################################
# load data and libraries and get relevant columns

#setwd("C:/Users/yaelh/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot")
library(readr)
library(ggplot2)
library(writexl)
library(ggrepel)
library(dplyr)

squarefoot <- read_csv("one-off scripts/Squarefoot_data_long_ger.csv")

dependent_vars <- c("Anzahl_Arten",
                    "Phylogenetische_Diversität",
                    "Funktionale_Diversität",
                    "Funktionale_Diversität_spezifische_Blattfläche",
                    "Funktionale_Diversität_Samengewicht",
                    "Funktionale_Diversität_Höhe",
                    "Temperatur",
                    "Nährstoff",
                    "Reaktion",
                    "Feuchtigkeit",
                    "Licht",
                    "Mahdverträglichkeit",
                    "Hemerobiezahl",
                    "Anteil_Deckung_Poaceae",
                    "Anteil_Deckung_Krautartige",
                    "Anteil_Deckung_Cyperaceae_und_Juncaceae",
                    "Stresszahl",
                    "Ruderalstrategie",
                    "Konkurrenzstrategie")

squarefoot <- squarefoot[,c(dependent_vars, "Time")]

# split into delta and historisch/resurvey as we want the thresholds separate for those two
sqft_delta <- squarefoot[squarefoot$Time == "delta",]
sqft_delta$Time <- NULL # delete time column, as we do not want to display it
sqft_longer_delta <- stack(sqft_delta)# get long format for the following functions

sqft_res_hist <- squarefoot[squarefoot$Time != "delta",]
sqft_res_hist$Time <- NULL
sqft_longer_res_hist <- stack(sqft_res_hist)# get long format for the following functions
#########################################################################################
#delta

#########################
#table

summary(sqft_delta)
stats_df_delta <- do.call(rbind, 
                          by(
                              sqft_longer_delta$values, sqft_longer_delta$ind, function(x) {
                              data.frame(
                                Min = min(x),
                                Q20 = quantile(x, 0.2),
                                Q25 = quantile(x, 0.25),
                                Q40 = quantile(x, 0.4),
                                Median = median(x),
                                Mean = mean(x),
                                Q60 = quantile(x, 0.6),
                                Q75 = quantile(x, 0.75),
                                Q80 = quantile(x, 0.8),
                                Max = max(x),
                                Delta_Q80_Q20 = quantile(x, 0.8) - quantile(x, 0.2)
                                )
                              }
                            )
                          )

stats_df_t_delta <- data.frame(t(stats_df_delta))
stats_df_t_delta <- cbind(RowName = rownames(stats_df_t_delta), stats_df_t_delta)
write_xlsx(stats_df_t_delta,path = "one-off scripts/variable_analysis_delta.xlsx")

#########################
#boxplots

ggplot(sqft_longer_delta, aes(x = factor(1), y = values)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red", width = 0.4, staplewidth = 0.1, orientation = "x" ) +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_blank())+
  stat_summary(fun = mean, geom = "point", size = 2, fill = "red", color = "black")+
  labs(x = NULL, y = NULL)+
  facet_wrap(~ind, scales = "free_y", strip.position = "top") +
  theme(panel.spacing = unit(1, "cm"))
  # trying to display the quantile numbers beside the boxplot in the graph:
  # stat_summary(fun = mean,
  #   geom = "text",
  #   aes(label = sprintf("%1.2f", ..y..)),
  #   vjust = 1.9, 
  #   color = "black",
  #   size = 3.5)+
  # stat_summary(  aes(x=1.25,label=sprintf("%1.2f", ..y..)),
  #   geom="text_repel",
  #  # nudge_x = 9,
  # #  nudge_y=1.6,
  #   fun.y = function(y) boxplot.stats(y)$stats,
  #   #position=position_nudge(x=0.3),
  #   direction = "y",
  #   size=3.5) +
  #theme(strip.text = element_text(size = 8))#, face = "bold")+


#########################################################################################
#historic and resurvey

#########################
#table

summary(sqft_res_hist)
stats_df_res_hist <- do.call(rbind, 
                          by(
                            sqft_longer_res_hist$values, sqft_longer_res_hist$ind, function(x) {
                              data.frame(
                                Min = min(x),
                                Q20 = quantile(x, 0.2),
                                Q25 = quantile(x, 0.25),
                                Q40 = quantile(x, 0.4),
                                Median = median(x),
                                Mean = mean(x),
                                Q60 = quantile(x, 0.6),
                                Q75 = quantile(x, 0.75),
                                Q80 = quantile(x, 0.8),
                                Max = max(x),
                                Delta_Q80_Q20 = quantile(x, 0.8) - quantile(x, 0.2)
                                )
                              }
                            )
                          )

stats_df_t_res_hist <- data.frame(t(stats_df_res_hist))
stats_df_t_res_hist <- cbind(RowName = rownames(stats_df_t_res_hist), stats_df_t_res_hist)
write_xlsx(stats_df_t_res_hist,path = "one-off scripts/variable_analysis_res_hist.xlsx")

#########################
#boxplots

ggplot(sqft_longer_res_hist, aes(x = factor(1), y = values)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red", width = 0.4, staplewidth = 0.1, orientation = "x" ) +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_blank())+
  stat_summary(fun = mean, geom = "point", size = 2, fill = "red", color = "black")+
  labs(x = NULL, y = NULL)+
  facet_wrap(~ind, scales = "free_y", strip.position = "top") +
  theme(panel.spacing = unit(1, "cm"))
  # trying to display the quantile numbers beside the boxplot in the graph:
  # stat_summary(fun = mean,
  #   geom = "text",
  #   aes(label = sprintf("%1.2f", ..y..)),
  #   vjust = 1.9, 
  #   color = "black",
  #   size = 3.5)+
  # stat_summary(  aes(x=1.25,label=sprintf("%1.2f", ..y..)),
  #   geom="text_repel",
  #  # nudge_x = 9,
  # #  nudge_y=1.6,
  #   fun.y = function(y) boxplot.stats(y)$stats,
  #   #position=position_nudge(x=0.3),
  #   direction = "y",
  #   size=3.5) #+
  #theme(strip.text = element_text(size = 8))#, face = "bold")  
