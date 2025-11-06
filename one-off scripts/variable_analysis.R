setwd("C:/Users/hedd/OneDrive - ZHAW/Dashboard Squarefoot Projekt/Squarefoot/Squarefoot code/squarefoot")
library(readr)
squarefoot <- read_csv("one-off scripts/Squarefoot_data_long.csv")

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

squarefoot <- squarefoot[,c(dependent_vars)]
library(ggplot2)
sqft_longer <- stack(squarefoot)

#########################
#table

summary(squarefoot)
stats_df <- do.call(rbind, by(sqft_longer$values, sqft_longer$ind, function(x) {
  data.frame(
    Min = min(x),
    Q1 = quantile(x, 0.25),
    Median = median(x),
    Mean = mean(x),
    Q3 = quantile(x, 0.75),
    Max = max(x)
  )}))

stats_df_t <- data.frame(t(stats_df))
stats_df_t <- cbind(RowName = rownames(stats_df_t), stats_df_t)
library(writexl)
#write_xlsx(stats_df_t,path = "one-off scripts/variable_analysis.xlsx")
#########################
#boxplots

library(ggrepel)
library(dplyr)
ggplot(sqft_longer, aes(x = factor(1), y = values)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red", width = 0.4, staplewidth = 0.1, orientation = "x" ) +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_blank())+
  stat_summary(fun = mean, geom = "point", size = 2, fill = "red", color = "black")+
  stat_summary(fun = mean,
    geom = "text",
    aes(label = sprintf("%1.2f", ..y..)),
    vjust = 1.9, 
    color = "black",
    size = 3.5)+
  stat_summary( aes(label=sprintf("%.2f", ..y..)),
    geom="text_repel",
    nudge_x = 0.1,
    nudge_y=1.6,
    fun.y = function(y) boxplot.stats(y)$stats,
    position=position_nudge(x=0.3),
    direction = "y", 
    size=3.5)+
  labs(x = NULL, y = NULL)+
  facet_wrap(~ind, scales = "free_y", strip.position = "top") +
  theme(panel.spacing = unit(1, "cm")) #+
  #theme(strip.text = element_text(size = 8))#, face = "bold")  

