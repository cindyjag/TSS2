library(ggplot2)
library(dplyr)
library(reshape2)
library(plotly)
library(lme4)
library(readr)
library(sjPlot)
library(tidyverse) 
library(haven) 
library(sjstats) 
library(effects) 
library(jtools) 
library(ROCR) 
library(patchwork)
library(Routliers)


all_data <- read.table("tss2_data.txt", 
                        header = TRUE, 
                        sep = "\t", 
                        stringsAsFactors = FALSE)

#####OUTLIERS


detect_mad_outliers <- function(x, threshold = 5) {
  med <- median(x, na.rm = TRUE)
  mad_val <- mad(x, na.rm = TRUE)
  abs(x - med) / mad_val > threshold
}

clean_data <- all_data %>%
  group_by(subj) %>%
  mutate(outlier = detect_mad_outliers(rTime)) %>%
  filter(!outlier) %>%
  ungroup()



all_data <- all_data[!all_data$rTime >10,]


###########

##aggregieren
agg_data <- all_data %>%
  group_by(subj,condition) %>%
  summarize(
    mean_antwort = mean(antwort),
    mean_rTime = mean(rTime),
    sd = sd(antwort),
    sdr = sd (rTime)
  )

agg_data_r <- clean_data %>%
  group_by(subj,condition) %>%
  summarize(
    mean_antwort = mean(antwort),
    mean_rTime = mean(rTime),
    sd = sd(antwort),
    sdr = sd (rTime)
  )







plot_agg <-all_data %>%
  group_by(condition) %>%
  summarize(
    mean_antwort = mean(antwort),
    mean_rTime = mean(rTime),
    se = sd(antwort)/sqrt(length((all_data))),
    ser = sd(rTime)/sqrt(length((all_data)))
  )


plot_agg_r <-clean_data %>%
  group_by(condition) %>%
  summarize(
    mean_antwort = mean(antwort),
    mean_rTime = mean(rTime),
    se = sd(antwort)/sqrt(length((all_data))),
    ser = sd(rTime)/sqrt(length((all_data)))
  )

overall <-clean_data %>%
  group_by(subj) %>%
  summarize(
    mean_antwort = mean(antwort),
    mean_rTime = mean(rTime),
    se = sd(antwort)/sqrt(length((all_data))),
    ser = sd(rTime)/sqrt(length((all_data)))
  )

###############################################

#######size for plots
large_text_theme <- theme(
  text = element_text(size = 12),        
  plot.title = element_text(size = 12),  
  axis.title = element_text(size = 12),  
  axis.text = element_text(size = 10)  ,
  axis.title.x = element_text(margin = margin(t = 20)),
  legend.position = "none"
)
#################################### t tests for choice


t.test(overall$mean_antwort, y = NULL, alternative = "two.sided", mu = 0.5)

#test large temporal small spatial vs small spatiotemporal
t.test(agg_data[agg_data$condition == 0,]$mean_antwort, agg_data[agg_data$condition == 1,]$mean_antwort,paired = TRUE)
#test large temporal small spatial vs large spatiotemporal
t.test(agg_data[agg_data$condition == 0,]$mean_antwort, agg_data[agg_data$condition == 2,]$mean_antwort,paired = TRUE)
#test small spatiotemporal vs large spatiotemporal
t.test(agg_data[agg_data$condition == 1,]$mean_antwort, agg_data[agg_data$condition == 2,]$mean_antwort,paired = TRUE)


#################################### t test für response time
#test large temporal small spatial vs small spatiotemporal
t.test(agg_data_r[agg_data_r$condition == 0,]$mean_rTime, agg_data_r[agg_data_r$condition == 1,]$mean_rTime,paired = TRUE)
#test large temporal small spatial vs large spatiotemporal
t.test(agg_data_r[agg_data_r$condition == 0,]$mean_rTime, agg_data_r[agg_data_r$condition == 2,]$mean_rTime,paired = TRUE)
#test small spatiotemporal vs large spatiotemporal
t.test(agg_data_r[agg_data_r$condition == 1,]$mean_rTime, agg_data_r[agg_data_r$condition == 2,]$mean_rTime,paired = TRUE)






#####figure 3
plot_agg$condition <- as.factor (plot_agg$condition)
custom_labels <- c("small-spatial-large-temporal", "small", "large")

ggplot(agg_data, aes(x = as.factor(condition), y = mean_antwort)) +
  geom_jitter(aes(color = as.factor(condition)), width = 0.2, alpha = 0.8, size = 3) + 
  geom_point(data = plot_agg, aes(x = as.factor(condition), y = mean_antwort), 
             color = "black", size = 3) +
  geom_errorbar(data = plot_agg, aes(x = as.factor(condition), ymin = mean_antwort - se, ymax = mean_antwort + se), 
                width = 0.2, color = "black", size = 1) +
  labs(title = "",
       x = "spatiotemporal scale", y = "% 'temporally shorter distance was longer'") +
  #ylab(expression(paste("% 'temporally shorter \n distance was longer'"))) +
  geom_hline(yintercept = 0.5, linetype = "dotted", col = "black")+
#  facet_wrap(~ speed_comparison)+
  
  scale_x_discrete(labels = custom_labels)+
  theme_minimal()+
  theme(legend.position = "None")+
  scale_color_manual(values = c("0" = "purple", "1" = "green3", "2" = "skyblue")) + 
  large_text_theme




##########figure 4

ggplot(agg_data_r, aes(x = as.factor(condition), y = mean_rTime)) +
  geom_jitter(aes(color = as.factor(condition)), width = 0.2, alpha = 0.8, size = 3) + 
  geom_point(data = plot_agg_r, aes(x = as.factor(condition), y = mean_rTime), 
             color = "black", size = 3) +
  geom_errorbar(data = plot_agg_r, aes(x = as.factor(condition), ymin = mean_rTime - ser, ymax = mean_rTime + ser), 
                width = 0.2, color = "black", size = 1) +
  labs(title = "",
       x = "spatiotemporal scale", y = "response time [s]") +
  scale_x_discrete(labels = custom_labels)+
  theme_minimal()+
  theme(legend.position = "None")+
  scale_color_manual(values = c("0" = "purple", "1" = "green3", "2" = "skyblue")) + 
  large_text_theme



