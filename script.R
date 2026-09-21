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
library(emmeans)

setwd("C:/Users/Cindy Jagorska/Downloads")

all_data <- read.table("tss2_data.txt", 
                        header = TRUE, 
                        sep = "\t", 
                        stringsAsFactors = FALSE)
getwd()
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



#all_data <- all_data[!all_data$rTime >10,]
all_data <- clean_data

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
  group_by(condition,subj) %>%
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
##lmer for answers

model1 <- glmer(antwort ~ as.factor(condition) + (1 | subj),
               data = clean_data,
               family = binomial(link = "logit"));summary(model1)

null_model <- glmer(antwort ~ 1 + (1 | subj),
                    data = clean_data,
                    family = binomial(link = "logit"))

anova(null_model, model1)
tab_model(model1)

emm <- emmeans(model1, ~ condition)
pairs(emm, adjust = "bonferroni", infer = TRUE) 

##standardized estimates
estimate1 <- 0.462
estimate2 <- -0.546
estimate3 <- -1.008

d_logit1 <- estimate1 / 1.81
d_logit1
d_logit2 <- estimate2 / 1.81
d_logit2
d_logit3 <- estimate3 / 1.81
d_logit3

mean(predict(model1, type = "response"))

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  ratio <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(ratio = ratio, p = pval)
}
overdisp_fun(model1)
plot(fitted(model1), residuals(model1, type = "pearson"))
abline(h = 0, col = "red")

##lmer for reaction times
model2 <- lmer(rTime ~ as.factor(condition) + (1 | subj),
               data = clean_data)
tab_model(model2)

null_model2 <- lmer(rTime ~ 1 + (1 | subj),
                    data = clean_data)
anova(null_model2, model2)

emm <- emmeans(model2, ~ condition)
contrasts_df <- as.data.frame(pairs(emm, adjust="bonferroni"))
contrasts_df[, c("contrast","estimate","SE","p.value")]
resid_sd <- sigma(model2)
resid_sd
contrasts_df$d <- contrasts_df$estimate / resid_sd
contrasts_df[, c("contrast","estimate","d","SE","p.value")]


pairs(emm, adjust = "bonferroni", infer = TRUE) 

library(effectsize)
effectsize(model2, method="residual")

qqnorm(residuals(model2))
qqline(residuals(model2))

plot(fitted(model2), residuals(model2),
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")

library(ggplot2)




##################################################

#######size for plots
large_text_theme <- theme(
  text = element_text(size = 12),        
  plot.title = element_text(size = 12),  
  axis.title = element_text(size = 12),  
  axis.text = element_text(size = 10)  ,
  axis.title.x = element_text(margin = margin(t = 20)),
  axis.title.y.left =   element_text(margin = margin(t = -20)),
  legend.position = "none"
)


#################################### t tests for choice




#################################### t test für response time
#test large temporal small spatial vs small spatiotemporal
t.test(agg_data_r[agg_data_r$condition == 0,]$mean_rTime, agg_data_r[agg_data_r$condition == 1,]$mean_rTime,paired = TRUE)
#test large temporal small spatial vs large spatiotemporal
t.test(agg_data_r[agg_data_r$condition == 0,]$mean_rTime, agg_data_r[agg_data_r$condition == 2,]$mean_rTime,paired = TRUE)
#test small spatiotemporal vs large spatiotemporal
t.test(agg_data_r[agg_data_r$condition == 1,]$mean_rTime, agg_data_r[agg_data_r$condition == 2,]$mean_rTime,paired = TRUE)







#########figure 3

a<-ggplot(plot_agg_r, aes(x = as.factor(condition), y = as.numeric(mean_antwort))) +
  stat_summary(fun = mean, geom = "bar", fill = "white", color = "black") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.4) +
  labs(y = "Proportion of '1' responses", x = "Condition") +
  labs(title = "",
       x = "", y = "% faster movement was longer in distance\n") +
  #ylab(expression(paste("% 'temporally shorter \n distance was longer'"))) +
  geom_hline(yintercept = 0.5, linetype = "dotted", col = "black")+
  geom_hline(yintercept = 0.58, linetype = "dashed", col = "red")+
  #stat_summary(fun = mean, geom = "line", aes(group = subj), color = "gray80", alpha = 0.5) +
  geom_jitter(aes(color = as.factor(condition)), width = 0.3, alpha = 0.5, size = 1.5) + 
  theme_minimal()+
  scale_x_discrete(labels = custom_labels)+
  theme(legend.position = "None")+
  scale_color_manual(values = c("0" = "purple", "1" = "green3", "2" = "skyblue")) +
  large_text_theme

##########figure 4


b<-ggplot(plot_agg_r, aes(x = as.factor(condition), y = as.numeric(mean_rTime))) +
  stat_summary(fun = mean, geom = "bar", fill = "white", color = "black") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.4) +
  labs(y = "Proportion of '1' responses", x = "Condition") +
  labs(title = "",
       x = "spatiotemporal scale", y = "response time [s]\n") +
  geom_jitter(aes(color = as.factor(condition)), width = 0.3, alpha = 0.5, size = 1.5)+
  theme_minimal()+
  scale_x_discrete(labels = custom_labels)+
  theme(legend.position = "None")+
  scale_color_manual(values = c("0" = "purple", "1" = "green3", "2" = "skyblue")) + 
  large_text_theme
(a+b) + plot_layout(ncol = 1)
