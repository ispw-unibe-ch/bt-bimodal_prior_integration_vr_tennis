#---------------------------------------------------------------
# Author: Damian Beck
# Date: June 2024
# Based on r version 4.3.2
#---------------------------------------------------------------
##bimodal prior integration in VR-Tennis 
##according to Bayesian decision theory

#Multilevel regression analysis (or synonym "linear mixed models") 
#of the horizontal differences between the ball and 
#the racket center (variable = “horizontal_difference”)
#as a function of ball position (variable = “ball_position”) 
#and uncertainty-condition (variable = “condition” slow/moderate/fast)

#The script is structured as follows:
#1. Load necessary packages and functions
#2. load data and make dummy variable for 
#left and right side of bimodal distribution
#3. Quantify non valid trials (no hits and backswing)
#4. Filter data by condition to analyse each condition separately
#5. Delete outliers detected with cooks distance, an outlier is defined
#as more than 3 times more influential than an average point
#6. Perform multilevel regression analysis as the assumption of independence 
#of residuals is violated. Therefor procedure according to Andy Field (2012)
#for each condition separately. Start  to build up the models according
#model complexity with an intercept only model, one predictor model, random
#intercept model, random slope model, additional dummy predictor left_right
#7. Check for collinearity, normality of residuals and homoscedasticity
#8. Model comparison with anova (according to the parsimony principle)
#9. Plot for all participants "estimation error" as a function of "ball_position"
#10. Create bins and calculate mean and 95% CI across all subjects for each condition
#11. Plot the mean and 95% confidence intervals for each bin across all subjects
#for each condition together in one single plot

# Packages ----
#---------------------------------------------------------------
#install.packages("car", dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("nlme", dependencies = TRUE)
#install.packages("tidyverse", dependencies = TRUE)
#install.packages("sjPlot", dependencies = TRUE)
#install.packages("broom.mixed", dependencies = TRUE)
#install.packages("psych", dependencies = TRUE)
#install.packages("MASS", dependencies = TRUE)

library(car)
library(ggplot2)
library(nlme)
library(tidyverse)
library(sjPlot)
library(broom.mixed)
library(psych)
library(MASS)

#function to calculate VIF in mixed models (same as car::vif())
vif.lme <- function (mod) 
{
  if (any(is.na(fixef(mod)))) 
    stop("there are aliased coefficients in the model")
  v <- vcov(mod)
  mm <- model.matrix(formula(mod), mod$data)
  assign <- attributes(mm)$assign
  if (names(fixef(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  }
  else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) 
    stop("model contains fewer than 2 terms")
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, 
                                                                       -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) 
    result <- result[, 1]
  else result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  result
}

# Import data ----
#---------------------------------------------------------------
data_all <- read.csv("data/output_day3.csv")
View(data_all)
summary(data_all)

#Make dummy variable for left and right side of bimodal distribution
data_all$left_right <- ifelse(data_all$ball_position > 70, 1,0)

# Non valid trials ----
#---------------------------------------------------------------
# overall number of non valid trials
# each day 24 subjects * 480 trials = 288 trials = 11520 trials (without warm-up)
# No hit trials = 11520 - length(data_all)
number_of_no_hits <- 11520 - dim(data_all)[1]
percentage_of_no_hits <- number_of_no_hits / 11520 * 100
percentage_of_no_hits #percentage of no hits is 8.37

# exclude data for backswing true
number_of_backswing_hits <- data_all %>% filter(backswing == "True")
dim(number_of_backswing_hits)[1]
percentage_of_backswing_hits <- dim(number_of_backswing_hits)[1] / 11520 * 100
percentage_of_backswing_hits #percentage of backswing hits is 1.55%

# Filter data ----
#---------------------------------------------------------------
# exclude data with backswing true
data_all <- data_all %>% filter(backswing == "False")

# Filter data by condition
fast <- filter(data_all, data_all$condition == "fast")
summary(fast)
moderate <- filter(data_all, data_all$condition == "moderate")
summary(moderate)
slow <- filter(data_all, data_all$condition == "slow")
summary(slow)

#Make a list of number of subjects
subjects <- unique(data_all$subject)

#Analyse each condition separately
#Fast ---- 
#---------------------------------------------------------------
#Outlier detection with cooks distance 
#if more than 3 times more influential than an average point
model_fast <- lm(horizontal_difference ~ ball_position+ left_right, data = fast)
summary(model_fast)
tab_model(model_fast)
plot(model_fast,4)
cd <- cooks.distance(model_fast)
influential <- cd[(cd > (3 * mean(cd, na.rm = TRUE)))]
names_of_outlier <- names(influential)
outliers <- fast[names_of_outlier,]
fast <- fast %>% anti_join(outliers)
summary(fast)

#Instructions according to Andy Field for the multilevel regression analysis
#Field, A. P., Miles, J., & Field, Z. (2012). Discovering statistics using 
#R: And sex and drugs and rock ‘n’ roll. London: Sage.
#intercept only model
model1_fast <- nlme::gls(horizontal_difference ~ 1,
                         data = fast,
                         method = "ML",
                         na.action = na.exclude)
tab_model(model1_fast)

#One predictor model
model2_fast <- nlme::gls(horizontal_difference ~ ball_position, 
                         data = fast,
                         method = "ML",
                         na.action = na.exclude)
tab_model(model2_fast)
summary(model2_fast)

#random intercept model
model3_fast <- nlme::lme(horizontal_difference ~ ball_position, 
                         data = fast,
                         random = ~ 1|subject,
                         method = "ML",
                         na.action = na.exclude)
tab_model(model3_fast)
summary(model3_fast)

#random slope model
model4_fast <- nlme::lme(horizontal_difference ~ ball_position, 
                         data = fast,
                         random = ~ ball_position|subject,
                         method = "ML",
                         na.action = na.exclude)
tab_model(model4_fast)
summary(model4_fast)

#additional dummy predictor left_right
model5_fast <- nlme::lme(horizontal_difference ~ ball_position + left_right, 
                         data = fast,
                         random = ~ ball_position|subject,
                         method = "ML",
                         na.action = na.exclude)
tab_model(model5_fast)
summary(model5_fast)

#check collinearity
#calculate correlation between ball_position and left_right))
cor.test((fast$ball_position), (fast$left_right)) 
#high correlation not necessarily a problem, check the variance inflation factor vif
car::vif(model5_fast)
vif.lme(model5_fast) #according to the vif, there is only little variance inflation

#check for normality of residuals
plot(residuals(model5_fast))
qqnorm(residuals(model5_fast))
qqline(residuals(model5_fast))
hist(residuals(model5_fast)) #residuals are visually normally distributed
#simple size to large for shapiro test
#shapiro.test(residuals(model5_fast)) 

#Model comparison
anova(model1_fast, model2_fast, model3_fast, model4_fast, model5_fast)

#plot individuals with a for loop from 1 to 24
for (i in 1:24) {
  individual_data <- filter(fast, subject==levels(as.factor(data_all$subject))[i])
  title <- paste("subject", i, "in the fast condition")
  i_plot <- ggplot(individual_data, aes(x = ball_position, y = horizontal_difference)) +
    geom_point() +  # This adds the scatter points
    labs(title = title,
         x = "ball positions (cm)",
         y = "estimation error [horizontal ball position - racket center] (cm)") +
    theme_minimal() +
    geom_segment(aes(x = 40,y = coef(model5_fast)[i,1]+ 40*coef(model5_fast)[i,2], xend = 60, yend = coef(model5_fast)[i,1]+ 60*coef(model5_fast)[i,2]), size = 2, alpha = 0.5,  linetype = "solid") +
    geom_segment(aes(x = 80,y = coef(model5_fast)[i,1]+ 80*coef(model5_fast)[i,2]+coef(model5_fast)[i,3], xend = 100, yend = coef(model5_fast)[i,1]+ 100*coef(model5_fast)[i,2]+coef(model5_fast)[i,3]), size = 2, alpha = 0.5,  linetype = "solid") +
    geom_segment(aes(x = 60,y = coef(model5_fast)[i,1]+ 60*coef(model5_fast)[i,2], xend = 70, yend = coef(model5_fast)[i,1]+ 70*coef(model5_fast)[i,2]), size = 0.5, alpha = 0.5,  linetype = "dotted") +
    geom_segment(aes(x = 70,y = coef(model5_fast)[i,1]+ 70*coef(model5_fast)[i,2]+coef(model5_fast)[i,3], xend = 80, yend = coef(model5_fast)[i,1]+ 80*coef(model5_fast)[i,2]+coef(model5_fast)[i,3]), size = 0.5, alpha = 0.5,  linetype = "dotted") +
    theme(legend.position = "none")
  path <- file.path("plots/individual_plots_day3/fast", paste(as.character(i),"fast_plot.png"))
  ggsave(path, i_plot, width = 9, height = 6, units = "cm")
}

#Moderate ----
#---------------------------------------------------------------
#Outlier detection with cooks distance 
#if more than 3 times more influential than an average point
model_moderate <- lm(horizontal_difference ~ ball_position+ left_right, data = moderate)
summary(model_moderate)
tab_model(model_moderate)
plot(model_moderate,4)
cd <- cooks.distance(model_moderate)
influential <- cd[(cd > (3 * mean(cd, na.rm = TRUE)))]
names_of_outlier <- names(influential)
outliers <- moderate[names_of_outlier,]
moderate <- moderate %>% anti_join(outliers)
summary(moderate)

#Instructions according to Andy Field for the multilevel regression analysis
#Field, A. P., Miles, J., & Field, Z. (2012). Discovering statistics using 
#R: And sex and drugs and rock ‘n’ roll. London: Sage.
#intercept only model
model1_moderate <- nlme::gls(horizontal_difference ~ 1,
                             data = moderate,
                             method = "ML",
                             na.action = na.exclude)
tab_model(model1_moderate)

#One predictor model
model2_moderate <- nlme::gls(horizontal_difference ~ ball_position, 
                             data = moderate,
                             method = "ML",
                             na.action = na.exclude)
tab_model(model2_moderate)
summary(model2_moderate)

#random intercept model
model3_moderate <- nlme::lme(horizontal_difference ~ ball_position, 
                             data = moderate,
                             random = ~ 1|subject,
                             method = "ML",
                             na.action = na.exclude)
tab_model(model3_moderate)
summary(model3_moderate)

#random slope model
model4_moderate <- nlme::lme(horizontal_difference ~ ball_position, 
                             data = moderate,
                             random = ~ ball_position|subject,
                             method = "ML",
                             na.action = na.exclude)
tab_model(model4_moderate)
summary(model4_moderate)

#additional dummy predictor left_right
model5_moderate <- nlme::lme(horizontal_difference ~ ball_position + left_right, 
                             data = moderate,
                             random = ~ ball_position|subject,
                             method = "ML",
                             na.action = na.exclude)
tab_model(model5_moderate)
summary(model5_moderate)

#check collinearity
#calculate correlation between ball_position and left_right))
cor.test((moderate$ball_position), (moderate$left_right)) 
#high correlation not necessarily a problem, check the variance inflation factor vif
car::vif(model5_moderate)
vif.lme(model5_moderate) #according to the vif, there is only little variance inflation

#check for normality of residuals
plot(residuals(model5_moderate))
qqnorm(residuals(model5_moderate))
qqline(residuals(model5_moderate))
hist(residuals(model5_moderate)) #residuals are visually normally distributed
#simple size to large for shapiro test
#shapiro.test(residuals(model5_moderate)) 

#Model comparison
anova(model1_moderate, model2_moderate, model3_moderate, model4_moderate, model5_moderate)

#plot individuals with a for loop from 1 to 24
for (i in 1:24) {
  individual_data <- filter(moderate, subject==levels(as.factor(data_all$subject))[i])
  title <- paste("subject", i, "in the moderate condition")
  i_plot <- ggplot(individual_data, aes(x = ball_position, y = horizontal_difference)) +
    geom_point() +  # This adds the scatter points
    labs(title = title,
         x = "ball positions (cm)",
         y = "estimation error [horizontal ball position - racket center] (cm)") +
    theme_minimal() +
    geom_segment(aes(x = 40,y = coef(model5_moderate)[i,1]+ 40*coef(model5_moderate)[i,2], xend = 60, yend = coef(model5_moderate)[i,1]+ 60*coef(model5_moderate)[i,2]), size = 2, alpha = 0.5,  linetype = "solid") +
    geom_segment(aes(x = 80,y = coef(model5_moderate)[i,1]+ 80*coef(model5_moderate)[i,2]+coef(model5_moderate)[i,3], xend = 100, yend = coef(model5_moderate)[i,1]+ 100*coef(model5_moderate)[i,2]+coef(model5_moderate)[i,3]), size = 2, alpha = 0.5,  linetype = "solid") +
    geom_segment(aes(x = 60,y = coef(model5_moderate)[i,1]+ 60*coef(model5_moderate)[i,2], xend = 70, yend = coef(model5_moderate)[i,1]+ 70*coef(model5_moderate)[i,2]), size = 0.5, alpha = 0.5,  linetype = "dotted") +
    geom_segment(aes(x = 70,y = coef(model5_moderate)[i,1]+ 70*coef(model5_moderate)[i,2]+coef(model5_moderate)[i,3], xend = 80, yend = coef(model5_moderate)[i,1]+ 80*coef(model5_moderate)[i,2]+coef(model5_moderate)[i,3]), size = 0.5, alpha = 0.5,  linetype = "dotted") +
    theme(legend.position = "none")
  path <- file.path("plots/individual_plots_day3/moderate", paste(as.character(i),"moderate_plot_day3.png"))
  ggsave(path, i_plot, width = 9, height = 6, units = "cm")
}

#Slow ----
#---------------------------------------------------------------
#Outlier detection with cooks distance 
#if more than 3 times more influential than an average point
model_slow <- lm(horizontal_difference ~ ball_position+ left_right, data = slow)
summary(model_slow)
tab_model(model_slow)
plot(model_slow,4)
cd <- cooks.distance(model_slow)
influential <- cd[(cd > (3 * mean(cd, na.rm = TRUE)))]
names_of_outlier <- names(influential)
outliers <- slow[names_of_outlier,]
slow <- slow %>% anti_join(outliers)
summary(slow)

#Instructions according to Andy Field for the multilevel regression analysis
#Field, A. P., Miles, J., & Field, Z. (2012). Discovering statistics using 
#R: And sex and drugs and rock ‘n’ roll. London: Sage.
#intercept only model
model1_slow <- nlme::gls(horizontal_difference ~ 1,
                         data = slow,
                         method = "ML",
                         na.action = na.exclude)
tab_model(model1_slow)

#One predictor model
model2_slow <- nlme::gls(horizontal_difference ~ ball_position, 
                         data = slow,
                         method = "ML",
                         na.action = na.exclude)
tab_model(model2_slow)
summary(model2_slow)

#random intercept model
model3_slow <- nlme::lme(horizontal_difference ~ ball_position, 
                         data = slow,
                         random = ~ 1|subject,
                         method = "ML",
                         na.action = na.exclude)
tab_model(model3_slow)
summary(model3_slow)

#random slope model
model4_slow <- nlme::lme(horizontal_difference ~ ball_position, 
                         data = slow,
                         random = ~ ball_position|subject,
                         method = "ML",
                         na.action = na.exclude)
tab_model(model4_slow)
summary(model4_slow)

#additional dummy predictor left_right
model5_slow <- nlme::lme(horizontal_difference ~ ball_position + left_right, 
                         data = slow,
                         random = ~ ball_position|subject,
                         method = "ML",
                         na.action = na.exclude)
tab_model(model5_slow)
summary(model5_slow)

#check collinearity
#calculate correlation between ball_position and left_right))
cor.test((slow$ball_position), (slow$left_right)) 
#high correlation not necessarily a problem, check the variance inflation factor vif
car::vif(model5_slow)
vif.lme(model5_slow) #according to the vif, there is only little variance inflation

#check for normality of residuals
plot(residuals(model5_slow))
qqnorm(residuals(model5_slow))
qqline(residuals(model5_slow))
hist(residuals(model5_slow)) #residuals are visually normally distributed
#simple size to large for shapiro test
#shapiro.test(residuals(model5_slow)) 


#Model comparison
anova(model1_slow, model2_slow, model3_slow, model4_slow, model5_slow)

#calculate a robust model because of potential small violations of normality assumptions of residuals
#robust model left_right not signicificant with less effect
robust_model_left_right <- rlm(horizontal_difference ~ ball_position + left_right, data = slow)
summary(robust_model_left_right)
tab_model(robust_model_left_right)

#plot individuals with a for loop from 1 to 24
for (i in 1:24) {
  individual_data <- filter(slow, subject==levels(as.factor(data_all$subject))[i])
  title <- paste("subject", i, "in the slow condition")
  i_plot <- ggplot(individual_data, aes(x = ball_position, y = horizontal_difference)) +
    geom_point() +  # This adds the scatter points
    labs(title = title,
         x = "ball positions (cm)",
         y = "estimation error [horizontal ball position - racket center] (cm)") +
    theme_minimal() +
    geom_segment(aes(x = 40,y = coef(model5_slow)[i,1]+ 40*coef(model5_slow)[i,2], xend = 60, yend = coef(model5_slow)[i,1]+ 60*coef(model5_slow)[i,2]), size = 2, alpha = 0.5,  linetype = "solid") +
    geom_segment(aes(x = 80,y = coef(model5_slow)[i,1]+ 80*coef(model5_slow)[i,2]+coef(model5_slow)[i,3], xend = 100, yend = coef(model5_slow)[i,1]+ 100*coef(model5_slow)[i,2]+coef(model5_slow)[i,3]), size = 2, alpha = 0.5,  linetype = "solid") +
    geom_segment(aes(x = 60,y = coef(model5_slow)[i,1]+ 60*coef(model5_slow)[i,2], xend = 70, yend = coef(model5_slow)[i,1]+ 70*coef(model5_slow)[i,2]), size = 0.5, alpha = 0.5,  linetype = "dotted") +
    geom_segment(aes(x = 70,y = coef(model5_slow)[i,1]+ 70*coef(model5_slow)[i,2]+coef(model5_slow)[i,3], xend = 80, yend = coef(model5_slow)[i,1]+ 80*coef(model5_slow)[i,2]+coef(model5_slow)[i,3]), size = 0.5, alpha = 0.5,  linetype = "dotted") +
    theme(legend.position = "none")
  path <- file.path("plots/individual_plots_day3/slow", paste(as.character(i),"slow_plot.png"))
  ggsave(path, i_plot, width = 9, height = 6, units = "cm")
}





#plot bins ----
#---------------------------------------------------------------
#Function to create bins and calculate mean and 95% CI across all subjects
create_bins_with_stats_all_subjects <- function(data) {
  bin_ranges <- list(
    c(40, 42), c(42, 44), c(44, 46), c(46, 48),
    c(48, 50), c(50, 52), c(52, 54), c(54, 56),
    c(56, 58), c(58, 60), c(80, 82), c(82, 84),
    c(84, 86), c(86, 88), c(88, 90), c(90, 92),
    c(92, 94), c(94, 96), c(96, 98), c(98, 100)
  )
  
  subject_bins <- data.frame(bin = integer(), mean = numeric(), lower = numeric(), upper = numeric())
  
  for (j in seq_along(bin_ranges)) {
    bin_range <- bin_ranges[[j]]
    bin_data <- data %>% filter(ball_position >= bin_range[1] & ball_position < bin_range[2])
    
    if (nrow(bin_data) > 0) {
      #bin_mean <- mean(bin_data$horizontal_difference, na.rm = TRUE)
      #bin_se <- sd(bin_data$horizontal_difference, na.rm = TRUE) / sqrt(nrow(bin_data))
      #bin_ci <- qt(0.975, df = nrow(bin_data) - 1) * bin_se
      # Fit a random intercept model
      model <- lme(horizontal_difference ~ 1, random = ~ 1 | subject, data = bin_data, na.action = na.omit)
      
      # Extract the fixed effect (mean) estimate
      bin_mean <- fixef(model)
      
      # Extract the standard error of the fixed effect
      se_estimate <- summary(model)$tTable[,"Std.Error"]
      
      # Calculate the 95% confidence interval
      bin_ci <- qt(0.975, df = model$fixDF$X) * se_estimate
      
      if (j < 11) {
        bin_center <- 39 + 2 * j
      } else {
        bin_center <- 59 + 2 * j
      }
      
      subject_bins <- rbind(subject_bins, data.frame(
        bin = bin_center,
        mean = bin_mean,
        lower = bin_mean - bin_ci,
        upper = bin_mean + bin_ci
      ))
    }
  }
  
  return(subject_bins)
}

# Create bins and calculate stats across all subjects
bins_with_stats_all_subjects_fast <- create_bins_with_stats_all_subjects(fast)
bins_with_stats_all_subjects_moderate <- create_bins_with_stats_all_subjects(moderate)
bins_with_stats_all_subjects_slow <- create_bins_with_stats_all_subjects(slow)




# Plot the mean and 95% confidence intervals for each bin across all subjects
#No interaction effect
p <- ggplot() +
  
  # Fast data points and error bars
  geom_point(data = bins_with_stats_all_subjects_fast, aes(x = bin, y = mean, color = "Fast")) +
  geom_errorbar(data = bins_with_stats_all_subjects_fast, aes(x = bin, ymin = lower, ymax = upper, color = "Fast"), width = 0.2) +
  
  # Moderate data points and error bars
  geom_point(data = bins_with_stats_all_subjects_moderate, aes(x = bin, y = mean, color = "Moderate")) +
  geom_errorbar(data = bins_with_stats_all_subjects_moderate, aes(x = bin, ymin = lower, ymax = upper, color = "Moderate"), width = 0.2) +
  
  # Slow data points and error bars
  geom_point(data = bins_with_stats_all_subjects_slow, aes(x = bin, y = mean, color = "Slow")) +
  geom_errorbar(data = bins_with_stats_all_subjects_slow, aes(x = bin, ymin = lower, ymax = upper, color = "Slow"), width = 0.2) +
  
  # Add segments for fast
  geom_segment(aes(x = 40,y = fixef(model5_fast)[1]+ 40*fixef(model5_fast)[2], xend = 60, yend = fixef(model5_fast)[1]+ 60*fixef(model5_fast)[2]), size = 2, color = "red", linetype = "solid") +
  geom_segment(aes(x = 80,y = fixef(model5_fast)[1]+ 80*fixef(model5_fast)[2]+fixef(model5_fast)[3], xend = 100, yend = fixef(model5_fast)[1]+ 100*fixef(model5_fast)[2]+fixef(model5_fast)[3]), size = 2, color = "red", linetype = "solid") +
  geom_segment(aes(x = 60,y = fixef(model5_fast)[1]+ 60*fixef(model5_fast)[2], xend = 70, yend = fixef(model5_fast)[1]+ 70*fixef(model5_fast)[2]), size = 2, alpha = 0.5,  color = "red", linetype = "dotted") +
  geom_segment(aes(x = 70,y = fixef(model5_fast)[1]+ 70*fixef(model5_fast)[2]+fixef(model5_fast)[3], xend = 80, yend = fixef(model5_fast)[1]+ 80*fixef(model5_fast)[2]+fixef(model5_fast)[3]), size = 2, color = "red", linetype = "dotted") +
  # Add similar segments for moderate
  geom_segment(aes(x = 40, y = fixef(model5_moderate)[1] + 40 * fixef(model5_moderate)[2], xend = 60, yend = fixef(model5_moderate)[1] + 60 * fixef(model5_moderate)[2]), size = 2, color = "blue", linetype = "solid") +
  geom_segment(aes(x = 80, y = fixef(model5_moderate)[1] + 80 * fixef(model5_moderate)[2] + fixef(model5_moderate)[3], xend = 100, yend = fixef(model5_moderate)[1] + 100 * fixef(model5_moderate)[2] + fixef(model5_moderate)[3]), size = 2, color = "blue", linetype = "solid") +
  geom_segment(aes(x = 60, y = fixef(model5_moderate)[1] + 60 * fixef(model5_moderate)[2], xend = 70, yend = fixef(model5_moderate)[1] + 70 * fixef(model5_moderate)[2]), size = 2, alpha = 0.5, color = "blue", linetype = "dotted") +
  geom_segment(aes(x = 70, y = fixef(model5_moderate)[1] + 70 * fixef(model5_moderate)[2] + fixef(model5_moderate)[3], xend = 80, yend = fixef(model5_moderate)[1] + 80 * fixef(model5_moderate)[2] + fixef(model5_moderate)[3]), size = 2, color = "blue", linetype = "dotted") +
  # Add similar segments for slow
  geom_segment(aes(x = 40, y = fixef(model5_slow)[1] + 40 * fixef(model5_slow)[2], xend = 60, yend = fixef(model5_slow)[1] + 60 * fixef(model5_slow)[2]), size = 2, color = "green", linetype = "solid") +
  geom_segment(aes(x = 80, y = fixef(model5_slow)[1] + 80 * fixef(model5_slow)[2] + fixef(model5_slow)[3], xend = 100, yend = fixef(model5_slow)[1] + 100 * fixef(model5_slow)[2] + fixef(model5_slow)[3]), size = 2, color = "green", linetype = "solid") +
  geom_segment(aes(x = 60, y = fixef(model5_slow)[1] + 60 * fixef(model5_slow)[2], xend = 70, yend = fixef(model5_slow)[1] + 70 * fixef(model5_slow)[2]), size = 2, alpha = 0.5, color = "green", linetype = "dotted") +
  geom_segment(aes(x = 70, y = fixef(model5_slow)[1] + 70 * fixef(model5_slow)[2] + fixef(model5_slow)[3], xend = 80, yend = fixef(model5_slow)[1] + 80 * fixef(model5_slow)[2] + fixef(model5_slow)[3]), size = 2, color = "green", linetype = "dotted") +
  
  # Color scale for legend
  scale_color_manual(name = "", 
                     values = c("Fast" = "red", "Moderate" = "blue", "Slow" = "green")) +
  
  # Set y-axis limits
  ylim(-20, 20) +  # Add this line to set y-axis range
  
  # Labels and theme
  labs(
    title = "",
    x = "Ball Position (cm)",
    y = "Estimation Error [Horizontal Ball Position - Racket Center] (cm)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",   
    panel.grid = element_blank(),  
    axis.line = element_line(linewidth = 1),    
    axis.ticks = element_line(linewidth = 1),
    text = element_text(size = 26),         
    axis.text = element_text(size = 26, face = "plain", color = "black"),    
    axis.title = element_text(size = 26, face = "plain", color = "black"),   
    legend.text = element_text(size = 26, margin = margin(b = 15)),  
    legend.title = element_text(size = 26, face = "plain", color = "black"),
    legend.spacing.x = unit(1, "cm"),   
    legend.key.width = unit(1.5, "cm")
  )
p

# Save the plot
ggsave(filename = "plots/all_subjects_day3.png", plot = p, width = 15, height = 13)
# Save as vector graphic
ggsave(filename = "plots/all_subjects_day3.svg", plot = p, width = 15, height = 13)




# Define the plot with bidirectional arrows at x=70
p <- ggplot() +
  
  # Fast data points and error bars
  geom_point(data = bins_with_stats_all_subjects_fast, aes(x = bin, y = mean, color = "Fast")) +
  geom_errorbar(data = bins_with_stats_all_subjects_fast, aes(x = bin, ymin = lower, ymax = upper, color = "Fast"), width = 0.2) +
  
  # Moderate data points and error bars
  geom_point(data = bins_with_stats_all_subjects_moderate, aes(x = bin, y = mean, color = "Moderate")) +
  geom_errorbar(data = bins_with_stats_all_subjects_moderate, aes(x = bin, ymin = lower, ymax = upper, color = "Moderate"), width = 0.2) +
  
  # Slow data points and error bars
  geom_point(data = bins_with_stats_all_subjects_slow, aes(x = bin, y = mean, color = "Slow")) +
  geom_errorbar(data = bins_with_stats_all_subjects_slow, aes(x = bin, ymin = lower, ymax = upper, color = "Slow"), width = 0.2) +
  
  # Add segments for fast
  geom_segment(aes(x = 40, y = fixef(model5_fast)[1] + 40 * fixef(model5_fast)[2], xend = 60, yend = fixef(model5_fast)[1] + 60 * fixef(model5_fast)[2]), size = 2, color = "red", linetype = "solid") +
  geom_segment(aes(x = 80, y = fixef(model5_fast)[1] + 80 * fixef(model5_fast)[2] + fixef(model5_fast)[3], xend = 100, yend = fixef(model5_fast)[1] + 100 * fixef(model5_fast)[2] + fixef(model5_fast)[3]), size = 2, color = "red", linetype = "solid") +
  geom_segment(aes(x = 60, y = fixef(model5_fast)[1] + 60 * fixef(model5_fast)[2], xend = 70, yend = fixef(model5_fast)[1] + 70 * fixef(model5_fast)[2]), size = 2, alpha = 0.5, color = "red", linetype = "dotted") +
  geom_segment(aes(x = 70, y = fixef(model5_fast)[1] + 70 * fixef(model5_fast)[2] + fixef(model5_fast)[3], xend = 80, yend = fixef(model5_fast)[1] + 80 * fixef(model5_fast)[2] + fixef(model5_fast)[3]), size = 2, color = "red", linetype = "dotted") +
  
  # Bidirectional arrow connecting fast regression lines at x=70
  geom_segment(aes(x = 70, y = fixef(model5_fast)[1] + 70 * fixef(model5_fast)[2], xend = 70, yend = fixef(model5_fast)[1] + 70 * fixef(model5_fast)[2] + fixef(model5_fast)[3]), 
               color = "red", arrow = arrow(type = "closed", ends = "both", length = unit(0.3, "cm"))) +
  
  # Add similar segments for moderate
  geom_segment(aes(x = 40, y = fixef(model5_moderate)[1] + 40 * fixef(model5_moderate)[2], xend = 60, yend = fixef(model5_moderate)[1] + 60 * fixef(model5_moderate)[2]), size = 2, color = "blue", linetype = "solid") +
  geom_segment(aes(x = 80, y = fixef(model5_moderate)[1] + 80 * fixef(model5_moderate)[2] + fixef(model5_moderate)[3], xend = 100, yend = fixef(model5_moderate)[1] + 100 * fixef(model5_moderate)[2] + fixef(model5_moderate)[3]), size = 2, color = "blue", linetype = "solid") +
  geom_segment(aes(x = 60, y = fixef(model5_moderate)[1] + 60 * fixef(model5_moderate)[2], xend = 70, yend = fixef(model5_moderate)[1] + 70 * fixef(model5_moderate)[2]), size = 2, alpha = 0.5, color = "blue", linetype = "dotted") +
  geom_segment(aes(x = 70, y = fixef(model5_moderate)[1] + 70 * fixef(model5_moderate)[2] + fixef(model5_moderate)[3], xend = 80, yend = fixef(model5_moderate)[1] + 80 * fixef(model5_moderate)[2] + fixef(model5_moderate)[3]), size = 2, color = "blue", linetype = "dotted") +
  
  # Bidirectional arrow connecting moderate regression lines at x=70
  geom_segment(aes(x = 70, y = fixef(model5_moderate)[1] + 70 * fixef(model5_moderate)[2], xend = 70, yend = fixef(model5_moderate)[1] + 70 * fixef(model5_moderate)[2] + fixef(model5_moderate)[3]), 
               color = "blue", arrow = arrow(type = "closed", ends = "both", length = unit(0.3, "cm"))) +
  
  # Add similar segments for slow
  geom_segment(aes(x = 40, y = fixef(model5_slow)[1] + 40 * fixef(model5_slow)[2], xend = 60, yend = fixef(model5_slow)[1] + 60 * fixef(model5_slow)[2]), size = 2, color = "green", linetype = "solid") +
  geom_segment(aes(x = 80, y = fixef(model5_slow)[1] + 80 * fixef(model5_slow)[2] + fixef(model5_slow)[3], xend = 100, yend = fixef(model5_slow)[1] + 100 * fixef(model5_slow)[2] + fixef(model5_slow)[3]), size = 2, color = "green", linetype = "solid") +
  geom_segment(aes(x = 60, y = fixef(model5_slow)[1] + 60 * fixef(model5_slow)[2], xend = 70, yend = fixef(model5_slow)[1] + 70 * fixef(model5_slow)[2]), size = 2, alpha = 0.5, color = "green", linetype = "dotted") +
  geom_segment(aes(x = 70, y = fixef(model5_slow)[1] + 70 * fixef(model5_slow)[2] + fixef(model5_slow)[3], xend = 80, yend = fixef(model5_slow)[1] + 80 * fixef(model5_slow)[2] + fixef(model5_slow)[3]), size = 2, color = "green", linetype = "dotted") +
  
  # Bidirectional arrow connecting slow regression lines at x=70
  geom_segment(aes(x = 70, y = fixef(model5_slow)[1] + 70 * fixef(model5_slow)[2], xend = 70, yend = fixef(model5_slow)[1] + 70 * fixef(model5_slow)[2] + fixef(model5_slow)[3]), 
               color = "green", arrow = arrow(type = "closed", ends = "both", length = unit(0.3, "cm"))) +
  
  # Color scale for legend
  scale_color_manual(name = "", values = c("Fast" = "red", "Moderate" = "blue", "Slow" = "green")) +
  
  # Set y-axis limits
  ylim(-17, 17) +
  
  # Labels and theme
  labs(
    title = "",
    x = "Ball Position (cm)",
    y = "Estimation Error (cm)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",   
    panel.grid = element_blank(),  
    axis.line = element_line(linewidth = 1),    
    axis.ticks = element_line(linewidth = 1),
    text = element_text(size = 30),         
    axis.text = element_text(size = 30, face = "plain", color = "black"),    
    axis.title = element_text(size = 30, face = "plain", color = "black"),   
    legend.text = element_text(size = 30, margin = margin(b = 15)),  
    legend.title = element_text(size = 30, face = "plain", color = "black"),
    legend.spacing.x = unit(1, "cm"),   
    legend.key.width = unit(1.5, "cm")
  )

# Display and save the plot
p
ggsave(filename = "plots/all_subjects_day3.png", plot = p, width = 17, height = 13)
ggsave(filename = "plots/all_subjects_day3.svg", plot = p, width = 17, height = 13)



