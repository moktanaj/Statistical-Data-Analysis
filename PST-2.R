library(tidyverse)
#setting the source directory
setwd("~/Desktop/MXN500/PST-2")

############################ 1.1

#downloading the soi_data 
soi_data <- read_csv("soi_data_wide.csv")
View(soi_data)
#converting to the longer format.
soi_data <-soi_data %>% pivot_longer( !Year, names_to = "Month",
                                      values_to = "Value")
View(soi_data)
head(soi_data,3)
class(soi_data$Month)
#############################1.2

library(ggplot2)
soi_data$Month = factor(soi_data$Month, levels = month.abb)
ggplot(soi_data, aes(x=Year, y=Value, fill = Month))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, outlier.size=1)+
  facet_wrap(~Month)+
  theme_bw()

##########################1.3

# Categorizing Seasons
summer_months = c("Dec", "Jan", "Feb")
spring_months = c("Sep", "Oct", "Nov")
autumn_months = c("Mar", "Apr", "May")
winter_months = c("Jun", "Jul", "Aug")

#Creating "soi_data" with season column.
soi_data <- soi_data %>%
  mutate(Season = NA_character_) %>%
  mutate(Season = if_else(Month %in% summer_months, "Summer", Season)) %>% 
  mutate(Season = if_else(Month %in% spring_months, "Spring", Season)) %>% 
  mutate(Season = if_else(Month %in% autum_months, "Autumn", Season)) %>% 
  mutate(Season = if_else(Month %in% winter_months, "Winter", Season))

#using case_when() function.
# soi_data <- soi_data %>% mutate(Season = case_when(
#   Month %in% summer_months ~"Summer",
#   Month %in% spring_months ~ "Spring",
#   Month %in% autum_months ~ "Autum",
#   Month %in% winter_months ~ "Winter") )
                          
View(soi_data)

# Code to show the rows corresponding to Year 2020.
soi_data %>% filter(Year == 2020 )

##########################1.4

#Creating data set "seasonal_soi_data"
rm(seasonal_soi_data)
# seasonal_soi_data <- group_by(soi_data,Year,Season) %>%
#   summarise(SeasonalSOI=mean(Value))

seasonal_soi_data <- soi_data %>%
  group_by(Year, Season) %>%
  summarise(Mean = mean(Value, na.rm =TRUE)) %>%
  ungroup() %>%
  transmute(Year = Year, Season = Season, SeasonalSOI = Mean) # Adding
#columns SeasonalSOI and assigning mean of each season of the each year and 
# Keeping Year and Season column as well.

head(seasonal_soi_data)  
View(seasonal_soi_data)
# Printing out the rows corresponding to Year 2020

seasonal_soi_data %>% filter(Year == 2020)


##########################1.5

seasonal_soi_data <- seasonal_soi_data %>% 
  mutate(Phase = "Neutral") %>%
  mutate(Phase = if_else(SeasonalSOI > 8, "LaNina", Phase)) %>% 
  mutate(Phase = if_else(SeasonalSOI < -8, "ElNino", Phase))
head(seasonal_soi_data, 9)

# Updated rows corresponding to Year 2011

seasonal_soi_data %>% filter(Year == 2011)


##########################1.6

#Checking the data type of the variables in "seasonal_soi_data"
str(seasonal_soi_data)
# $ Year       : num [1:584] 1876 1876 1876 1876 1877 ...
# $ Season     : chr [1:584] "Autum" "Spring" "Summer" "Winter" ...
# $ SeasonalSOI: num [1:584] 5.4667 -0.0667 6.4333 7.9667 -3.5667 ...
# $ Phase      : chr [1:584] "Neutral" "Neutral" "Neutral" "Neutral" ...

# The variables are not a factor data type.
# Converting variables into a factor data type.

View(seasonal_soi_data)
seasonal_soi_data$Season <- as.factor(seasonal_soi_data$Season)
seasonal_soi_data$Phase <-as.factor(seasonal_soi_data$Phase)
# seasonal_soi_data$Season <- seasonal_soi_data %>% mutate(Season = factor(Season),
#                         Phase = factor(Phase))
#   
View(seasonal_soi_data)
str(seasonal_soi_data)

# > str(seasonal_soi_data)
# tibble [584 × 4] (S3: tbl_df/tbl/data.frame)
# $ Year       : num [1:584] 1876 1876 1876 1876 1877 ...
# $ Season     : Factor w/ 4 levels "Autumn","Spring",..: 1 2 3 4 1 2 3 4 1 2 ...
# $ SeasonalSOI: num [1:584] 5.4667 -0.0667 6.4333 7.9667 -3.5667 ...
# $ Phase      : Factor w/ 3 levels "ElNino","LaNina",..: 3 3 3 3 3 1 1 1 3 2 ...

#  Hence, Variable Season and Phase are converted into a factor datatype. 
#Season has 4 levels.
# Phase has 3 levels

######################### Section 2: Linear Regression ##############


# rm(total_seasonal_rainfall)
total_seasonal_rainfall <- read_csv("total_seasonal_rainfall.csv") %>%
  mutate(total_seas_prcp = total_seas_prcp/10) %>%
  left_join(seasonal_soi_data)

View(total_seasonal_rainfall)


########################## 2.1

# For the Brisbane Station in Spring, a linear model was specified to model 
# how the total seasonal precipitation, ..yi., is related to the mean seasonal  
# SOI value, ..xi.. The parameter .β1.. describes the rate of change in the 
# total seasonal precipitation with an increase in mean seasonal SOI value. 
# The parameter . β0. . represents the total seasonal precipitation when the mean 
# seasonal SOI value is 0.

########################## 2.2   Linear Model

# creating a dataset "BRO_Spring_rainfall" with the information related to 
# BRISBANE REGIONAL OFFICE Station and Spring season only. 

BRO_Spring_rainfall <- total_seasonal_rainfall %>% 
  filter( Season == "Spring" & name == "BRISBANE REGIONAL OFFICE")

View(BRO_Spring_rainfall)

BRO_Spring_rainfall_lm <- lm(data = BRO_Spring_rainfall,
                                total_seas_prcp ~ SeasonalSOI)
summary(BRO_Spring_rainfall_lm)

# Hence, Based on the parameter estimate, the equation is:

#   Total Seasonal Precipitation = 222.199 + 3.68 * SeasonalSOI + ε 


########################## 2.3

library(broom)
round(glance(BRO_Spring_rainfall_lm)$r.squared,4)
# > round(glance(BRO_Spring_rainfall_model)$r.squared,4)
# [1] 0.0749

# The variability in data explained by the model is 7.49 %

########################## 2.4

tidy(BRO_Spring_rainfall_lm, conf.int = T, conf.level = 0.95) %>%
  select(term, estimate, conf.low, conf.high, p.value)

# A tibble: 2 x 5
# term        estimate conf.low conf.high  p.value
# <chr>          <dbl>    <dbl>     <dbl>    <dbl>
# 1 (Intercept)   222.    197.       247.   5.00e-30
# 2 SeasonalSOI   3.68    0.840      6.52   1.17e- 2

########################## 2.5

# Let's create a data frame using fortify() function from ggplot2 
# that has everything to analyse the residuals.

BRO_Spring_rainfall_lm.fort <- fortify(BRO_Spring_rainfall_lm)
head(BRO_Spring_rainfall_lm.fort)

# total_seas_prcp SeasonalSOI       .hat   .sigma      .cooksd  .fitted     .resid
# 1           215.3   15.100000 0.04818984 114.3712 0.0079992750 277.7602  -62.46024
# 2           168.3   -9.266667 0.02511424 114.5706 0.0003994288 188.1025  -19.80246
# 3           253.5   -3.133333 0.01335298 114.4920 0.0009699179 210.6702   42.82978
# 4           126.3  -10.166667 0.02783457 114.4025 0.0038839100 184.7909  -58.49089
# 5            99.1  -10.966667 0.03046631 114.2112 0.0085544080 181.8473  -82.74726
# 6           108.9   -1.566667 0.01224400 113.9598 0.0055938485 216.4348 -107.53481
# .stdresid
# 1 -0.5621313
# 2 -0.1760968
# 3  0.3785945
# 4 -0.5208677
# 5 -0.7378728
# 6 -0.9500217

# Visualising the fitted value (y[i] hat), .fitted compared with 
# the residuals (epsilon[i]), .resid

ggplot(data = BRO_Spring_rainfall_lm.fort, aes(x = .fitted, y = .resid))+
  geom_point()+
  theme_bw()+
  geom_smooth()+
  labs(x = expression(paste("Fitted (",hat(y[i]), ")")),
       y = expression(paste("Residual (",epsilon[i],")")))

# visualising the standardised quantiles of the residuals 
# compared with the theoretical quantiles.

ggplot(data=BRO_Spring_rainfall_lm.fort, aes(sample=.stdresid)) +
  stat_qq() +
  geom_abline(intercept=0, slope=1) +
  coord_equal()+
  theme_bw()
### validity of the underlying assumptions of linear regression:

# 1. From the fig. 1 (fitted vs. residual )
# The fitted vs. residual is essential in determining the assumption of Linearity
# and homoscedasticity of the model.
# 1.1 Having seen the residuals are not too far away from 0, it suggests that the model 
# complies with linearity. 
# 1.2 Since the residuals are equally spread around the line y=0 and also, there is no any 
# clear pattern seen, hence, homoscedasticity holds true. 
# 2. From fig. 2 (standard residual vs theoretical quantiles)
# it is used to test the assumption of Normality. Since the sample observation lies well 
# along the line of 45-degree, we can say that Normality holds. 

# Hence, it validates the underlying assumption of linear regression.

########################## 2.6

# summary(BRO_Spring_rainfall_lm)
# 
# Call:
#   lm(formula = total_seas_prcp ~ SeasonalSOI, data = BRO_Spring_rainfall)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -188.72  -82.69  -15.65   52.31  471.54 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  222.199     12.427  17.880   <2e-16 ***
#   SeasonalSOI    3.680      1.428   2.578   0.0117 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 113.9 on 82 degrees of freedom
# (26 observations deleted due to missingness)
# Multiple R-squared:  0.07495,	Adjusted R-squared:  0.06367 
# F-statistic: 6.644 on 1 and 82 DF,  p-value: 0.01174

###### Interpretation of the Model 

# for one unit increase in the SeasonaSOI, there is an increase of 3.68 unit in a
# total_seaosnal_rainfall. SeasonalSOI explains 7.49% variability in 
# total_seaosnal_rainfall. When testing the null hypothesis that there is no 
# linear association between SeasonalSOI and total seasonal rainfall, we reject the null 
# hypothesis given by the fact that p-value = 0.0117 < 0.05. 

########################## 2.7

ggplot(total_seasonal_rainfall, aes(x=SeasonalSOI, y=total_seas_prcp))+
  geom_point()+
  facet_wrap(~name + Season, nrow = 7)+
  theme_bw()+
  geom_smooth(method='lm', formula= y~x) +
  labs(y= "Toal seasonal rainfall ( mm )")


########################## 2.8

# In terms of significant linear relationship based on visualisation of 
# total seasonal precipitation and the mean soasonal SOI value, the cities and seasons are:

# 1. BRISBANE REGIONAL OFFICE, Summer. and  
# 2. DARWIN BOTANIC GARDENS, Summer.


########################## 2.9

# 1. All the locations have almost same amount of rainfall recorded However, 
# BRISBANE REGIONAL OFFICE and HOBART BOTANICAL GARDENS seems to have recorded
# higher rainfall. 
# 2. The total seasonal rainfall is significantly responsive to ENSO as we move from 
# El Niño phase to La Niña in BRISBANE REGIONAL OFFICE, Summer and DARWIN BOTANIC GARDENS, Summer. 
# There is a slight response to total seasonal rainfall due to ENSO in the locations like 
# MELBOURNE REGIONAL OFFICE(winter), PERTH REGIONAL OFFICE(winter) and 
# SYDNEY (OBSERVATORY HILL) (winter). 
# However, the rest of the locations with seasons seems to have no effect on total
# seasonal rainfall due to ENSO. 



########### Section 3: Polynomial Lines of Best Fit #########

###### 3.1 

Brisbane_spring_poly <- mutate(BRO_Spring_rainfall, 
                               Log_prcp = log(total_seas_prcp))
view(Brisbane_spring_poly)
Bris_spring_poly_lm <- lm(data = Brisbane_spring_poly, 
                          Log_prcp ~ poly(SeasonalSOI, 2, raw=T))


tidy(Bris_spring_poly_lm) %>% select(term, estimate)

# Output:

# A tibble: 3 x 2
# term                            estimate
# <chr>                              <dbl>
#   1 (Intercept)                    5.26     
# 2 poly(SeasonalSOI, 2, raw = T)1 0.0195   
# 3 poly(SeasonalSOI, 2, raw = T)2 0.0000342

# Hence, the equation is :

log (total_seas_prcp) = 5.26 + 0.0195 * SeasonalSOI + 0.0000342 * SeasonalSOI^2

################ 3.2

summary(Bris_spring_poly_lm)

# Output:

# > summary(Bris_spring_poly_lm)
# 
# Call:
#   lm(formula = Log_prcp ~ poly(SeasonalSOI, 2, raw = T), data = Brisbane_spring_poly)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.47857 -0.30333  0.05821  0.37638  1.42097 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    5.255e+00  7.383e-02  71.176  < 2e-16 ***
#   poly(SeasonalSOI, 2, raw = T)1 1.950e-02  6.950e-03   2.806  0.00627 ** 
#   poly(SeasonalSOI, 2, raw = T)2 3.424e-05  5.666e-04   0.060  0.95196    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5499 on 81 degrees of freedom
# (26 observations deleted due to missingness)
# Multiple R-squared:  0.09044,	Adjusted R-squared:  0.06798 
# F-statistic: 4.027 on 2 and 81 DF,  p-value: 0.02151

# Intrepretation of Result of summary:

# When SeasonalSOI is 0, The log of total rainfall  = 5.255e+00. 
# log of total rainfall(Log_prcp) increases by 1.950e-02 with one unit increase in SeasonalSOI.
# log of total rainfall(Log_prcp) increases by 3.424e-05 with one unit increase in SeasonalSOI square.
# There is 9% variability on log of total rainfall(Log_prcp) predicted by SeasonalSOI.
# p-value: 0.02151 which is less than 0.05 indicates that we can reject the null
# hypothesis which suggests that there is no effect of log of total rainfall (Log_prcp)
# due to SeasonalSOI ( that is beta_one and beta_one are 0)


################ 3.3
install.packages("prediction")
library(prediction)
predict(Bris_spring_poly_lm,data.frame(SeasonalSOI = 25),
        interval = "conf", level = 0.95)
# output:
#   fit      lwr     upr
# 1 5.764111 5.083633 6.44459
# Comment: For mean SeaonalSOI of 25 with confidence interval of 95% , 
# Log_prcp = 5.76411 which is within the range (5.083633 6.44459) of true 
# population parameter of 95%  confidence interval.
predict(Bris_spring_poly_lm,data.frame(SeasonalSOI = -25),
        interval = "conf", level = 0.95)
# output:
#   fit      lwr      upr
# 1 4.788972 4.033551 5.544393

# Comment: For mean SeaonalSOI of -25 with confidence interval of 95% , 
# Log_prcp = 4.788972 which is within the range (4.033551 5.544393) of true 
# population parameter of 95%  confidence interval.

################ 3.4

anova(BRO_Spring_rainfall_lm , Bris_spring_poly_lm)

# Output:
# > anova(BRO_Spring_rainfall_lm , Bris_spring_poly_lm)
# Analysis of Variance Table
# 
# Response: total_seas_prcp
# Df  Sum Sq Mean Sq F value  Pr(>F)  
# SeasonalSOI  1   86176   86176  6.6436 0.01174 *
#   Residuals   82 1063642   12971                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Since p-value: 0.01174 < 0.05, we can reject the null hypothesis that 
# models explain the same amount of variability in data in favour of alternate
# hypothesis that the quadratic model explains the greater deal of variability in data.




###################### Section 4: ######################


# BRO_Spring_rainfall

###### Q 4.1

BRO_Spring_rainfall$Phase <- relevel(BRO_Spring_rainfall$Phase, ref = "ElNino") 

BRO_Spring_rainfall_phase_lm <- lm(data = BRO_Spring_rainfall,
                                   total_seas_prcp ~ Phase)
tidy(BRO_Spring_rainfall_phase_lm)
# Output:

# > tidy(BRO_Spring_rainfall_phase_lm)
# # A tibble: 3 x 5
# term         estimate std.error statistic      p.value
# <chr>           <dbl>     <dbl>     <dbl>        <dbl>
#   1 (Intercept)     181.       29.1      6.23 0.0000000201
# 2 PhaseLaNina      91.4      45.5      2.01 0.0482      
# 3 PhaseNeutral     42.6      32.9      1.30 0.199       


# Our Equation is of the form :

#total_seas_prcp (i) = Beta (0) + Beta (1) I (Phase(i) ==LaNina )+
# Beta (2) I (Phase(i) == Neutral ) + erroe
# Since Phase(i) ==LaNina  is true as it is significantly different from 0 given by  0.0482 < 0.05
# Phase(i) == Neutral is false as its not significantly different from 0 given by  0.1985 > 0.05
# Hence, The equation based on the model is :

# mean total_seas_prcp = 180.94 + 91.36 


###### Q 4.2

summary(BRO_Spring_rainfall_phase_lm)

# > summary(BRO_Spring_rainfall_phase_lm)
# 
# Call:
#   lm(formula = total_seas_prcp ~ Phase, data = BRO_Spring_rainfall)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -167.48  -81.98   -7.64   47.05  476.86 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    180.94      29.07   6.225 2.01e-08 ***
#   PhaseLaNina     91.36      45.54   2.006   0.0482 *  
#   PhaseNeutral    42.64      32.89   1.296   0.1985    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 116.3 on 81 degrees of freedom
# (26 observations deleted due to missingness)
# Multiple R-squared:  0.04778,	Adjusted R-squared:  0.02427 
# F-statistic: 2.032 on 2 and 81 DF,  p-value: 0.1377 

# Interpetation of Result:


# intercept = 180.94 is the mean total_seas_prcp of ElNino Phase.
# PhaseLaNina = 91.36 is the sum of mean of total_seas_prcp of ElNino Phase and LaNina phase.
# PhaseNeutral = 42.64 is the sum of mean of total_seas_prcp of ElNino Phase and Neutral phase.
# P-value of baseline (2.01e-08) and the LaNina phase (0.0482) are less than 0.05 which indicates that the beta 0 and beta 1 are not zero.
# However, the P-value of Neutral pahse 0.1985 > 0.05 indicating beta 2 equals to zero.
# on ElNino Phase is significantly different from 0.
# on LaNina phase is significantly different from ElNino.
# on Neutral phase is not significantly different from ElNino.

























