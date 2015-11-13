#!/usr/bin/env RScript

library(data.table)

temp <- read.csv('temperature_experiment.csv')
temp.t <- data.table(temp)

temp.t[, list(mean_rating=mean(rating), sd_rating=sd(rating)), by=zone]
#    zone     mean        sd
# 1:    2 2.337368 0.7038635
# 2:    3 2.044231 0.8112433

all_means <- temp.t[, list(mean_rating=mean(rating), sd_rating=sd(rating)), by=degrees][order(degrees)]
all_means
#    degrees mean_rating sd_rating
# 1:      20    1.774286 0.7830344
# 2:      21    1.637097 0.7293796
# 3:      22    2.507353 0.6127598
# 4:      23    2.681944 0.4819829
# 5:      24    2.347297 0.6467811

means_zone <- temp.t[, list(mean_rating=mean(rating), sd_rating=sd(rating)), by=c('zone', 'degrees')][order(degrees, zone)]
means_zone
#     zone degrees mean_rating sd_rating
#  1:    2      20    2.063158 0.8173198
#  2:    3      20    1.431250 0.5974599
#  3:    2      21    1.953333 0.7998809
#  4:    3      21    1.340625 0.5225638
#  5:    2      22    2.858333 0.3011009
#  6:    3      22    2.112500 0.6396614
#  7:    2      23    2.663636 0.4392043
#  8:    3      23    2.710714 0.5589064
#  9:    2      24    2.071429 0.6181770
# 10:    3      24    2.709375 0.4980692

means_mornaft <- temp.t[, list(mean_rating=mean(rating), sd_rating=sd(rating)), by=c('morning_afternoon', 'degrees')][order(degrees, morning_afternoon)]
means_mornaft
#     morning_afternoon degrees mean_rating sd_rating
#  1:         afternoon      20    1.788889 0.8266793
#  2:           morning      20    1.758824 0.7591598
#  3:         afternoon      21    1.735294 0.7305316
#  4:           morning      21    1.517857 0.7368350
#  5:         afternoon      22    2.596667 0.6443343
#  6:           morning      22    2.436842 0.5946152
#  7:         afternoon      23    2.697222 0.4538956
#  8:           morning      23    2.666667 0.5213106
#  9:         afternoon      24    2.334211 0.7509351
# 10:           morning      24    2.361111 0.5370276

all_means[all_means$mean_rating == max(all_means[, mean_rating]), ]
#    degrees mean_rating sd_rating
# 1:      23    2.681944 0.4819829

means_zone[means_zone$zone==2 & means_zone$mean_rating == max(means_zone[means_zone$zone==2, mean_rating]), ]
#    zone degrees mean_rating sd_rating
# 1:    2      22    2.858333 0.3011009

means_zone[means_zone$zone==3 & means_zone$mean_rating == max(means_zone[means_zone$zone==3, mean_rating]), ]
#    zone degrees mean_rating sd_rating
# 1:    3      23    2.710714 0.5589064

means_mornaft[means_mornaft$morning_afternoon=='morning' & means_mornaft$mean_rating == max(means_mornaft[means_mornaft$morning_afternoon=='morning', mean_rating]), ]
#    morning_afternoon degrees mean_rating sd_rating
# 1:           morning      23    2.666667 0.5213106

means_mornaft[means_mornaft$morning_afternoon=='afternoon' & means_mornaft$mean_rating == max(means_mornaft[means_mornaft$morning_afternoon=='afternoon', mean_rating]), ]
#    morning_afternoon degrees mean_rating sd_rating
# 1:         afternoon      23    2.697222 0.4538956

lm1 <- lm(rating ~ zone, data=temp)
summary(lm1)
# Call:
# lm(formula = rating ~ zone, data = temp)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.33737 -0.54423 -0.04423  0.66263  0.95577 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.9236     0.2882  10.146   <2e-16 ***
# zone         -0.2931     0.1152  -2.544   0.0118 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.7541 on 171 degrees of freedom
# Multiple R-squared:  0.03647,   Adjusted R-squared:  0.03083 
# F-statistic: 6.472 on 1 and 171 DF,  p-value: 0.01184

lm2 <- lm(rating ~ morning_afternoon, data=temp)
summary(lm2)
# Call:
# lm(formula = rating ~ morning_afternoon, data = temp)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -1.2247 -0.6855 -0.1747  0.7753  0.8145 

# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               2.22471    0.08234  27.019   <2e-16 ***
# morning_afternoonmorning -0.03925    0.11678  -0.336    0.737    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.768 on 171 degrees of freedom
# Multiple R-squared:  0.0006601, Adjusted R-squared:  -0.005184 
# F-statistic: 0.1129 on 1 and 171 DF,  p-value: 0.7372

lm4 <- lm(rating ~ morning_afternoon + degrees + zone, data=temp)
summary(lm4)
# Call:
# lm(formula = rating ~ morning_afternoon + degrees + zone, data = temp)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.75995 -0.60229  0.00608  0.56360  1.34019 

# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              -1.71264    0.87170  -1.965   0.0511 .  
# morning_afternoonmorning -0.05752    0.10586  -0.543   0.5876    
# degrees                   0.20853    0.03699   5.638 7.08e-08 ***
# zone                     -0.26603    0.10649  -2.498   0.0134 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.6955 on 169 degrees of freedom
# Multiple R-squared:   0.19, Adjusted R-squared:  0.1756 
# F-statistic: 13.21 on 3 and 169 DF,  p-value: 8.637e-08




