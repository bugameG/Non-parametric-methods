# LOADING PACKAGES
library(tidyverse)
library(DescTools)
library(data.table)


# EXERCISE 9.1 (Sign Test).)
battery <- c(9.8, 10.2, 9.9, 10.5, 9.7, 
             10.1, 9.6, 10.3, 9.8, 10.4, 
             9.9, 10.0, 9.5, 10.2, 9.8)

## The Sign Test
### H_0 = battery lifetime has a median of 10hrs
### H_1 = battery lifetime has a median different from 10 hrs
battery_test <- binom.test(x = sum(battery > 10),
           n = sum(battery != 10),
           p = 0.5,
           alternative = "two.sided", conf.level = 0.95)
#^^^ p-value = 0.7905273
#^^^ Decision: We fail to reject the null hypothesis. 

## 95% confidence interval for the median number of hours
battery_95ci <- SignTest(x = battery,
                         alternative = "two.sided",
                         mu = 10, conf.level = 0.95)

#^^^ (9.8hrs,10.2hrs)

## Interpret your results
#^^^ The median battery life of the new product is 10 hours at a significance level of 5%. 
#^^^ Further, we are 95% confident that the median battery life in hours lies between 9.8 and 10.2 hours.


# EXERCISE 9.2 (Wilcoxon Signed-Rank Test).)
## Systolic blood pressure
bp <- data.table("before" = c(145,138,152,140,148,135,150,142,146,138,144,140),
                 "after" = c(138,132,144,138,142,130,142,136,140,132,138,134))

#^^^ Difference column
bp$difference <- bp$before-bp$after

## Wilcoxon-signed rank test
### H_0 = Blood pressure difference = 0
### H_1 = Blood pressure difference > 0
bp_test <- wilcox.test(x = bp$difference,
                       alternative = "greater",
                       mu = 0, conf.level = 0.95,
                       exact = TRUE # exact handles the ties present 
                       )

#^^^ p-value: 0.001005306
#^^^ Decision: We reject the null hypothesis in favor of the alternative hypothesis

## Hodges-Lehmann estimator of the median reduction
bp_hl <- HodgesLehmann(x = bp$before, y = bp$after)

#^^^HodgesLehmann estimator is 6.0 

## 95% Confidence interval for the median reduction
bp_95ci <- wilcox.test(x = bp$difference,
                       mu = 0, 
                       conf.int = TRUE, conf.level = 0.95)

#^^^ (5.499, 6.999)

## t-test vs. WSR
### paired t-test
bp_test2 <- t.test(x = bp$before,
                   y = bp$after, 
                   alternative = "greater",
                   paired = TRUE,
                   mu = 0,
                   conf.level = 0.95)

#^^^ Likewise,a paired t-test returns a small p-value less than 0.05.
#^^^ The data presented for the sysytolic blood pressure comes from a very small sample size of 12 patients. 
#^^^ Due to this, a non-parametric procedure (WSR) would be preferred.

# ----------------------------------------------

# EXERCISE 9.3 (Comparison of Methods)
## Data generation from familiar distributions, n=20
### a.  X ~ N(0,1)
norm <- rnorm(n = 20, mean = 0, sd = sqrt(1))

### b. Cauchy
cau <- rcauchy(n = 20, location = 0, scale = 1)

### c. X~Exp(1)
expo <- rexp(n = 20, rate = 1) # mean = 1
expo2 <- expo - log(2)

# ---------------------------------------------
  
# EXERCISE 9.4 (Hodges-Lehmann Estimator)
x <- c(3.2, 4.1, 2.8, 3.9, 4.5, 3.7, 2.9, 4.2, 3.5, 3.8)

## Compute the sample mean and median
x_bar <- mean(x, na.rm = TRUE); m_bar <- median(x, na.rm = TRUE);cat(paste("sample mean:", x_bar,"\n","sample median:", m_bar))

## Walsh averages and the Hodges-Lehmann estimator
w <- outer(X = x, Y = x, FUN = function(a,b){(a+b)/2}) # repeated Walsh averages
w_values <- w[upper.tri(x, diag = TRUE)] |> sort() # Unique and ordered Walsh averages
x_hl <- HodgesLehmann(x = w_values); cat(paste("Hodges-Lehmann estimator:", x_hl)) # Hodges-Lehmann estimator

## Add an outlier of 20 to the dataset and recompute all three estimators
x2 <- append(x, 20)

### Compute the sample mean and median
x2_bar <- mean(x2, na.rm = TRUE); m2_bar <- median(x2, na.rm = TRUE);cat(paste("sample mean:", x2_bar,"\n","sample median:", m2_bar))

### Walsh averages and the Hodges-Lehmann estimator
w2 <- outer(X = x2, Y = x2, FUN = function(a,b){(a+b)/2}) # repeated Walsh averages
w2_values <- w2[upper.tri(x2, diag = TRUE)] |> sort() # Unique and ordered Walsh averages
x2_hl <- HodgesLehmann(x = w2_values); cat(paste("Hodges-Lehmann estimator:", x2_hl)) # Hodges-Lehmann estimator

#^^^ Estimator robustness table
estimators <- data.frame(
  "first"=c(x_bar, m_bar, x_hl),
  "second"=c(x2_bar, m2_bar, x2_hl),
  row.names = c("sample mean", "sample median", "Hodges-Lehmann estimator")
);estimators$difference <- estimators$second-estimators$first;estimators

#^^^ The Hodges-Lehmann estimator and the sample median experience the least change when the outlier value is added to the initial dataset.
#^^^ On the other hand the sample mean is exhibits the largest change when the new value is added. 
#^^^ This shows that both the Hodges-Lehmann estimator and the sample median are significantly more robust to outliers
#^^^ as compared to the sample mean 


# EXERCISE 9.5 (Paired Data Application)
# An agricultural experiment tests two varieties
# of wheat. Ten plots are each split into two halves, with one variety planted in each half.
# Yields (bushels per acre):
agric <- data.table("plot" = seq(1,10,1),
                    "a" = c(42,38,45,40,43,41,39,44,42,40),
                    "b" = c(40,37,44,39,42,40,38,43,41,38))






