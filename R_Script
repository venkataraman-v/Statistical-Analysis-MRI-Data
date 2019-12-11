
### Exploratory Analysis and Data Visualization
## Loading Packages
library(ggplot2)
library(cowplot) 
library(scales)

## Reading Dataset
MRI_Data <- read.csv(file="oasis_cross-sectional.csv",header=TRUE)

## Rows 416 to 436 consists of MRI data for the same people already in the test. This was taken for testing reliability.
MRI_Data <- MRI_Data[c(1:416),];
MRI_Data <- MRI_Data[MRI_Data$eTIV<1818,]

# eTIV 
summary(MRI_Data$eTIV)
mean(MRI_Data$eTIV)
median(MRI_Data$eTIV)
max(MRI_Data$eTIV)
min(MRI_Data$eTIV)
sd(MRI_Data$eTIV)

# nWBV
summary(MRI_Data$nWBV)
mean(MRI_Data$nWBV)
median(MRI_Data$nWBV)
max(MRI_Data$nWBV)
min(MRI_Data$nWBV)


# Age Classification
MRI_Data_Young <- MRI_Data[MRI_Data$Age<45,]
MRI_Data_Middle <- MRI_Data[MRI_Data$Age<66 & MRI_Data$Age>44 ,]
MRI_Data_Older <- MRI_Data[MRI_Data$Age>65,]

# Gender Classification
MRI_Data_M <- subset(MRI_Data,MRI_Data$M.F== "M")
MRI_Data_F <- subset(MRI_Data,MRI_Data$M.F== "F")
n_male=length(MRI_Data_M$ID)
n_female=length(MRI_Data_F$ID)

# eTIV for F
summary(MRI_Data_F$eTIV)
mean(MRI_Data_F$eTIV)
median(MRI_Data_F$eTIV)
max(MRI_Data_F$eTIV)
min(MRI_Data_F$eTIV)
sd(MRI_Data_F$eTIV)
# eTIV for M
summary(MRI_Data_M$eTIV)
mean(MRI_Data_M$eTIV)
median(MRI_Data_M$eTIV)
max(MRI_Data_M$eTIV)
min(MRI_Data_M$eTIV)
sd(MRI_Data_M$eTIV)


## Exploratory Analysis and Data Visualization

# Box Plot of three different age groups in terms of eTIV---------------------1
boxplot(MRI_Data_Young$eTIV,MRI_Data_Middle$eTIV,MRI_Data_Older$eTIV,
        main="Descriptive plot of eTIV for 3 different Age Group",
        xlab = "Age Group", names=c("Young","Middle","Older"),
        ylab = "Estimated TIV", font.lab=3,
        notch = TRUE,col=c("red","blue", "yellow")) 


# Histogram eTIV for Male and Female ----------------------------- 2
p1<-ggplot(MRI_Data,aes(eTIV))+
  geom_histogram(binwidth=30,aes(fill=M.F),alpha=0.6,color="black")+
  labs(title="Histogram for eTIV", x="eTIV", y="Frequency")+
  scale_fill_manual(values = c("green","blue"))
p3<-p1+guides(fill=FALSE)
p2<-p1+facet_grid(M.F ~ .)
plot_grid(p3,p2,labels="AUTO")


## A Q-Q plot of nWBV--------------------3
qqnorm(MRI_Data$nWBV,
       col="blue",
       ylab="Normalized Whole Brain Volume",
       main="Normal Q-Q Plot of all the subject's nWBV ")
qqline(MRI_Data$nWBV,col="blue")

## Pie Chart of Male and Female Ratio------------4
df <- data.frame(
  group = c("Male", "Female"),
  value = c(n_male,n_female)
)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
pie <- bp + coord_polar("y", start=0)
pie + scale_fill_grey() +  blank_theme + labs(title="Pie Chart of Subject Gender")+
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]), 
                label = (value)), size=5)


### Statistical Analysis
## ONE SAMPLE T-TEST
## Loading Packages
library(ggplot2)
library(cowplot) 

## Reading Dataset
MRI_Data <- read.csv(file="oasis_cross-sectional.csv",header=TRUE)

## Rows 416 to 436 consists of MRI data for the same people already in the test. This was taken for testing reliability.
MRI_Data <- MRI_Data[c(1:416),];
MRI_Data <- MRI_Data[MRI_Data$eTIV<1818,]

head(MRI_Data)

## Creating a Subset for two Groups "Male" and "Female"
MRI_Data_M <- subset(MRI_Data,MRI_Data$M.F== "M")
MRI_Data_F <- subset(MRI_Data,MRI_Data$M.F== "F")

# Verifying Normality
qqnorm(MRI_Data$eTIV,main="Normal Q-Q plot of sample eTIV",ylab="eTIV",col="red")
qqline(MRI_Data$eTIV,col="blue")

# the parts of the test statistic
# sample mean
x_bar <- mean(MRI_Data$eTIV)
# null hypothesized population mean
mu_0 <- 1500
# sample st. dev
s <- sd(MRI_Data$eTIV)
# sample size
n <- length(MRI_Data$eTIV)
# t-test test statistic
t <- (x_bar - mu_0)/(s/sqrt(n))
# two-sided p-value so multiply by 2
two_sided_t_pval <- pt(q = t, df = n-1, lower.tail = TRUE)*2
two_sided_t_pval

## Critical Values
qt(0.025, n-1)
qt(0.975, n-1)

# lower bound
x_bar+(qt(0.025, n-1)*(s/sqrt(n)))
# upper bound
x_bar+(qt(0.975, n-1)*(s/sqrt(n))) 

## Verifying with Welch Test - Sanity Check
t.test(MRI_Data$eTIV,
       alternative = "two.sided",
       mu = 1500)

# Histogram of Distribution of Sample Statistic
hist(MRI_Data$eTIV,
     xlab='Sample Means i.e Sample Statistic',
     main="Distribution of Sample Statistic") 



## Bootstrap
# Number of Simulations with seed = 0
set.seed(0)
num_sims <- 10000

# A vector to store my results
results <- rep(NA, num_sims)

# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(sample(x =MRI_Data$eTIV,
                            size = n,
                            replace = TRUE))
}

# Finally plot the results
hist(results,freq = FALSE, main='Sampling Distribution of the Sample Mean',
     xlab = 'Average Estimated Total Intracranial Volume',
     ylab = 'Density', ylim=c(0,0.056),)

# estimate a normal curve over it - this looks pretty good!
lines(x = seq(1400, 1500, 1),
      dnorm(seq(1400, 1500, 1),
            mean = x_bar,
            sd = 158.343744/sqrt(416)))


# Shift the sample so that the null hypothesis is true
etiv_given_H0_true <- MRI_Data$eTIV + mean(MRI_Data$eTIV) - mu_0

# Simulations to get accurate result with seed = 0
set.seed(0)
num_sims <- 10000


# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)

# A loop for completing the simulation
for(i in 1:num_sims){
  results_given_H0_true[i] <- mean(sample(x = etiv_given_H0_true,
                                          size = n,
                                          replace = TRUE))
}

# Finally plot the results
hist(results_given_H0_true, freq = FALSE,
     main='Sampling Distribution of the Sample Mean, when Null is True',
     xlab = 'Average eTIV ', ylab = 'Density')

# add line to show values more extreme on upper end
abline(v=x_bar, col = "red")

# add line to show values more extreme on lower end
low_end_extreme <- mean(results_given_H0_true)+(mean(results_given_H0_true)-x_bar)
abline(v=low_end_extreme, col="red")

# counts of values more extreme than the test statistic in our original sample,given H0is true
# two sided given the alternate hypothesis
count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= low_end_extreme)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true >= x_bar)
bootstrap_pvalue <- (count_of_more_extreme_lower_tail + count_of_more_extreme_upper_tail)/num_sims
bootstrap_pvalue

# two sided t p-value
two_sided_t_pval

# need the standard error which is the standard deviation of the results
bootstrap_SE_X_bar <- sd(results)
# an estimate is to use the formula statistic +/- 2*SE
c(x_bar - 2*bootstrap_SE_X_bar, x_bar + 2*bootstrap_SE_X_bar)

# you can also use the 5th and 95th quantiles to determine the bounds: 
c(quantile(results, c(.025, .975)))

# compare to our t-methods
c(x_bar+(qt(0.025, n-1)*(s/sqrt(n))), x_bar+(qt(0.975, n-1)*(s/sqrt(n))))



## ONE SAMPLE TEST OF PROPORTIONS
## Loading Packages
library(ggplot2)
library(cowplot) 

## Reading Dataset
MRI_Data <- read.csv(file="oasis_cross-sectional.csv",header=TRUE)

## Rows 407 to 436 consists of MRI data for the same people already in the test. This was taken for testing reliability.
MRI_Data <- MRI_Data[c(1:407),]
MRI_Data <- MRI_Data[MRI_Data$eTIV<1818,]

head(MRI_Data)

## Creating a Subset for two Groups "Male" and "Female"
MRI_Data_M <- subset(MRI_Data,MRI_Data$M.F== "M")
MRI_Data_F <- subset(MRI_Data,MRI_Data$M.F== "F")
length(MRI_Data_M$ID)

n_m <-length(MRI_Data_M$ID)
n_f <-length(MRI_Data_F$ID)

p<- n_f/((n_m)+(n_f))
p

## Test Stat

## p_O = 0.5.. Null Hypothesis that the experiment was done with equal number of male and female
z <- (p - .5) / sqrt((.5*(1-.5)) / 407)
z

# One Sided Lower Tail
binom.test(x=256, n = 407, p=(.5), alternative="less")

pvalue<-pnorm(z, lower.tail = TRUE)
pvalue

binom.test(x=256, n = 461, p=(.5), alternative="less")$conf.int

cat("normal approx")
c(0,p - (qnorm(p))*sqrt(((p)*(1-p))/407))

## Bootstrap

people <- factor(rep(c("Female", "Male"), c(256, 407-256)))
people

table(people)

people <- rep(c(1, 0), c(256, 407-256))
people

# Number of Simulations
num_sims <- 10000

# A vector to store my results
results <- rep(NA, num_sims)

# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(sample(x = people,
                            size = 407,
                            replace = TRUE))
}

# Finally plot the results
hist(results, freq = FALSE,ylim=c(0,20),
     main='Sampling Distribution of the Sample Proportion',
     xlab = 'Proportion of Femlaes', ylab = 'Density')
# estimate a normal curve over it - this looks pretty good!
lines(x = seq(.45, 3.85, .001),
      dnorm(seq(.45, 3.85, .001),
            mean = mean(results), sd = sd(results)))

cat("Bootstrap Confidence Interval")
c(quantile(results, c(0,.95)))
cat("exact binomial test")
binom.test(x=256, n = 461, p=(.5), alternative="less")$conf.int
cat("normal approx")
c(0,p - (qnorm(p))*sqrt(((p)*(1-p))/407))


# Under the assumption that the null hypothesis is true, we have 50% females
people <- rep(c(1, 0), c(407/2, 407/2))
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(sample(x = people,
                            size = 407,
                            replace = TRUE))
}

# Finally plot the results
hist(results, freq = FALSE,ylim=c(0,20),xlim=c(0.4,0.63),
     main='Sampling Distribution of the Sample Proportion under H_0:p=0.5',
     xlab = 'Proportion of Females', ylab = 'Density')

# estimate a normal curve over it - this looks pretty good!
lines(x = seq(.40, 9.85, .001),
      dnorm(seq(.40,9.85, .001),
            mean = mean(results), sd = sd(results)))
abline(v=p, col="red")

## Bootstrap p-value 
count_of_more_extreme_lower_tail <- sum(results <= p)
bootstrap_pvalue <- count_of_more_extreme_lower_tail/num_sims
bootstrap_pvalue
## Comparing with traditional
binom.test(x=256, n = 407, p=(.5), alternative="less")$p.value
pnorm(z, lower.tail = TRUE)



## TWO SAMPLE T-TEST FOR DIFFERENCE IN MEANS
## Loading Packages
library(ggplot2)
library(cowplot) 

## Reading Dataset
MRI_Data <- read.csv(file="oasis_cross-sectional.csv",header=TRUE)

## Rows 416 to 436 consists of MRI data for the same people already in the test. This was taken for testing reliability.
MRI_Data <- MRI_Data[c(1:416),]
MRI_Data <- MRI_Data[MRI_Data$eTIV<1818,]
head(MRI_Data)
boxplot(MRI_Data$eTIV)
## Creating a Subset for two Groups "Male" and "Female"
MRI_Data_M <- subset(MRI_Data,MRI_Data$M.F== "M")
MRI_Data_F <- subset(MRI_Data,MRI_Data$M.F== "F")

# Statistical Analysis

# Checking Normality
qqnorm(MRI_Data$eTIV,
       col="green",
       ylab="Estimated Total Intracranial Volume",
       main="Normal Q-Q Plot from the subjects")
qqline(MRI_Data$eTIV,col="blue")
qqnorm(MRI_Data_M$eTIV,
       col="red",
       ylab="Estimated Total Intracranial Volume",
       main="Normal Q-Q Plot for Males")
qqline(MRI_Data_M$eTIV,col="blue")
qqnorm(MRI_Data_F$eTIV,
       col="blue",
       ylab="Estimated Total Intracranial Volume",
       main="Normal Q-Q Plot for Females")
qqline(MRI_Data_F$eTIV,col="red")

# Sample Means of the two Categories 
x_bar_n <- mean(MRI_Data_M$eTIV)
x_bar_s <- mean(MRI_Data_F$eTIV)
# Null Hypothesized population mean difference 
mu_0 <- 0
# Sample Variances
s_n_sq <- sd(MRI_Data_M$eTIV)**2
s_s_sq <- sd(MRI_Data_F$eTIV)**2
# Sample Size
n_n <-length(MRI_Data_M$eTIV)
n_s <- length(MRI_Data_F$eTIV)


# T-Test Statistic
t <- (x_bar_n - x_bar_s - mu_0)/sqrt((s_n_sq/n_n) + (s_s_sq/n_s))
t
# Degrees of Freedom
df = min(n_n, n_s)-1
df
# Two Sided p-value
two_sided_diff_t_pval <- pt(q = t, df = min(n_n, n_s)-1, lower.tail =
                              FALSE)*2
two_sided_diff_t_pval
## Confidence Interval
# Critical Values
qt(0.975, min(n_n, n_s)-1)
qt(0.025, min(n_n, n_s)-1)
# Upper Bound
(x_bar_n-x_bar_s)+(qt(0.975, min(n_n, n_s)-1)*sqrt((s_n_sq/n_n) +
                                                     (s_s_sq/n_s)))
# Lower Bound
(x_bar_n-x_bar_s)+(qt(0.025, min(n_n, n_s)-1)*sqrt((s_n_sq/n_n) +
                                                     (s_s_sq/n_s))) 

# Verifying results with Welch Test
t.test(MRI_Data_M$eTIV,MRI_Data_F$eTIV)

# Histogram of Distribution of Sample Statistic
hist(MRI_Data_M$eTIV-MRI_Data_F$eTIV,
     xlab='Difference in Sample Means i.e Sample Statistic',
     main="Distribution of Sample Statistic") 



# Bootstrap Approach

num_sims <- 10000
results <- rep(NA, num_sims)
for(i in 1:num_sims){
  mean_Male <- mean(sample(x = MRI_Data_M$eTIV,
                           size = 160,
                           replace = TRUE))
  mean_Female <- mean(sample(x = MRI_Data_F$eTIV,
                             size = 256,
                             replace = TRUE))
  results[i] <- mean_Male - mean_Female
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Mean',
     xlab = 'Average Difference in Estimated Intracranial Volume', ylab = 'Density')

# Bootstrap one-sided CI
c(quantile(results, c(.025, .975)))
# compare to our t-methods
t.test(MRI_Data_M$eTIV, MRI_Data_F$eTIV)


# Check out the transform function used to shuffle
transform(MRI_Data, M.F=sample(M.F))
num_sims <- 10000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  # idea here is if there is no relationshipm we should be able to shuffle the groups
  shuffled_gender <- transform(MRI_Data, M.F=sample(M.F))
  mean_Male <- mean(shuffled_gender$eTIV[shuffled_gender$M.F=="M"])
  mean_Female <- mean(shuffled_gender$eTIV[shuffled_gender$M.F=="F"])
  results_given_H0_true[i] <- mean_Male - mean_Female
}

# Finally plot the results
hist(results_given_H0_true, freq = FALSE, xlim=c(-165,165),
     main='Dist. of the Diff in Sample Means Under Null',
     xlab = 'Average Difference in Estimated Intracranial Volume Under Null',
     ylab = 'Density')
diff_in_sample_means <-  mean(MRI_Data_M$eTIV)- mean(MRI_Data_F$eTIV)
abline(v=diff_in_sample_means, col = "blue")
abline(v=abs(diff_in_sample_means), col = "red")


# counts of values more extreme than the test statistic in our original sample, given H0 is true
# two sided given the alternate hypothesis
count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= diff_in_sample_means)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true >= abs(diff_in_sample_means))
bootstrap_pvalue <- ( count_of_more_extreme_upper_tail)/num_sims
cat("Bootstrap p-value")
bootstrap_pvalue



## TWO SAMPLE TEST FOR DIFFERENCE IN PROPORTIONS
## Loading Packages
library(ggplot2)
library(cowplot) 

## Reading Dataset
MRI_Data <- read.csv(file="oasis_cross-sectional.csv",header=TRUE)

## Rows 416 to 436 consists of MRI data for the same people already in the test. This was taken for testing reliability.
MRI_Data <- MRI_Data[c(1:416),]
MRI_Data <- MRI_Data[MRI_Data$eTIV<1818,]
head(MRI_Data)

## Creating a Subset for two Groups "Male" and "Female"
MRI_Data_M <- subset(MRI_Data,MRI_Data$M.F== "M")
MRI_Data_F <- subset(MRI_Data,MRI_Data$M.F== "F")


MRI_Data_Male_WBV <- MRI_Data_M[, c('nWBV')]
MRI_Data_Female_WBV <- MRI_Data_F[, c('nWBV')]


MRI_Data_Male_eTIV <- MRI_Data_M[, c('eTIV')]
MRI_Data_Female_eTIV <- MRI_Data_F[, c('eTIV')]

MRI_Data_Male_WBV <-MRI_Data_Male_WBV * MRI_Data_Male_eTIV
MRI_Data_Female_WBV <-MRI_Data_Female_WBV * MRI_Data_Female_eTIV


# the parts of the test statistic
# sample props
p_hat_m <- sum(MRI_Data_Male_WBV )/sum(MRI_Data_Male_eTIV)
p_hat_f <- sum(MRI_Data_Female_WBV)/sum(MRI_Data_Female_eTIV)
# null hypothesized population prop difference between the two groups
p_0 <- 0
# sample size
n_m <- sum(MRI_Data_Male_eTIV)
n_f <- sum(MRI_Data_Female_eTIV)
# sample variances
den_p_m <- (p_hat_m*(1-p_hat_m))/n_m
den_p_f <- (p_hat_f*(1-p_hat_f))/n_f
# z-test test statistic
z <- (p_hat_m - p_hat_f - p_0)/sqrt(den_p_m + den_p_f)
# two sided p-value
two_sided_diff_prop_pval <- pnorm(q = z, lower.tail = FALSE)*2
two_sided_diff_prop_pval

# lower bound
(p_hat_m - p_hat_f)+(qnorm(0.025)*sqrt(den_p_m + den_p_f))
# upper bound
(p_hat_m - p_hat_f)+(qnorm(0.975)*sqrt(den_p_m + den_p_f))

## Bootstrap Approach

# Make the data
male <- rep(c(1, 0), c(sum(MRI_Data_Male_WBV), n_m -sum(MRI_Data_Male_WBV)))
female <- rep(c(1,0), c(sum(MRI_Data_Female_WBV),n_f - sum(MRI_Data_Female_WBV)))
num_sims <- 1000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  prop_male<- mean(sample(male,
                          size = n_m,
                          replace = TRUE))
  prop_female <- mean(sample(x = female,
                             size = n_f,
                             replace = TRUE))
  results[i] <- prop_male - prop_female
}
# Finally plot the results
hist(results, freq = FALSE, main='Dist. of the Diff in Prop',
     xlab = 'Difference in Prop. of Whole Brain Volume', ylab = 'Density')

cat("Bootstrap")
c(quantile(results, c(.025, .975)))
cat("Normal Approximation")
c((p_hat_m - p_hat_f)+(qnorm(0.025)*sqrt(den_p_m + den_p_f)),(p_hat_m - p_hat_f)+(qnorm(0.975)*sqrt(den_p_m + den_p_f)))

str(male)
str(female)
# Make the data
df_combined <- data.frame("WBV" = c(male,female), "Gender" = rep(c("male", "female"), c(n_m-1, n_f-1)))
# Sanity checks
summary(df_combined$Gender)

num_sims <- 1000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  # idea here is if there is no relationshipm we should be able to shuffle the groups
  shuffled_groups <- transform(df_combined, Gender=sample(Gender))
  prop_male <- mean(shuffled_groups$WBV[shuffled_groups$Gender=="male"])
  prop_female <- mean(shuffled_groups$WBV[shuffled_groups$Gender=="female"])
  results_given_H0_true[i] <- prop_male - prop_female
}

# Finally plot the results
hist(results_given_H0_true, freq = FALSE,
     main='Dist. of the Diff in Sample Sample Props Under Null',
     xlab = 'Average Difference in Prop. of WBV  under Null',
     ylab = 'Density')
diff_in_sample_props <- p_hat_m - p_hat_f
abline(v=diff_in_sample_props, col = "blue")
abline(v=-diff_in_sample_props, col = "red")


# counts of values more extreme than the test statistic in our original sample, given H0 is true
# two sided given the alternate hypothesis
count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= -diff_in_sample_props)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true >= diff_in_sample_props)
bootstrap_pvalue <- (count_of_more_extreme_lower_tail + count_of_more_extreme_upper_tail)/num_sims
cat("Bootstrap p-value")
bootstrap_pvalue



## CHI-SQUARE GOODNESS OF FIT TEST
## Loading Packages
library(ggplot2)
library(cowplot) 

## Reading Dataset
MRI_Data <- read.csv(file="oasis_cross-sectional.csv",header=TRUE)

## Rows 416 to 436 consists of MRI data for the same people already in the test. This was taken for testing reliability.
MRI_Data <- MRI_Data[c(1:416),]
MRI_Data <- MRI_Data[MRI_Data$eTIV<1818,]
MRI_Data_Young <- MRI_Data[MRI_Data$Age<45,]
MRI_Data_Middle <- MRI_Data[MRI_Data$Age<66 & MRI_Data$Age>44 ,]
MRI_Data_Older <- MRI_Data[MRI_Data$Age>65,]
Young<-length(MRI_Data_Young$ID)
Middle<-length(MRI_Data_Middle$ID)
Older<-length(MRI_Data_Older$ID)
AgeTab<-rep(c("Young","Middle", "Older"),c(Young,Middle,Older))
table(AgeTab)
prop.table(table(AgeTab))


chisq<-sum(((table(AgeTab) - (407/3))^2)/(407/3))
pchisq(chisq, df = 3-1, lower.tail = FALSE)

# Create our data under the assumption that H_0 is true
solutions_under_H_0 <- rep(c("Young", "Middle", "Older"), (407/3))
# Sanity Check
table(solutions_under_H_0)

num_sims <- 10000
# A vector to store my results
chisq_stats_under_H0 <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  new_samp <- sample(solutions_under_H_0, 407, replace = T)
  chisq_stats_under_H0[i] <- sum(((table(new_samp) - (407/3))^2)/(407/3))
}

# TO be run separetely as it may cause conflict with previous histogram
hist(chisq_stats_under_H0, freq = FALSE,xlim=c(0,45),
     main='Dist. of the Chi-Square Statistic Under Null',
     xlab = 'Chi-Square Stat under Null',
     ylab = 'Density')
abline(v=sum(((table(AgeTab) - (407/3))^2)/(407/3)), col="red")

## Randomization p-value
sum(chisq_stats_under_H0 >= sum(((table(AgeTab) - (407/3))^2)/(407/3)))/num_sims

