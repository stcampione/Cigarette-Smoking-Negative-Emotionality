## Cigarette Smoking and Neuroticism Personality Factor

data <- read.csv("MIDUS_Refresher.csv", header = TRUE)

#############################################
################### Setup ###################
#############################################

install.packages("tidyverse", "qqplotr")
library(tidyverse)
library(qqplotr)

participants <- data %>%
  filter(complete.cases(data$B1SNEURO) &
           complete.cases(data$B1PA39))

#############################################
################ Hypotheses #################
#############################################

#H0: Neuroticism scores of smokers and nonsmokers are the same
#H1: Neuroticism scores of smokers are higher than scores of nonsmokers


#############################################
########## Descriptive Statistics ###########
#############################################


##### Neuroticism Personality Trait (B1SNEURO)
table(participants$B1SNEURO)
#Neuro mean
mean(participants$B1SNEURO, na.rm = T)
#2.068262
#Neuro SD
sd(participants$B1SNEURO)
#0.6149801

#Histogram of Neuroticism Scores
Neuroticism <- participants$B1SNEURO
hist(Neuroticism, breaks = 10)



##### Cigarette Smoking (B1PA39)
table(participants$B1PA39)
length(participants$B1PA39)
#Relative frequency: Smokers
smokers.relfreq <- 131 / 481
#0.2723493
#Relative frequency: Non-Smokers
nonsmokers.relfeq <- 350 / 481
#0.7276507



##### AGE (B1PAGE_M2)
age <- data$B1PAGE_M2
table(age)
mean(age, na.rm = T)
summary(age)
#Mean age: 55.92
#Range: [34,83]
#Median: 55



##### GENDER (B1PGENDER)
table(data$B1PGENDER)
#Relative frequency: men
men.relfreq <- 154 / 360
#0.4277778
#Relative frequency: women
women.relfreq <- 206 / 360
#0.5722222



##### RACE (B1PF7A)
table(data$B1PF7A)
sum(data$B1PF7A, na.rm = T)
#1 White: 0.8660944
#2 African American: 0.06609442
#3 Native American or Alaskan Native Aleutian Islander/Eskimo: 0.01802575
#4 Asian: 0.01030043
#6 Other: 0.03519313
#7 don't know & #8 refused: .0.004291845

race <- as.factor(data$B1PF7A)
binaryrace = revalue(race,
                     c(
                       "1" = "1",
                       "2" = "2",
                       "3" = "2",
                       "4" = "2",
                       "6" = "2",
                       "7" = "2",
                       "8" = "2"
                     ))
table(binaryrace)
#Relative frequency: White
white.relfreq <- 1009 / 1165
#0.8660944
#Relative frequency: Non-White
non_white.relfreq <- 156 / 1165
#0.1339056



##### TOTAL HOUSEHOLD INCOME (B1STINC1)
income <- data$B1STINC1
table(income)
summary(income)
#Mean: 70115
#Range: [0, 300000]
#Q1: 26250
#Q3: 92500
#Median: 52750



##### Neuroticism Scores of Smokers
neuro_smokes <- participants$B1SNEURO[participants$B1PA39 == 1]
mean_neuro_smokes <- mean(neuro_smokes)
sd(neuro_smokes)

##### Neuroticism Scores of Non-Smokers
neuro_nonsmokes <- participants$B1SNEURO[participants$B1PA39 == 2]
mean_neuro_nonsmokes <- mean(neuro_nonsmokes)
sd(neuro_nonsmokes)



###########################################
################# Testing #################
###########################################

# Normality
boxplot(participants$B1SNEURO)

shapiro.test(participants$B1SNEURO)
#p-value = 4.071e-09 < alpha
#reject null; data is non-normal

# Attempted Transformations
log_nueoro <- log(participants$B1SNEURO)
shapiro.test(log_nueoro)
recip_neoro <- 1 / participants$B1SNEURO
shapiro.test(recip_neoro)
sqrt_neuro <- sqrt(participants$B1SNEURO)
shapiro.test(sqrt_neuro)

# Q-Q Plot
participants %>%
  ggplot(aes(sample = B1SNEURO)) +
  stat_qq(size = 2) +
  stat_qq_line() +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  labs(title = "Q-Q Plot of Neuroticism Personality Trait")



# Independent samples t-test

#H0: µsmokers – µnon-smokers = 0 (N scores of smokers and nonsmokers are the same)
#H1: µsmokers – µnon-smokers > 0 (N scores of smokers are higher than scores of nonsmokers)

neuro_smokers <- participants$B1SNEURO[participants$B1PA39 == 1]
neuro_nonsmokers <- participants$B1SNEURO[participants$B1PA39 == 2]

t.test(
  neuro_smokers,
  neuro_nonsmokers,
  paired = F,
  alternative = "greater",
  var.equal = F,
  conf.level = 0.99
)
# Results
#t = 3.0726
#df = 210
#p < .01

# Reject null hypothesis. Mean N scores of smokers significantly differ from scores of nonsmokers
