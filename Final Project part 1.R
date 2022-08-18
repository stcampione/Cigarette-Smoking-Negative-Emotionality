setwd("/Users/sam/Downloads")
getwd()
data <- read.csv("finalprojectdata.csv", header = TRUE)
View(data)

participants <- data[complete.cases(data$B1SNEURO) & complete.cases(data$B1PA39),]
View(participants)



neuro <- table(participants$B1SNEURO)
length(participants$B1SNEURO)
View(neuro)

smokes <- table(participants$B1PA39)
length(participants$B1PA39)
View(smokes)

length(participants$B1SNEURO)
length(participants$B1PA39)

n = 481

neuro_mean <- mean(participants$B1SNEURO, na.rm = T) 
  #2.068262
neuro_sd <- sd(participants$B1SNEURO) 
  #0.6149801


smokers.relfreq <- 131/481 
  #0.2723493
nonsmokers.relfeq <- 350/481 
  #0.7276507

~~~~~~~~~~~~~~~~~~~~~~~~~~(part 2, data analysis)~~~~~~~~~~~~~~~~~

boxplot(participants$B1SNEURO)
?
shapiro.test(participants$B1SNEURO)
  #p-value = 4.071e-09 < alpha 
  #reject null; data is non-normal

log_nueoro <- log(participants$B1SNEURO)
shapiro.test(log_nueoro)
recip_neoro <- 1/participants$B1SNEURO
shapiro.test(recip_neoro)
sqrt_neuro <- sqrt(participants$B1SNEURO)
shapiro.test(sqrt_neuro)

?qqnorm
qqnorm(participants$B1SNEURO, main = "Q-Q Plot of Neuroticism Personality Trait")
qqline(participants$B1SNEURO)

install.packages("ggplot2")
library(ggplot2)
install.packages("qqplotr")
library(qqplotr)
ggplot(mapping = aes(sample = participants$B1SNEURO)) +
  stat_qq_point(size=2) +
  stat_qq_line() +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles")
  

qqnorm(participants$B1PA39)
qqline(participants$B1PA39)

hist(participants$B1SNEURO, breaks = 15)

neuro_smokes <- participants$B1SNEURO[participants$B1PA39 == 1]
View(neuro_smokes)

neuro_nonsmokes <- participants$B1SNEURO[participants$B1PA39 == 2]
View(neuro_nonsmokes)
mean_neuro_smokes <- mean(neuro_smokes)
mean_neuro_nonsmokes <- mean(neuro_nonsmokes)

sd(neuro_smokes)
length(neuro_smokes)
v1=((0.6634433)^2)/131

sd(neuro_nonsmokes)
length(neuro_nonsmokes)
v2=((0.5873671)^2)/350

df_num= (v1+v2)^2
df_denom=((((v1^2))/130)+((v2^2)/349))
df= df_num/df_denom
df #210.7095 ~ 210 

tcrit= qt(p=.05, df=210, lower.tail=FALSE)

tstat_num = (mean_neuro_smokes - mean_neuro_nonsmokes) -0
tstat_denom = sqrt(v1+v2)
tstat = tstat_num/tstat_denom
tstat

p_value = pt(q=tstat,df=210,lower.tail=FALSE)
p_value

t.test(neuro_smokes, neuro_nonsmokes, paired = F, alternative = "greater", var.equal = F, conf.level = 0.99)
  #t = 3.0726
  #df = 210.71 ~ 210
  #p < .01
  #reject null hyp. mean N scores of smokers significantly differ from scores of nonsmokers

#H0: µsmokers – µnon-smokers = 0 (N scores of smokers and nonsmokers are the same)
#H1: µsmokers – µnon-smokers > 0 (N scores of smokers are higher than scores of nonsmokers)
?t.test

#t(degress of freedom) = the t statistic, p = p value.

__________________________________________________________________________
#demographic variables

#AGE B1PAGE_M2
age <- data$B1PAGE_M2
age_table <- table(data$B1PAGE_M2)
View(age_table)

mean(data$B1PAGE_M2, na.rm = T)
summary(data$B1PAGE_M2)
  #mean age: 55.92
  #range: [34,83]
  #median: 55

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GENDER (B1PGENDER)
gender_table <- table(data$B1PGENDER)
View(gender_table)

154+206
men.relfreq <- 154/360
  #0.4277778
women.relfreq <- 206/360
  #0.5722222

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#RACE (B1PF7A)
race_table<- table(data$B1PF7A)
View(race_table)
sum(data$B1PF7A, na.rm = T)

#1 White: 1009/1165 = 0.8660944
#2 African American: 77/1165 = 0.06609442
#3 Native American or Alaskan Native Aleutian Islander/Eskimo: 21/1165 =0.01802575
#4 Asian: 12/1165 =0.01030043
#6 Other: 41/1165 =0.03519313
#7 don't know: 3/1165 #8 refused: 2/1165  7 & 8 = .0.004291845
      0.002575107+0.001716738
      0.004291845*100 = #0.4291845

install.packages("plyr")
library("plyr")
data$B1PF7A <- as.factor(data$B1PF7A)
data$binaryrace = revalue(data$B1PF7A, c("1" = "1", "2" = "2", "3" = "2", "4" = "2", "6"="2","7" ="2","8"="2"))
data$binaryrace
View(data$binaryrace)

race_table2 <- table(data$binaryrace)
View(race_table2)

white <- 1009
non_white <- 156
n_race <- white + non_white #1165

white.relfreq <- 1009/1165
  #0.8660944
non_white.relfreq <- 156/1165
  #0.1339056
  
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TOTAL HOUSEHOLD INCOME (B1STINC1)

income <- table(data$B1STINC1)
View(income) 
mean_income <- mean(data$B1STINC1, na.rm =T)
  #70115.12

summary(data$B1STINC1)
  #mean: 70115
  #range: [0, 300000]
  #Q1: 26250
  #Q3: 92500
  #median: 52750




