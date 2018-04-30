#Exercises_ANOVA.R
#Joshua Adrian
#Introduction to Biostats
#Purpose: ANOVA Exercises
#30_April_2018


library(tidyverse)

#Does feed type have an effect on the mass of pigs at the end of the experiment?

#H0: Feed type has no effect on the mass of pigs
#H1: Feed type has an effect on the mass of pigs

# enter the mass at the end of the experiment
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)
# make a dataframe
bacon <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))
  ),
  mass = c(feed_1, feed_2, feed_3, feed_4)
))

# Multiple factor ANOVA 

#Visualise data
ggplot(data = bacon, aes(x = feed, y = mass))+
  geom_boxplot(notch = T, aes(fill = feed))

#Run ANOVA
summary(aov(mass ~ feed, data = filter(bacon)))

             #Df Sum Sq Mean Sq F value   Pr(>F)    
#feed         3   4226  1408.8   164.6 1.06e-11 ***
#  Residuals   15    128     8.6                     
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Perform Tukey post-hoc test
TukeyHSD(aov(mass ~ feed, data = filter(bacon)))

#Look at the CI
plot(TukeyHSD(aov(mass ~ feed, data = filter(bacon))))

#Conclusion: 

#There is a significant relationship between feed type and pig mass. 
#The most significance can be seen in feed type 3 and 4. 
#Therefore; the null hypothesis is rejected.


# Exercise 2 --------------------------------------------------------------

teeth <- datasets::ToothGrowth

# 1-way ANOVA ---------------------------------------------------


# Research question: 
#Does the 2.0 dosage of VC affect tooth growth significantly more than 
#2.0 dosage of OJ?

#H0: There is no difference in tooth length between 2.0 OJ dosages and 2.0 
#VC dosages
#H1: There is no difference in tooth length between 2.0 OJ dosages and 2.0 
#VC dosages

teeth_2.0 <- teeth %>% 
  filter(dose %in% 2.0 )

#Visualise data
ggplot(data = teeth_2.0, aes(x = supp, y = len))+
  geom_boxplot(notch = T, aes(fill = supp))+
  labs(x = "Supplement", y = "Length (cm)")+
  theme_bw()

#Run ANOVA
teeth.aov <- aov(len ~ supp, data = filter(teeth_2.0))

summary(teeth.aov)

#Results

#Df Sum Sq Mean Sq F value Pr(>F)
#supp         1   0.03   0.032   0.002  0.964
#Residuals   18 270.61  15.034   


ggplot(data = teeth_2.0, aes(x = len, fill = supp)) +
  geom_histogram(position = "dodge", binwidth = 2)+
  theme_bw()

ggplot(teeth_2.0, aes(x = supp, y = len))+
  geom_boxplot(aes(fill = supp), notch = TRUE)

#Therefore there is no significant difference in tooth length 
#between 2.0 OJ dosages and 2.0 VC dosages
#The null hypothesis is not rejected


# Exercise 3 --------------------------------------------------------------

# 2 way ANOVA



teeth <- datasets::ToothGrowth
View(teeth)

#Question: Does tooth length depend on supp and dose?

#H0: Significant different tooth length is not assoiated with supp and dose
#H1: Significant different tooth length is assoiated with supp and dose

teeth$dose <- factor(teeth$dose, 
              levels = c(0.5, 1, 2),
              labels = c("D0.5", "D1", "D2")) #dose is numeric, convert to factor



#Visualise data
ggplot(data = teeth, aes(x = dose, y = len))+
  geom_boxplot(notch = T, aes(fill = supp))

#Run ANOVA
res.aov2 <- aov(len ~ supp + dose, data = teeth)
summary(res.aov2)

             #Df Sum Sq Mean Sq F value   Pr(>F)    
#supp         1  205.4   205.4   14.02 0.000429 ***
#dose         2 2426.4  1213.2   82.81  < 2e-16 ***
#Residuals   56  820.4    14.7                     
#---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    
res.aov3 <- aov(len ~ supp + dose + supp:dose, data = teeth)

summary(res.aov3)  

             #Df Sum Sq Mean Sq F value   Pr(>F)    
#supp         1  205.4   205.4  15.572 0.000231 ***
#dose         2 2426.4  1213.2  92.000  < 2e-16 ***
#supp:dose    2  108.3    54.2   4.107 0.021860 *  
#Residuals   54  712.1    13.2                     
#---
  #  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1  

#Perform Tukey post-hoc test
  
  #Conclusion: 
  
  #We can conclude that both supp and dose are statistically significant. 
  #Dose is the most significant fator. 
  #Therefore, changing the supp or the dose of vitamin C, will significantly 
  #impact the mean tooth length.    


#Tukey HSD test will be done only for the factor variable “dose”
# not for "supp” variable because it has only two significanty different levels

TukeyHSD(res.aov3, which = "dose")

#Look at the CI
plot(TukeyHSD(res.aov3, which = "dose"))


#Don’t need to perform the test for the “supp” variable 
#because it has only two levels



