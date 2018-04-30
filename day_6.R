#Day_6.R
#Joshua Adrian
#Introduction to Biostats
#Purpose: Confidence intervals, testing assumptions (exercises included), chi-squared, recaps
#26_April_2018

 

#A confidence interval (CI) tells us within what range we may be certain to 
#find the true mean from which any sample has been taken. 
#If we were to repeaqtedly sample the same population over and over 
#and calculated a mean every time, the 95% CI indicates the range that 95% 
#of those means would fall into.

library(rcompanion)

Input <- ("
Student  Sex     Teacher  Steps  Rating
          a        female  Jacob    8000   7
          b        female  Jacob    9000  10
          c        female  Jacob   10000   9
          d        female  Jacob    7000   5
          e        female  Jacob    6000   4
          f        female  Jacob    8000   8
          g        male    Jacob    7000   6
          h        male    Jacob    5000   5
          i        male    Jacob    9000  10
          j        male    Jacob    7000   8
          k        female  Sadam    8000   7
          l        female  Sadam    9000   8
          m        female  Sadam    9000   8
          n        female  Sadam    8000   9
          o        male    Sadam    6000   5
          p        male    Sadam    8000   9
          q        male    Sadam    7000   6
          r        female  Donald   10000  10
          s        female  Donald    9000  10
          t        female  Donald    8000   8
          u        female  Donald    8000   7
          v        female  Donald    6000   7
          w        male    Donald    6000   8
          x        male    Donald    8000  10
          y        male    Donald    7000   7
          z        male    Donald    7000   7
          ")

data <- read.table(textConnection(Input),header = TRUE)
summary(data)

groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3)
 #Mean steps taken by students across all the different factors / across the entire class

# one-way data:
#for males and females separately
groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3)

# two-way data:
#Does the teachers' name affect the sex
grp_t <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)

#affect of teacher and sex on the mean value



# Load libraries ----------------------------------------------------------

library(ggplot2)
  


ggplot(grp_t, aes(x = Sex, y = Mean))+
  geom_point(aes(colour = Teacher)) +
  geom_errorbar(aes(ymin = Mean - Trad.lower,
                    ymax = Mean + Trad.upper)) +
  facet_wrap(~Teacher) +
  theme_bw()

#read up on bootstrapping

# Testing assumptions -----------------------------------------------------


#in write up you have to mention you have made the various assumotions and how
  #Always check your data to make sure they meet the assumptions

#For data conforming to this expectation, we say that the data are independent and identically distributed, or i.i.d. We will deal in particular with the the assumptions of normality and heteroscedasticity in this chapter. Whether or not the dependent data are continuous and independent comes down to proper experimental design, so if these are violated then?c (I?fll say no more).

#How do we know this? Here are your options, followed by a quick refresher:
  
#  Perform any of the diagnostic plots we covered in the earlier Chapters.
#Compare the variances and see if they differ by more than a factor of four.
#Do a Levene?fs test to test for equal variances.
#Do a Shapiro-Wilk test for normality.



# Log transforming --------------------------------------------------------

as.tibble(Input)

#Log
log <- data %>% 
  mutate(ln_steps = round(log(Steps), digits = 2), #log = natural log
         log_step = round(log10(Steps), digits = 2),
         cube_root = round(Steps^(1/3), digits = 2),
         square_root = round(sqrt(Steps), digits = 2)) #%>%
  #select(-Student, -Rating) %>%  #columns you're not interested in - throw it out
  #gather(key = "data.type", value = "trans.data", -Sex, -Teacher) %>%  #you want to add 1 beneath the other
  #mutate(data.type = as.factor(data.type))
 
  

 library(tidyverse) 

library(ggpubr)
#Frequency graphs

plot1 <- ggplot(log, aes(x = Sex, y = log_step))+
  geom_histogram(stat = "identity")+
  facet_wrap(~ Teacher)

plot2 <- ggplot(log, aes(x = Sex, y = ln_steps))+
  geom_histogram(stat = "identity")+
  facet_wrap(~ Teacher)

plot3 <- ggplot(log, aes(x = Sex, y = cube_root))+
  geom_histogram(stat = "identity")+
  facet_wrap(~ Teacher)

plot4 <- ggplot(log, aes(x = Sex, y = square_root))+
  geom_histogram(stat = "identity")+
  facet_wrap(~ Teacher)

  ggarrange(plot1, plot2, plot3, plot4)
  

# Example: Dataset for ANOVA ----------------------------------------------

  
#H0: There is no significant difference in petal.width between the three iris species

irisdat <- as.tibble(iris)

shapiro.test(iris$Petal.Width)

iris %>%
  group_by(Species) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(Petal.Width)[2]))
#We find that some of the species have non-normal data

#Do a krustal-willis test instead of an ANOVA

kruskal.test(Petal.Width ~ Species, data = iris)


# ASSUMPTIONS EXERCISES ---------------------------------------------------

#• Normally distributed data
#• Homogeneity of variances
#• Independence of data
#• In our case, we will encourage also that the data are balanced


# Load the libraries ------------------------------------------------------

library(tidyverse)

shapiro.test(chicks$Time)

#Ex. 1

chicks %>% 
  group_by(Time) %>% 
  summarise(norm_wt = as.numeric(shapiro.test(weight)[2]))

#When p >= 0.05 we may assume that
#the data are normally distributed. 
#If p < 0.05 then the data are not normally distrubted.

#Day 2- not normal
#Day 16 - normal

chicks_16 <- chicks %>% 
  filter(Time == 16) %>%
  group_by(Diet) 

  chicks_2 <- chicks %>% 
  filter(Time == 2) %>%
  group_by(Diet) 
  
  
# Ex. 2 :Log transforming 


#Log
log_chicks1 <- chicks_16 %>% 
  mutate(ln_wt = round(log(weight), digits = 2), #log = natural log
         log_wt = round(log10(weight), digits = 2),
         cube_wt = round(weight^(1/3), digits = 2),
         square_wt = round(sqrt(weight), digits = 2))  
  
log_chicks2 <- chicks_2 %>% 
  mutate(ln_wt = round(log(weight), digits = 2), #log = natural log
         log_wt = round(log10(weight), digits = 2),
         cube_wt = round(weight^(1/3), digits = 2),
         square_wt = round(sqrt(weight), digits = 2))  

#Day 16

Plot1 <-  ggplot(log_chicks1, aes(x = Diet, y = ln_wt ))+
  geom_histogram(stat = "identity") 
  
Plot2 <-ggplot(log_chicks1, aes(x = Diet, y = log_wt ))+
  geom_histogram(stat = "identity") 

Plot3 <-ggplot(log_chicks1, aes(x = Diet, y = cube_wt ))+
  geom_histogram(stat = "identity") 

Plot4 <-ggplot(log_chicks1, aes(x = Diet, y = square_wt ))+
  geom_histogram(stat = "identity") 

ggarrange(Plot1, Plot2, Plot3, Plot4)

#Day 2

Plot1.1 <-  ggplot(log_chicks2, aes(x = Diet, y = ln_wt ))+
  geom_histogram(stat = "identity") 

Plot2.1 <-ggplot(log_chicks2, aes(x = Diet, y = log_wt ))+
  geom_histogram(stat = "identity") 

Plot3.1 <-ggplot(log_chicks2, aes(x = Diet, y = cube_wt ))+
  geom_histogram(stat = "identity") 

Plot4.1 <-ggplot(log_chicks2, aes(x = Diet, y = square_wt ))+
  geom_histogram(stat = "identity") 

ggarrange(Plot1.1, Plot2.1, Plot3.1, Plot4.1)






















