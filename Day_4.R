#Day_4.R
#Joshua Adrian
#Introduction to Biostats
#Purpose: ANOVA
#19_April_2018

#ANOVA to compare more than 2 samples
  #same no. individuals assigned per group

#($ - chooses the column)

library(tidyverse)

# t-test ------------------------------------------------------------------



# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21) #to isolate variables from one column

#t-test
t.test(weight ~ Diet, data = chicks_sub) #weight as a function of Diet
 
#We do not reject the Null Hypothesis



# 1-way ANOVA -------------------------------------------------------------


# Research question: 
  #is there a difference in chicken mass attained after 21 days
  #after the chickens hav been fed four different diets?

#H0: There is no difference in chicken mass at 21 days after having been fed
    #onee of four diets

chicks_21 <- chicks %>% 
  filter(Time == 21)

#or

chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))

chicks.aov1 <- aov(weight ~ Diet, data = chicks_21)

summary(chicks.aov1)



ggplot(data = chicks_21, aes(x = weight, fill = Diet)) +
  geom_histogram(position = "dodge", binwidth = 100)+
  theme_bw()

ggplot(chicks_21, aes(x = Diet, y = weight))+
  geom_boxplot(aes(fill = Diet), notch = TRUE)
#can't be trusted, there's a lot f variance
#If bands intersect there is no significant difference
# Significant difference difference between diets 1,3 
#triangles to ensure a symmetrical notch i.e. the distance is the same
  # >/< Q1 and Q3
  # 1.5 times IQR


# Tukey HSD test ----------------------------------------------------------

TukeyHSD(chicks.aov1)
 #What does this tell us?
    # difference
    # Confidence intervals
    # significant difference 
  # Diet 2 and 1 does not result in significant different chickens
  #If lwr confidence is positive, there is a significant difference 


#BOXPLOT is for distribution for many pts of data


# Visuals -----------------------------------------------------------------

#read Tukey helpfile
#geom_segment()

ggplot(chicks_21, aes(x = Diet, y = weight, fill = Diet))+
  geom_boxplot(notch = TRUE, colour = "grey50")+
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight+2))

#boxplot with horizontal line = confidence interval
#confidence interval - what's our level of confidence that our means won't overlap

#does lower and upper values have a bar?

#crossing 0 line means you can't be certain

#segments showing CI
#Dataframe of sgements assigned to chicks_Tukey
chicks_Tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)
  #use this to generate a geom_segment
chicks_Tukey$pairs <- as.factor(row.names(chicks_Tukey))

ggplot(data = chicks_Tukey, aes(x = pairs, y = diff))+
  geom_segment(aes(x = pairs, xend = pairs, y = lwr, yend = upr+2))+
  labs(y = "Dfferences in mean level of diet", x = "Pairs")+
  geom_hline(aes(yintercept = 0), colour = "red", linetype = "dashed")+
  coord_flip()+
  theme_bw()

#or BASE R

plot(TukeyHSD(aov(weight ~ Diet, data = chicks_21)))
  


# Multiple factor ANOVA ---------------------------------------------------

#H0; There is no change in mass (kg) from day 0 to day 21.

Chicks_0_21 <- ChickWeight %>% 
  filter(Time %in% c(0, 2, 21))

#Visualise data
ggplot(data = Chicks_0_21, aes(x = as.factor(Time), y = weight))+
  geom_boxplot(notch = T, aes(fill = as.factor(Time)))

#Run ANOVA
summary(aov(weight ~ as.factor(Time), data = filter(Chicks_0_21)))

#Perform Tukey post-hoc test
TukeyHSD(aov(weight ~ as.factor(Time), data = filter(Chicks_0_21)))

#Look at the CI
plot(TukeyHSD(aov(weight ~ as.factor(Time), data = filter(Chicks_0_21))))

#Look only at day 0 ad 21 for both time and diet
summary(aov(weight ~ Diet + as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

#Or simply look at ALL of the Time
#. . . which is NOT the hypothesis
summary(aov(weight ~ Diet + as.factor(Time), data = ChickWeight))
#Note the Increase in the degrees of freedom for time factor
#But no increase for d.f. for Diet

#Now to look at interactions BETWEEN factors
summary(aov(weight ~ Diet + as.factor(Time), data = filter(ChickWeight, Time %in% c(4, 21))))

# Look at Tukey
TukeyHSD(aov(weight ~ Diet *as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))
plot(TukeyHSD(aov(weight ~ Diet *as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21)))))


#Create a line graph to help explain this concept
#First create mean values by Time and Diet
chicks_mean <- ChickWeight %>% 
  group_by(Diet, Time) %>% 
  summarise(weight_mean = mean(weight, na.rm = T))

ggplot(data = chicks_mean, aes(x = Time, y = weight_mean, colour = Diet))+
  geom_line(size = 2)+
  geom_point(shape = 15, size = 5)
  
#No effect at beginning but major effect at the end  
  


# non - parametric tests --------------------------------------------------

#But what if we don't have normal data?

#For a t-test we rather use wilcox rank sum test
wilcox.test() #And then one fills this in the same as for t.test()

#And now for the Kruskall-Wallis 
kruskal.test(weight ~ Diet, data = Chicks_0_21) #same as ANOVA

#Load this for a non-parametric posthoc test
library(pgirmess)
kruskalmc(weight ~ Diet, data = Chicks_0_21)

