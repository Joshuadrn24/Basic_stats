#Day_5.R
#Joshua Adrian
#Introduction to Biostats
#Purpose: 5th day of the stats class covering distributions, t-tests and heatmaps 
#20_April_2018


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(Rmisc)
  #Has summarySE


# #Load data --------------------------------------------------------------

snakes <- read_csv("snakes.csv") %>%
  mutate(day = as.factor(day))

#or 

snakes$day <- as.factor(snakes$day)

#Do the snakes eventually become acclimatized/habituated?
#Data is not independent because we're using the same snakes.

# Manipulate the data -----------------------------------------------------

snakes_summary <- snakes %>%
  #group_by(day, snake) %>%  #There is no  replication within a day  
  group_by(day) %>% 
  summarise(snakes_mean = mean(openings),
            snakes_sd = sd(openings))


# Formulate hypothesis ----------------------------------------------------

#H0: There is NO difference in the number of openings from day to day
#H1: There is a difference in the number of openings from day to day



# Test a hypothesis -------------------------------------------------------

snakes.summary2 <- summarySE(data = snakes, measurevar = "openings", 
                             groupvars = c("day"))
#measurevar = measurement variable; the variable you're measuring
#groupvars + grouping cariables

#Visualise data
ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

#But wait, we need another null hypothesis

#H0: There is NO difference between snakes with respect to 
      #the number of openings at which they habituate.
#H0: There is NO difference between days i.t.o. the number of openings at which
      #the snakes habituate

#Test just the days hypothesis
snakes.day.aov <- aov(openings ~ day, data = snakes)
summary(snakes.day.aov)

#Df in your thesis is important
#Need F value
#You can say the probability is less than 0.05

#Test both hypotheses:

snakes.all.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.all.aov)


# Testing assumptions afterwards ------------------------------------------

#First test normality of data
snakes.residual <-  residuals(snakes.all.aov)
hist(snakes.residual)

#Then visualise homoscedasticity of results
plot(fitted(snakes.all.aov), residuals(snakes.all.aov))

#Check Tukey results
snakes.tukey <- TukeyHSD(snakes.all.aov, which = "day")
plot(snakes.tukey)

#Visualise factor interaction
ggplot(data = snakes, aes(x = as.numeric(day),
                          y = openings,
                          colour = snake)) +
  geom_line(size = 3)+
  geom_point(size = 4)



# Exercise ----------------------------------------------------------------

#Get the moth data fom Github
#Run a 2-way ANOVA on them

#Moths can be bad for trees, invasive species. Bad for resources. 

#1 - numerical summaries
#2 - graphical summaries
#Hypotheses
  #Test them

moth <- read_csv("Moth.csv") %>% 
  gather(key = "trap", value = "count", -Location)


moth_summary <- moth %>%
  group_by(Location) %>% 
  summarise(moth_mean = mean(count),
            moth_sd = sd(count))


#H0: There is NO difference between the number of moths at the various 
      #locations of the traps 
#H1: There is a difference between the number of moths at the various 
      #locations of the traps 


moth.summary2 <- summarySE(data = moth, measurevar = "count", 
                             groupvars = c("Location"))

ggplot(data = moth, aes(x = Location, y = count)) +
  geom_segment(data = moth.summary2, aes(x = Location, xend = Location, y = count - ci, yend = count + ci, colour = Location),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = Location), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

#H0: There is NO difference between the number of moths at the various 
      #locations of the traps 
#H0: There is NO diffrence between the trap types with respect to location

moth.loc.aov <- aov(count ~ Location, data = moth)
summary(moth.loc.aov)

#Df Sum Sq Mean Sq F value   Pr(>F)    
#Location     3   1981   660.5   11.34 6.46e-06 ***
  #Residuals   56   3262    58.2                     
#---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
  #Test both hypotheses:
  
moth.all.aov <- aov(count ~ Location + trap, data = moth)
summary(moth.all.aov)


  #Testing assumptions afterwards ------------------------------------------
  
  #First test normality of data
  moth.residual <-  residuals(moth.all.aov)
hist(moth.residual)

#Then visualise homoscedasticity of results
plot(fitted(moth.all.aov), residuals(moth.all.aov))

#Check Tukey results
moth.tukey <- TukeyHSD(moth.all.aov, which = "Location")
plot(moth.tukey)

#Visualise factor interaction
plot1 <- ggplot(data = moth, aes(x = Location,
                          y = count)) +
geom_boxplot()+
  geom_jitter(shape = 2, width = 0.05)

plot2 <- ggplot(data = moth, aes(x = Location,
                        y = count)) +
  geom_boxplot(aes(fill = trap))

plot3 <- ggplot(data = moth, aes(x = trap,
                        y = count)) +
  geom_boxplot()+
  geom_jitter(shape = 2, width = 0.05)

library(ggpubr)

ggarrange(plot1, plot2, plot3, nrow = 2, ncol = 2, labels = "AUTO")


# Correlations ------------------------------------------------------------

#Linear line doesn't mean the relationship is linear
#Ordinal - ordered data value

# Load libraries
library(tidyverse)
library(ggpubr)
library(corrplot)

# Load data
ecklonia <- read_csv("ecklonia.csv")


# Formulate hypothesis ----------------------------------------------------

#H0: There is no relationship between stipe length and primary blade width for the kelp
    #Ecklonia maxima
#H1: There is a relationship between stipe length and primary blade width for the kelp
    #Ecklonia maxima


# Test a hypothesis -------------------------------------------------------

cor.test(ecklonia$stipe_length, ecklonia$primary_blade_width)

#visualise the data
ggplot(data = ecklonia, aes(x = stipe_length, y = primary_blade_width))+
  geom_point()

# Run hecka tests at once -------------------------------------------------

ecklonia_sub <- ecklonia %>% 
  select(stipe_length:epiphyte_length)

ecklonia_cor <- cor(ecklonia_sub)
ecklonia_cor


# Spearman rank test ------------------------------------------------------

#For ordinal data

# Create ordinal data
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length + ecklonia$frond_length), breaks = 3))

#how many breaks do you want to have; how many cuts - effectively creates bins (histogram)

#Then run a Spearman test
cor.test(ecklonia$length, ecklonia$stipe_diameter, method = "spearman")


# Kendell rank test -------------------------------------------------------
#for not normal data

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width,
         method = "kendall")


# Visualise all the things! ------------------------------------------------

#tutorials are online
ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

corrplot(ecklonia_pearson, method = "circle")
 #positve or negative influences the size of the dots



# Make heat map of correlatins for Ecklonia data --------------------------



# Load libraries
library(tidyverse)
library(ggpubr)
library(corrplot)
library(reshape2)
library(ggplot2)

# Load data
eckl <- read_csv("ecklonia.csv")


melted_eckl <- melt(ecklonia_pearson)
head(melted_eckl)

#Visualise
ggplot(data = melted_eckl, aes(x =  Var1, y = Var2, fill=value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
 





















 



