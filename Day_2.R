#Day_2
#Introduction to Biostats
#Purpose: 2nd day of the stats class discussion data visualisation and distributions
#13_April_2018


# Load libraries ----------------------------------------------------------

library(tidyverse)


# Manual calculations -----------------------------------------------------



#using random data
r_dat <- data.frame(dat = rnorm(n = 600, mean = 372, sd = 50), #making a lil spreadsheet
                    sample = "A")#random normal distributions: 3 arguments: no. of samples(n), mean, standard deviation (sd)

#quick visualisation
ggplot(data = r_dat, aes(x = dat))+
  geom_density()

#The mean
#sum of all points
#divide by 
#no. of points

r_dat %>%
  summarise(r_sum = sum(dat), r_n = n(), r_mean = r_sum/r_n, r_mean_func = mean(dat))     
#gives you the summarised data that you want  #don't ever use uppercase letters for headings of your columns                                   


#The median

#brute force with base R
order(r_dat$dat)[length(r_dat$dat)/2]
#or use tidy
r_dat %>% 
  arrange(dat) %>% 
  slice(n()/2)
#Or the tidy automagic way
r_dat %>% 
  summarise(r_median = median(dat))
#slice to select a particular row of data

#variance
#The sum of 
  #Each value 
    #minus 
      #the mean
        #Squared
#Divided by
  #count of samples minus 1
r_dat %>% 
  mutate(r_error = dat-mean(dat),
         r_error_square = r_error * r_error) %>% #to calculate anomaly values 
  summarise(r_squared_sum = sum(r_error_square),
            r_var = r_squared_sum/(n()-1),
            #OR use the built in function
            r_var_func = var(dat))

#The standard deviation
r_dat %>% 
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var),
            r_sd_func = sd(dat))

# Exercise 1 --------------------------------------------------------------

summary(ChickWeight$weight) #gives summary on distribution of data

ChickWeight %>%
  summarise(min_wt = min(weight),
            quart_1 = quantile(weight, 0.25),
            med_wt = median(weight),
            mean_wt = mean(weight),
            quart_3 = quantile(weight, 0.75),
            max_weight = max(weight))

# Visualisations ----------------------------------------------------------

#These few functions contain most functions necessary 
#to make publication ready figures
library(ggpubr)
library(RColorBrewer)
library(viridis)

#Loa our SA time data
sa_time <- read_csv("SA_time.csv") #Tab key to give you your data paths

#Edit our data
sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1)) 

sa_long <- sa_time %>% 
  gather(key = "time_type", value = "minutes", -human) #name of column that contains  our variables

 #from row 1 up to n rows adding 1
#geo = c(rep(c("Cape Town", "George", "PE"), times = 6), rep("Joburg", 2)))
# adds human row from 1 to the number of rows 
#(20) by increments of 1 (ie 1,2,3,4...)

#don't ever alter data unless it is in code

# Qualitative  ------------------------------------------------------------

# Create a count of qualitative values
sa_count <- sa_long %>%
  count(time_type) %>% 
  mutate(prop = n/sum(n))


#Stacked bar graphs

ggplot(data = sa_count, aes(x= "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +   #because it needs to know that's making a graph from the count data
  labs(title = "Stacked bar graph", subtitle = "cumulative sum",
       x = NULL, y = "Count") +
  theme_minimal()

#Stacked proportion bar graph

 ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_minimal()

#A pie chart - TABOO!!!!!

ggplot(data = sa_count, aes(x = "", y = n,fill = time_type))+
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Pie chart", subtitle = "seriously? My mom bakes better pies!",
       x = NULL, y = NULL) +
  coord_polar("y", start = 0)
  theme_minimal()


# Continuous data ---------------------------------------------------------

#Histograms
ggplot(data = sa_long, aes(x = minutes))+
  geom_histogram()

#oh no!
# let's get rid of those two values
sa_clean <- sa_long %>% 
  filter(minutes <= 300)

#Try again
ggplot(data = sa_clean, aes(x = minutes))+
  geom_histogram(aes(y = ..density.., fill = time_type), position = "dodge") +
  #stat_count(width = 0.5)+
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

#Boxplots!
?geom_boxplot
#The boxplot compactly displays the distribution of a continuous variable. 
#It visualises five summary statistics 
#(the median, two hinges and two whiskers),
# and all "outlying" points individually.

# Boxplots
# x - qualitative, y - quantitative
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type))

#q1 and q3 are the ends of the blocks
#dot is max value
#between q1 and q3 is interquartile range
#tail = q1 or q3 x 1.5 But why are tails not same length on either side? hhhmm?
#tail shows all datapoints within q1 or q3 x 1.5
#Blue is more variable compared to red, thus red has a point as an outlier 
#green is very small, so two points are outliers

# Notched boxplots
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE)
#middle line is median
#areas in boxes that are overlapping within the notch - 
  #not statistically different from each other
    #"indented" parts in blue and red overlap, in green it does not so green is statistically different to blue and red

# Calculate summary stats for plotting over the boxplots
sa_summary_stats <- sa_clean %>% 
  group_by(time_type) %>% 
  summarise(time_type_mean = mean(minutes))

ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE) +
  geom_point(data = sa_summary_stats, size = 6, shape = 18,
             aes(y = time_type_mean), colour = "goldenrod")

# Relationships -----------------------------------------------------------

# Add geo column
sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1),
         geo = c(rep(c("Cape Town", "George", "PE"), times = 6),
rep("Joburg", 2)))

#Basic scatterplot 
#Has quantitative data on both x and y axes
ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point() +
  coord_equal(xlim = c(0,300), ylim = c(0,300))

#Make the scale smaller at xlim and ylim
ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point() +
  coord_equal(xlim = c(0,60), ylim = c(0,60))

#Adding trend lines
ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point(aes(colour = geo)) +
  geom_smooth(aes(colour = geo), method = "lm") +
  coord_equal(xlim = c(0,60), ylim = c(0,60))
#if grey overlaps with other grey - points are similar
#outliers shows difference
#angle of line shows relationship between x and y 




  



















