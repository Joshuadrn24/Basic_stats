#Day_1
#Introduction to Biostats
#Purpose: 1st day of the stats class
#12_April_2018


# Load libraries ----------------------------------------------------------

library(tidyverse)

# Integers ----------------------------------------------------------------

#Generate some integer  data
integer_r <- as.integer(seq(5, 14, by = 1)) # from 5 to 22, but differencing by 1

#Look at a brief summary of them
summary(integer_r)


# Continuous --------------------------------------------------------------

#Generate a sequence of numeric values
numeric_r <- seq(23, 43, length.out = 10) # length of 10 numbers








# Dates -------------------------------------------------------------------

as.Date("2005-12-31") - as.Date ("2005-12-12") #what is the difference
#or, for examole
dates_r <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")
#There is much more
summary(dates_r)


# Dataframes --------------------------------------------------------------

#create the base dataframe
df_r <- data.frame(integers = integer_r, 
                  numeric = numeric_r,
                  dates = dates_r) #create tibble, with 3 columns, with these names
#NB_make sure lengths are all the same

#Then upgrade to tibble
df_r <- as_tibble(df_r) #better at displaying metadata
summary(df_r)


# Categories --------------------------------------------------------------

#Electrons
elec_r <- as.factor(c("laptops",
                      "desktops",
                      "cell phones")) #quantifies these words as factors - because certain functions are unique to factor variables

#People
people_r <- as.factor(c("funny hair",
                        "beautiful",
                        "beanies"))

#colours
colour_r <- as.factor(c("red", "blue"))



# Ordinal -----------------------------------------------------------------

#Here we still have qualitative data with some ranks

colour_qual <- ordered(c("blue", "green", "yellow", "orange", "red"),
                          levels = c("blue", "green", "yellow", "orange", "red")) #applies importance



# Binary ------------------------------------------------------------------

#Takes on "either or" data or TRUE or FALSE
binary_r <- c(TRUE, FALSE, TRUE, TRUE)
summary(binary_r)


# Characters --------------------------------------------------------------

#creating a character vector
sites_r <- c("Yztervarkpunt", "Betty's Bay", "Gansbaai", "Sea Point")



# Missing values ----------------------------------------------------------

#No. eggs recorded in a nest
#NA shows a nest that wasn't able to be sampled
chicks_nest <- c(3, 2, 0, 10, 5, 6, 8, 2, 4, NA)
#A summary
summary(chicks_nest)
#The mean
mean(chicks_nest)
#The standard deviation
sd(chicks_nest)

# [1: row, column]


# Descriptive stats -------------------------------------------------------

#First create a dataframe
chicks <- as_tibble(ChickWeight)

chicks %>% 
  summarise(chicken_count = n()) #scientific notation for number of thngs is n
#or
nrow(chicks)


# Measures of central tendency --------------------------------------------

#calculate mean weight

chicks %>%
  summarise(mean_wt = mean(weight))

#Be more specific
chicks %>% 
  filter(Time == 21) %>% #== means equivalent to
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight))

#visualise the density of the dtata
ggplot(data = filter(chicks, Time == 21), aes(x = weight, fill = Diet))+
  geom_density(alpha = 0.4)



# Skewness ----------------------------------------------------------------

#value of the mean denotes skewness

#Calculate the numeric value
#1st load lib
library(e1071)

#Compare difference in ean and median against skewness
chicks %>% 
  filter(Time == 21) %>% #== means equivalent to
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight))


# Kurtosis ----------------------------------------------------------------

#calculate the kurtosis of the tails of a distributio
  chicks %>% 
  filter(Time == 21) %>% #== means equivalent to
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight), 
            kurtosis_wt = kurtosis(weight))


#Kurtosis = the sharpness of the peak of a frequency-distribution curve.


# Measures of variability (Variance) ----------------------------------------------------------------

#Below is a summary of many different statistical properties
wt_summary <- chicks %>% 
  filter(Time == 21) %>% #== means equivalent to
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
           var_wt = var(weight),
           sd_wt = sd(weight),
           min_wt = min(weight),
           wt_quart2 = quantile (weight, 0.5), #proportion of quantile
           wt_quart1 = quantile(weight, 0.25),
           wt_quart3 = quantile(weight, 0.75))
 




