#Day_3.R
#Introduction to Biostats
#Purpose: 3rd day of the stats class covering distributions and also t-tests
#Joshua Adrian
#17_April_2018



# Bernoulli distribution --------------------------------------------------

# It is used to represent data resulting from a single experiment with binary 
#(yes or no; black or white; positive or negative; success or failure; 
#dead or alive;) outcomes, such as a coin toss—there are only two options, 
#heads or tails. Nothing else.

#Biological factors in a study need to have a large enough 
#sample size e.g dead or alive


# Binomial distribution ---------------------------------------------------

#This data distribution results from repeating identical experiments that produce a binary outcome with probability  
#p
#a specified number of times, and choosing  
#n
#samples at random. As such, it represents a collection of Bernoulli trials.



# Generating a Cullen and Frey graph --------------------------------------

#load the libraries
library(tidyverse)
library(fitdistrplus)
library(logspline)

# Generate log-normal data
y <- c(37.50,46.79,48.30,46.04,43.40,39.25,38.49,49.51,40.38,36.98,40.00,
       38.49,37.74,47.92,44.53,44.91,44.91,40.00,41.51,47.92,36.98,43.40,
       42.26,41.89,38.87,43.02,39.25,40.38,42.64,36.98,44.15,44.91,43.40,
       49.81,38.87,40.00,52.45,53.13,47.92,52.45,44.91,29.54,27.13,35.60,
       45.34,43.37,54.15,42.77,42.88,44.26,27.14,39.31,24.80,16.62,30.30,
       36.39,28.60,28.53,35.84,31.10,34.55,52.65,48.81,43.42,52.49,38.00,
       38.65,34.54,37.70,38.11,43.05,29.95,32.48,24.63,35.33,41.34)

par(mfrow = c(2, 2))
plot(x = c(1:length(y)), y = y)
hist(y)
descdist(y, discrete = FALSE, boot = 100)

r_norm <- rnorm(n = 1000, mean = 13, sd = 1)   #generating random data, n = no.  samples, mean

hist(r_norm) %>% 
descdist(r_norm, discrete = FALSE, boot = 100)

#uniform data
y <- rnorm(100, 13, 2)
par(mfrow = c(2, 2))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)


# T-tests -----------------------------------------------------------------

#If you have a basic hypothesis, use this
#At the heart of many basic scientific inquiries is the simple question “Is A different from B?” The scientific notation for this question is:

  #H0: Group A is not different from group B.
  #H1: Group A is different from group B.

# Random normal data

r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))


# Check assumptions -------------------------------------------------------

#Normality
#For this we may use Shapiro-Wilk test
shapiro.test(r_dat$dat) #is the data normal or not: <0.05 - data are significantly different from normal
shapiro.test(r_dat$dat)[1]
shapiro.test(r_dat$dat)[2]

#But that is testing all of data together
#we must be a bit more clever
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2])) #force to look at no.
  #[2] - shapiro test for group A + B, and adding them together
  #NB: Normal data = p>0.5
  # non_normal = p<=0.5


# Check homoscedasticity --------------------------------------------------

#There are many ways to check for this
#which is the similarity of vrince between sample sets
#for now we will simply say that these assumptions is met when
#the variance of the samples are not > 2 - 4 times greater than one another

#check everything at once
#WRONG
var(r_dat$dat)

#OR do it the tidy way
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))
 

# A one sample t-test -----------------------------------------------------

r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")


#Test normaity of distribution
shapiro.test(r_dat$dat)



#Perhaps a visualisation?
ggplot(data = r_one, aes(x = dat, fill = sample)) +
  geom_histogram(binwidth = 0.4)+
  theme_bw()
  


#Run the test
t.test(r_one$dat, mu = 20)  #indicate the column,  define the population mean 
                              #Look at "Console" to check if what you're doing
                              #is correct

#Run a test we know will produce a significant result
t.test(r_one$dat, mu = 30)


# Pick a side -------------------------------------------------------------

#Are these data SMALLER/LESS than the population mean
t.test(r_one$dat, mu = 20, alternative = "less")
# OR GREATER
t.test(r_one$dat, mu = 20, alternative = "greater")

#But what about for the larger population mean?
#Are the samples less than the population of 30?
t.test(r_one$dat, mu = 30, alternative = "less")
#What about >?
t.test(r_one$dat, mu = 30, alternative = "greater")



# Two-sampled t-tests -----------------------------------------------------

#create another dataframe
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))
#20 samples is normally the minimum, in biology

#Run a default/basic test
t.test(dat ~ sample, data = r_two, var.equal = TRUE) #is the variance =, TRUE/FALSE

# Large sample size prevents variation across replicate experiments

#Pick a side
#Is A < B?
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")

#Is A > B?
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "greater")


# Replicate 6.6 -----------------------------------------------------------


#Ex. 1

library(tidyverse)
library(fitdistrplus)
library(logspline)

#From Cape Town in the Western Cape (A) and Durban in the Eastern Cape (B)
#30 pet dogs were weighed to determine which city has the heaviest dogs of a
#single German sepherd breed. All dogs were the same age.

#H0: Weight of 30 dogs at A is not greater than B
#H1: Weight of 30 dogs at A is greater than B


r_2_test <- data.frame(dat = c(rnorm(n = 30, mean = 6, sd = 1),
                            rnorm(n = 30, mean = 8, sd = 1)),
                    sample = c(rep("A", 30), rep("B", 30)))

ggplot(data = r_2_test, aes(x = dat, fill = sample)) +
  geom_histogram()+
  theme_bw()+
  facet_wrap(~sample)

#Run a default/basic test
t.test(dat ~ sample, data = r_2_test, var.equal = TRUE, alternative = "greater")

t.test(dat ~ sample, data = r_2_test, var.equal = TRUE, alternative = "less")

#Conclusion

    #The overall weight of German shepherds is not significantly greater in Cape Town
      #than in Durban







