
# Solutions to exercies ----

## 1. Hierarchical linear model exercise ----

## Libraries ----

# ** remeber that you have to load the packages each time you start a new R session **
library(lme4)
library(tidyverse)
library(ggeffects) 


## Load the data ----

itex <- read_csv("Data/ITEX_diversity_data.csv") %>% 
  dplyr::select(-...1) # this code takes out the first column that was called "...1", it's not necessary though, just makes the dataframe cleaner I think

# look at the data to make sure it looks okay
view(itex)

## Exercise ----

# What is the relationship between diversity and temperature across sites (i.e. across latitude)?

itex <- itex %>% 
  mutate(TempC = (WarmQuarterTemp - 30)/2) #change temperature from F to °C (optional)
# mutate is used to make a new column, in this case we are making a new column called "TempC"
# by subtracting 30 from the column "PlotTemp" and dividing by 2.

hist <- hist(itex$SppRich) # this shows us the distribution of our response variable

itex <- itex %>% 
  mutate(TempC_scaled = scale(TempC, center = TRUE, scale = TRUE))
# it is a good practice to scale the explanatory variables
# do you remember what it is doing ?

# .....

# by scaling/centering you position the data around the mean of 0 
# so the values will be positioned on either side of the 0 depending on how far from the mean the values were

# now lets start with a simple linear model

itex.lm <- lm(SppRich ~ TempC_scaled, data = itex)
summary(itex.lm)


(prelim_plot <- ggplot(itex, aes(x = TempC_scaled, y = SppRich)) +
    geom_point() +
    geom_smooth(method = "lm"))

# notice that we get a warming message ... wonder what is causing that?

# here we are plotting the residuals (the variation that was not explained by the model) with the fitted values
# this is done to evaluate the model fit, and to detect non-lineartiy, unequal error variances and outliers
# When a linear regression model is suitable for a data set, then the residuals are more or less randomly distributed around the 0 line.
plot(itex.lm, which = 1)  # not perfect... 
## but we'll go with it
## the bigger the sample size, the less of a trend you'd expect to see

plot(itex.lm, which = 2)  # a bit off at the extremes, but that's often the case

# lets add colors based on sites to see if we notice any pattern
# in ggplot you can add colors by including "color = SITE" within the aesthetic 
(colour_plot <- ggplot(itex, aes(x = TempC_scaled, y = SppRich, color = SITE)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "none"))

# do we notice any pattern?
# yes I think so... colors are in some cases clustered together right?

# because of the nested structure of our data, i.e. plots within subsites within sites 
# we might want to think about using a linear mixed model
# then we need to think about what we want to include as random effects
# because we expect plots within SITE and SUBSITE to be more similar to each other, we will add SITE and SUBSITE as random effect

mixed.lmer <- lmer(SppRich ~ TempC_scaled + (1|SITE/SUBSITE), data = itex)
summary(mixed.lmer)
# lets first look at if the summary output corresponds to the structure of the data
# from the output we can see that the model was fitted with: 1294 obs., 82 SUBSITES and 23 SITES
# see this line in the summary output - Number of obs: 1294, groups:  SUBSITE:SITE, 82; SITE, 23

# hmm... that's not quite what we have is it? 
nrow(itex) # we have 1303 observation
length(unique(itex$SITE)) # 24 sites
length(unique(itex$SUBSITE)) # 83 subsites

# Something is not quite right here. Do you remember how we got a warning message earlier in the script when we were plotting the relationship?
# The problem we have is that one of the sites has missing values that we should remove before the analysis

# lets look for missing values then
sum(is.na(itex$SppRich)) # no NAs in SppRich, how about TempC_scaled?
sum(is.na(itex$TempC_scaled)) # yes, here we have 9 missing values

itex <- itex %>% 
  filter(!is.na(TempC_scaled)) # here we filter out all NA values within TempC_scaled,
# adding a ! infront will remove values when using the filter function

# okay lets try it again
nrow(itex) # now we have 1294 observation
length(unique(itex$SITE)) # 23 sites
length(unique(itex$SUBSITE)) # 82 subsites
#great now we can continue

#run the model again with the fixed dataset
mixed.lmer <- lmer(SppRich ~ TempC_scaled + (1|SITE/SUBSITE), data = itex)
summary(mixed.lmer)

# the summary output for linear mixed models don't show p-values. 
# this can be done using the stargazer package
library(stargazer) #remember to install it first if you haven't already - then load!

stargazer(mixed.lmer,
          digits = 3,
          type="text",
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# from this table we see that there is not a significant relationship with temperature and species richness across sites
# if there would have been a significant relationship, there would have been an asterick/s after the 1.714 value (the slope of the model)

# now for fun, lets visualize the new model to compare with our original linear model
# Extract the prediction data frame
pred.mm <- ggpredict(mixed.lmer, terms = c("TempC_scaled"))  # this gives overall predictions for the model

# Plot the predictions 
#install.packages("viridis") # install this package to be able to add nice colors 
#only install this if you haven't done that previously, otherwise go ahead and load it
library(viridis) 

(ggplot(pred.mm) + 
    geom_point(data = itex, 
               aes(x = TempC_scaled, y = SppRich, colour = SITE)) + # adding the raw data (scaled values) as points
    geom_line(aes(x = x, y = predicted), linewidth = 1, color = "black") +  # this is the slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # this is the error band
    labs(x = "Temperature (°C)", y = "Species richness", 
         title = "") + 
    scale_color_viridis(discrete = TRUE, option = "viridis") + #this will add nice colors to the plot from the viridis package
    theme_classic() +
    theme(legend.position = "none") # here we are removing the legend
)

# now we can compare the two models to see how they differ visually

(colour_plot <- ggplot(itex) +
    geom_point(size = 2, aes(x = TempC_scaled, y = SppRich, color = SITE)) +
    geom_smooth(method = "lm", aes(x = TempC_scaled, y = SppRich), color = "black") +
    scale_color_viridis(discrete = TRUE, option = "viridis") +
    theme_classic() +
    theme(legend.position = "none"))

# Can you make sense of the difference? By adding a random effect to our model, we no longer detected a positive relationship with species richness and temperature across sites.

# Now this is it for linear mixed modelling - hope you've learned something :-)
# remember to check out the Coding Club's INTRODUCTION TO LINEAR MIXED MODELS, find it here: https://ourcodingclub.github.io/tutorials/mixed-models/
# and this is also a great source to understand how to work with colors in ggplot: https://r-graph-gallery.com/ggplot2-color.html

## 2. Ordinations ----

## Libraries ----

library(vegan)


## Exercise ----

# Using the ITEX dataset (itex.long.csv), plot the species composition from two sites and four sub-sites in an NMDS ordination space and group them based on their location.

## Load the data ----

itex.long <- read_csv("Data/itex.long.csv") %>% 
  dplyr::select(-...1)


# lets look at the data
view(itex.long)

# here we have another ITEX dataset where we have information on species composition and species abundance within two different ITEX sites
unique(itex.long$SITE) # two sites
unique(itex.long$SUBSITE) # four subsites, two within each site

# This is what the different habitats name represent:
  # AD = ATQASUK dry 
  # AW = ATQASUK wet 
  # BD = Barrow dry 
  # BW = Barrow wet

# first we need to convert our data from a long format to a wide format

itex.wide <- itex.long %>% 
  pivot_wider(names_from  = SPECIES_NAME, values_from  = SumAbund, values_fill = 0) 

view(itex.wide)
# take a look at the dataset to see how this turned out
# it looks good, our species are now columns and the species abundance within each plot is now rows

# now remember how ordinations only take in numerical values (!), so we need to remove all columns that are something but numerical
# lets take a look at the structure of out dataset
str(itex.wide)
# we have 101 columns (or variables) but the three first ones are characters. lets remove them

itex.nmds <- itex.wide %>% 
  dplyr::select(-SITE, -SUBSITE, -PLOT)

#did that work?
view(itex.nmds)
str(itex.nmds)
#yes that seemed to work, make sure that you make a new dataframe (in our case itex.nmds) instead of overwiting the old one, because we need information on the sites and subsites later on

# now we can start making our NMDS
# here we are assigning a distance matrix to "dist" using Bray-Curtis index (remember how that index was suitable for species abundance data).
dist <- vegdist(itex.nmds,  method = "bray")
# notice -- if you get an error saying "Error in vegdist: (itex.nmds, method = "bray") : could not find function "vegdist" -- you need to load the vegan package !!


# Functions are used in R to perform a certain tasks. First you define the function (in our case NMDS.scree) and then later on you can call that function to perform a task
NMDS.scree <- function(x) { # where x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", main = "NMDS stress plot")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
  }
}

#here we are calling our function to perform a NMDS for 1-10 dimensions and then plotting the nr. of dimensions vs the stress

# we perform this to evaluate the optimal number of dimensions we will use in our analysis
NMDS.scree(dist)
# this could take a while...
# here we can see that between 2 and 3 dimensions, there isn't much difference - so we can go with 2 dimensions

set.seed(2)
# set.seed() allows you to control the randomness in your code, making your results reproducible. It doesn't really matter what number you put in the brackets, you just have to put some number

# Here, we perform the final analysis and check the result
NMDS1 <- metaMDS(dist, k = 2, trymax = 100, trace = F)
# k = 2 defines the number of dimensions we want to use, in our case 2, trymax determines the number of fits the ordination will perform to find the best fit, in our case 100 fits

# Lets check the results - check if the stress looks good - here 0.05 seems low enough
NMDS1

stressplot(NMDS1)

# now we can plot our ordination
plot(NMDS1, type = "t")

plot(NMDS1,)

# but notice how we only have our plots in this space, no species.
# we can add the species scores by giving the metaMDS the original community matrix as input and specifying the distance measure
# so essentially we are performing a new ordination analysis and instead of using vegsdist we define the distance within the ordination

NMDS2 <- metaMDS(itex.nmds, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance = "bray")
plot(NMDS2)
plot(NMDS2, display = "sites", type = "n")
points(NMDS2, display = "sites", col = "red", cex = 1.25)
text(NMDS2, display ="species")

# Alternatively, you can use the functions ordiplot and orditorp
ordiplot(NMDS2, type = "n")
orditorp(NMDS2, display = "species", col = "red", air = 0.01)
orditorp(NMDS2, display = "sites", cex = 1.1, air = 0.01)

# now this is a nice plot - we can see how the plots are really separated in space, each one of the four associated with different plant species 
# but we don't know which group belongs to which subsite

#first we need to define our subsites in a vector
subsites <- itex.wide$SUBSITE  #remember how I told you not to overwrite this dataset, now we are going to use the SUBSITE column to define our groups
colors <- c(rep("#ff0000", 24), rep("#590004", 24), rep("#606c38", 24), rep("#283618", 24)) #make a vector of different colors that you think are nice

# Plot convex hulls with colors based on the group identity
ordiplot(NMDS2, type = "n")
#orditorp(NMDS2, display = "sites", type = "n", air = 0.01, cex = 1.25)
orditorp(NMDS2, display = "species", col = "#444444", air = 0.01)
for(i in unique(group)) {
  ordihull(NMDS2$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=T) } 

# you can skip plotting in the species or the plot numbers since it makes it a bit complicated

# Okay there we have it, notice how the habitats (the two dry habitats and the two wet habitats) are closer together in space than the two sites - why could that be?

# Remember to check out the Coding Club's INTRODUCTION TO ORDINATION, find it here: <https://ourcodingclub.github.io/tutorials/ordination/>

# Don't hesitate to contact us if you are having problems with running the script.

#[katrin.bjornsdottir\@bioenv.gu.se](mailto:katrin.bjornsdottir@bioenv.gu.se){.email}

#[camila.pacheco.riano\@bioenv.gu.se](mailto:camila.pacheco.riano@bioenv.gu.se){.email}

# Good luck :-)
# there we have it, notice how the subsites are closer together in space than the sites - why could that be?
