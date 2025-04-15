
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

# The summary output for linear mixed models in the lme4 package don't show p-values.
#This is a conscious choice made by the authors of the package, as there are many problems with p-values. 
#Please be careful when it comes to evaluating the significance of your model. Focus on your question and avoid plugging in and dropping variables from a model haphazardly until you make something “significant”. 
#Always choose variables based on biology/ecology and avoid adding in all possible variables that you have access to (i.e. don’t overfit). 
#Remember that as a rule of thumb, you need 10 times more data than parameters you are trying to estimate. 
#There are some common different methods used for model selection which we won't go into here but take a look at this thread if you are interested:
#   https://stats.stackexchange.com/questions/95054/how-to-get-an-overall-p-value-and-effect-size-for-a-categorical-factor-in-a-mi 
# 
# For simplification, we'll use the stargazer package that gives us a summary output table from our model.

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

