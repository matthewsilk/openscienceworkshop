###Analysis script###
library(brms)
source("R/file_read.r")

ls()

gapminder_dat<-file_read("data/gapminder-FiveYearData.csv")

head(gapminder_dat)

#I will use two Gaussian linear mixed effect models to analyse the dataset. In model one life-expectancy will be the response variable. The fixed effect structure will consist of main effects of population size, per capita GDP, continent and time, with an interaction fitted between per capita GDP and continent. I will use a random slopes model with a random slope for the effect of time in each country. Model two will not include the interaction between per capita GDP and continent, but be otherwise identical.

#Time will be mean-centred. Population size and per capita GDP will be mean-centred and scaled to have unit standard deviation.

#Life-expectancy is continuous but is bounded by zero and will be log-transformed if necessary.

mod1 <- brm(lifeExp ~ scale(gdpPercap)*continent + scale(pop) + (1|country), data = gapminder_dat, family = gaussian,save_all_pars=TRUE)
mod2 <- brm(lifeExp ~ scale(gdpPercap) + continent + scale(pop) + (1|country), data = gapminder_dat, family = gaussian,save_all_pars=TRUE)

#Inspect model summaries (look at coefficients and assess convergence)
summary(mod1)
summary(mod2)

#Indication of model fit (to be taken with a pinch of salt)
WAIC(mod1)
WAIC(mod2)

#The relative support for Model 1 vs. Model 2 will be assessed using Bayes factors.
bayes_factor(mod1, mod2)
