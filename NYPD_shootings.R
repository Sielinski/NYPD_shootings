# NYPD Shooting Incident Data (Historic)

# Project Step 1: Start an Rmd Document
# 
# Start an Rmd document that describes and imports the shooting project dataset 
# in a reproducible manner.

#library(stringr)
#library(readr)
#library(dplyr)
#library(tidyr)
#library(lubridate)
#library(ggplot2)

library(tidyverse)

data_file <- 'https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD'

if (T) {
  ny_dat <- read_csv(data_file)
  ny_backup <- ny_dat
} else {
  ny_dat <- ny_backup
}


# Project Step 2: Tidy and Transform Your Data
#
# Add to your Rmd document a summary of the data and clean up your dataset by 
# changing appropriate variables to factor and date types and getting rid of any 
# columns not needed.  Show the summary of your data to be sure there is no 
# missing data. If there is missing data, describe how you plan to handle it.

# for my convenience, convert column names to lower case (to make typing easier)
colnames(ny_dat) <- colnames(ny_dat) %>% 
  str_to_lower()

# convert the data types of location, perpetrator, and victim fields to factors 
# location factors
ny_dat$precinct <- as.factor(ny_dat$precinct)
ny_dat$loc_of_occur_desc <- as.factor(ny_dat$loc_of_occur_desc)
ny_dat$jurisdiction_code <- as.factor(ny_dat$jurisdiction_code)
ny_dat$loc_classfctn_desc <- as.factor(ny_dat$loc_classfctn_desc)
ny_dat$location_desc <- as.factor(ny_dat$location_desc)

# perpetrator factors
ny_dat$perp_age_group <- as.factor(ny_dat$perp_age_group)
ny_dat$perp_sex <- as.factor(ny_dat$perp_sex)
ny_dat$perp_race <- as.factor(ny_dat$perp_race)

# victim factors
ny_dat$vic_age_group <- as.factor(ny_dat$vic_age_group)
ny_dat$vic_sex <- as.factor(ny_dat$vic_sex)
ny_dat$vic_race <- as.factor(ny_dat$vic_race)


# display the number of shooting records per unique incident
ny_dat %>%
  group_by(incident_key) %>% 
  summarize(cnt = n()) %>% 
  ungroup() %>% 
  arrange(desc(cnt))

# keep only fields needed to analyze multi-victim incidents
#ny_dat <- ny_dat %>% 
#  select(incident_key, occur_date, statistical_murder_flag)

# convert the date field from a character data type to a date 
ny_dat$occur_date <- mdy(ny_dat$occur_date)

# coarsen the granularity of the occurrence date to monthly and yearly and convert 
# them to factors, which will help to simplify analysis and visualizations 
ny_dat <- ny_dat %>% 
  mutate(occur_year = as.factor(year(occur_date)), 
         occur_month = floor_date(occur_date, unit = 'month'), 
         calendar_month = month(occur_date), 
         month_name = factor(month.name[calendar_month], levels = month.name)
  )

# create a data frame that contains multi-victim incidents
# for each incident, include the number of victims and murders per incident
incident_cnts <- ny_dat %>%
  group_by(incident_key) %>% 
  summarize(victims = n(), murders = sum(statistical_murder_flag)) %>% 
  ungroup() %>% 
  filter(victims > 1) 

# reduce ny_dat to just those rows with a matching incident_key in incident_cnts
#ny_dat <- ny_dat |>
#  inner_join(select(incident_cnts, incident_key), by = 'incident_key')  

summary(ny_dat)

# how many such events have occurred
nrow(incident_cnts)


# Project Step 3: Add Visualizations and Analysis

# Add at least two different visualizations & some analysis to your Rmd.  
# Does this raise additional questions that you should investigate?  
# You should include at least two visualizations and one model.

# count the number of shootings by month and year 
ny_dat %>% 
  group_by(month_name, occur_year) %>% 
  summarize(shootings = n()) %>% 
  ungroup() %>% 
  # plot the result
  ggplot(aes(x = month_name)) +
  geom_line(aes(y = shootings, group = occur_year, color = occur_year)) +
  labs(title = 'Incidents per month on a year-over-year basis', x = 'calendar month', color = 'year')

# look at the frequency distribution of multi-victim incidents 
victims_plot <- incident_cnts %>% 
  ggplot() +
  geom_bar(aes(x = victims)) +
  scale_x_continuous(breaks = pretty(incident_cnts$victims, n = length(unique(incident_cnts$victims)))) +
  labs(title = 'Frequency distribution of multi-victim incidents by the number of victims', 
       x = 'number of victims')

victims_plot

# look at the frequency distribution of murders for multi-victim incidents 
murders_plot <- incident_cnts %>% 
  ggplot() +
  geom_bar(aes(x = murders)) +  
  scale_x_continuous(breaks = pretty(incident_cnts$murders, n = length(unique(incident_cnts$murders)))) +
  labs(title = 'Frequency distribution multi-victim incidents by the number of murders')

murders_plot


############################################
## model victims as a Poisson distribution
############################################

cnt_all_incidents <- nrow(incident_cnts)

# average number of murders 
victims_lambda <- mean(incident_cnts$victims)

# when the var will exceeds the mean, the data exhibit overdispersion 
mean(incident_cnts$victims)
var(incident_cnts$victims)
min(incident_cnts$victims)

sum_all_victims <- sum(incident_cnts$victims)

# count the number of incidents and total number of victims for incidents 
# that have n victims 
victims_model <- incident_cnts %>% 
  group_by(victims) %>% 
  summarize(incidents_cnt = n(), victims_cnt = sum(victims)) %>% 
  mutate(incidents_pct = incidents_cnt / cnt_all_incidents) %>% 
  ungroup() 

# add rows to the table for any missing n
n_range <- min(victims_model$victims):max(victims_model$victims)
missing_n <- data.frame(victims = n_range[!(n_range %in% victims_model$victims)], 
                        incidents_cnt  = 0, 
                        victims_cnt = 0, 
                        incidents_pct  = 0
)
victims_model <- victims_model |>
  bind_rows(missing_n) %>% 
  arrange(victims)

# use a Poisson distribution to predict the percentage of incidents with n victims
victims_model$model_pct <- dpois(victims_model$victims, lambda = victims_lambda)

if (T) {
  # The Poisson model allows for the number of incidents to be zero, which can  
  # *never* happen because there's always at least one victim in an incident. 
  # Because the sum all *possible* probabilities needs to add up to 1, rescale 
  # the model's initial probabilities to exclude 0 and normalize the sum of 
  # of probabilities to 1. 
  
  # First, rescale sum_all_victims: We'll use the rescaled percentages
  # to predict counts for each n, and this will ensure that the sum of actual 
  # and predicted counts will (closely) match
  sum_all_victims <- round(sum_all_victims / sum(victims_model$model_pct), 0)

  # This is the same as chisq.test's rescale.p = T
  victims_model$model_pct <- victims_model$model_pct / sum(victims_model$model_pct)

  # confirm that the percentages both add to 1
  sum(victims_model$incidents_pct)
  sum(victims_model$model_pct)
}

# use the percentage to predict a number of incidents and victims
victims_model <- victims_model %>% 
  mutate(predicted_incidents = round(cnt_all_incidents * model_pct, 0), 
         predicted_victims = round(predicted_incidents * victims, 0)
  )

# confirm that the model accounts for all victims 
sum(victims_model$predicted_victims)
sum_all_victims

victims_model

victims_plot +
  geom_point(data = victims_model, aes(x = victims, y = predicted_incidents), color = 'red') +
  geom_line(data = victims_model, aes(x = victims, y = predicted_incidents), color = 'red')

# perform the chi-squared test
chi_squared_test <- chisq.test(x = victims_model$incidents_pct, p = victims_model$model_pct, rescale.p =  T)

# Print the test result
# A low p-value means that there is statistically significant evidence to 
# suggest that the observed distribution does *not* fit the expected distribution
print(chi_squared_test)


############################################
## model murders as a Poisson distribution
############################################

# we don't need to rescale the murders model because the number of murders 
# can be zero

# average number of murders 
murders_lambda <- mean(incident_cnts$murders)
sum_all_murders <- sum(incident_cnts$murders)

# when the var will exceeds the mean, the data exhibit overdispersion 
# consider the negative binomial 
mean(incident_cnts$murders)
var(incident_cnts$murders)

# count the number of incidents and total number of murders for incidents
# that have n murders. calculate the percentage of incidents
murders_model <- incident_cnts %>% 
  group_by(murders) %>% 
  summarize(incidents_cnt = n(), murders_cnt = sum(murders)) %>% 
  mutate(incidents_pct = incidents_cnt / cnt_all_incidents) %>% 
  ungroup() 

# add rows to the table for any missing n
n_range <- min(murders_model$murders):max(murders_model$murders)
missing_n <- data.frame(murders = n_range[!(n_range %in% murders_model$murders)], 
                        incidents_cnt  = 0, 
                        murders_cnt = 0, 
                        incidents_pct  = 0
)
murders_model <- murders_model |>
  bind_rows(missing_n) %>% 
  arrange(murders)

# use a Poisson distribution to predict the percentage of incidents with n murders
murders_model$model_pct <- dpois(murders_model$murders, lambda = murders_lambda)

# use the percentage to predict a number of incidents and murders
murders_model <- murders_model %>% 
  mutate(predicted_incidents = round(cnt_all_incidents * model_pct, 0), 
         predicted_murders = round(predicted_incidents * murders, 0))

murders_model

# confirm that the model accounts for all victims (numbers should be close)
sum(murders_model$predicted_murders)
sum_all_murders

# plot the data and the model
murders_plot +
  geom_point(data = murders_model, aes(x = murders, y = predicted_incidents), color = 'red') +
  geom_line(data = murders_model, aes(x = murders, y = predicted_incidents), color = 'red')

# perform the chi-squared test
# the counts/frequencies can't be too small (ideally, >= 5)
# one workaround is to combine bins
max_cnts_bin <- max(which(murders_model$incidents_cnt >= 5))
max_predicted_bin <- max(which(murders_model$predicted_incidents >= 5))
shared_max <- min(max_cnts_bin, max_predicted_bin) 

incidents_pcts <- c(murders_model$incidents_pct[1:(shared_max - 1)], 
                sum(murders_model$incidents_pct[shared_max:nrow(murders_model)])
)

model_pcts <- c(murders_model$model_pct[1:(shared_max - 1)], 
                sum(murders_model$model_pct[shared_max:nrow(murders_model)])
)


chi_squared_test <- chisq.test(x = incidents_pcts, p = model_pcts)

chi_squared_test

if (F) {
  incidents_cnts <- c(murders_model$incidents_cnt[1:(shared_max - 1)], 
                   sum(murders_model$incidents_cnt[shared_max:nrow(murders_model)])
  )
  
  model_cnts <- c(murders_model$predicted_incidents[1:(shared_max - 1)], 
                  sum(murders_model$predicted_incidents[shared_max:nrow(murders_model)])
  )
  
  chisq.test(x = incidents_cnts, p = model_cnts, rescale.p = T)
  
}

# Print the test result
# A high p-value suggests that the observed data is consistent with the Poisson 
# distribution, within the bounds of normal statistical variation
print(chi_squared_test)


murders_model %>% 
  select(murders, incidents_pct, model_pct)


###


# number of shootings and murders over time
ny_dat |> 
  select(occur_year, occur_month, calendar_month, statistical_murder_flag)

ny_dat %>% 
  group_by(occur_month) %>% 
  summarize(shootings = n(), murders = sum(statistical_murder_flag)) %>% 
  ungroup() %>% 
  ggplot(aes(x = occur_month)) +
  geom_point(aes(y = shootings, color = 'shootings')) +
  geom_smooth(aes(y = shootings, color = 'shootings'), se = F, linetype = 'dashed') +
  geom_point(aes(y = murders, color = 'murders')) +
  geom_smooth(aes(y = murders, color = 'murders'), se = F, linetype = 'dashed') +
  labs(title = 'Incidents per week')

# average number of murders per shooting is low
# but that's because most multi-shooting incidents involve only two shootings 
# and most of those involve zero murders. 
# simple averages would suggest 0.25 murders per shooting
# average number of murders per shooting
global_rate <- sum(incident_cnts$murders) / sum(incident_cnts$victims)
print(global_rate)


# plot the number of murders against the shootings per incident
# most incidents with two shootings have zero murders
# most incidents with four shootins have two murders
ggplot(incident_cnts, aes(x = victims, y = murders)) +
  geom_point(position = 'jitter', alpha = 0.1) +
  scale_y_continuous('murders per incident', 
                     breaks = 0:9) +
  labs(title = 'Murders-per-incident by shootings-per-incident', 
       x = 'shootings per incident')

# plot the frequency distribution of murders, faceted by the number of victims
# the murders-to-shootings ratio changes with shootings-per-incident 
# In some cases, we have only one incident. In some cases (e.g., 11 shootings), 
# we have zero incidents
ggplot(incident_cnts, aes(x = murders)) +
  geom_histogram(aes(y = ..density..), bins = 10) + 
  facet_wrap(. ~ victims) +
  scale_x_continuous(breaks = 0:9) +
  labs(title = 'Frequency distribution of murders', 
       subtitle = 'Faceted by the number of victims', 
       y = 'number of incidents')

# calculate the actual ratio of murders-to-victims based on the number of   
# victims-per-incident
murder_ratio <- incident_cnts %>% 
  group_by(victims) %>% 
  summarize(incidents = n(), mean_murders = mean(murders)) %>% 
  ungroup() %>% 
  mutate(murder_ratio = mean_murders / victims)

# use the global_rate to predict the number of murders
murder_ratio$global_rate <- global_rate  * murder_ratio$victims

#############
## CHANGES
#############

# global_rate -> global_ratio
# murder_ratio -> faceted_ratio
# murder_ratio$global_rate -> predicted_global
# murder_ratio$predicted -> predicted_lm


incident_cnts %>% 
  group_by(victims) %>% 
  summarize(cnt = n(), death = sum(murders >= 1)) %>% 
  mutate(death_pct = death / cnt) %>% 
  arrange(death_pct) %>% 
  ggplot(aes(x = victims, y = death_pct)) +
  geom_point()


# plot the difference between the actual and predicted number of murders
ggplot(murder_ratio, aes(x = victims)) +
  geom_point(aes(y = mean_murders), color = 'blue') + 
  # add the average model
  geom_line(aes(y = global_rate), color = 'red', linetype = 'dotted') +  
  labs(title = 'Number of murders by victims-per-incident', 
       y = 'number of murders', 
       x = 'victims-per-incident')


# plot the murders-to-victims ratio
ggplot(murder_ratio) +
  geom_col(aes(x = victims, y = murder_ratio)) +
  # show the average murder ratio as a dashed red line
  geom_hline(yintercept = global_rate, color = 'red', linetype = 'dashed') +
  labs(title = 'Murders-to-victims ratio by number of victims', 
       x = 'number of victims', 
       y = 'murders-to-victims ratio')


# build a model to predict the number of murders based on victims-per-incident
lm_fit <- lm(murders ~ victims, data = select(incident_cnts, murders, victims))
summary(lm_fit)

####################
## Add date to LM
####################

lm_fit <- lm(murders ~ victims, data = select(incident_cnts, murders, victims))
summary(lm_fit)

table(ny_dat$perp_sex)

tmp <- incident_cnts %>% 
  mutate(multi_victim = victims > 1)

cor(tmp$victims, tmp$multi_victim)

incidents_dates_cnts <- ny_dat %>% 
  mutate(male_perp = perp_sex == 'M') %>% 
  group_by(incident_key, occur_year, occur_month, calendar_month, month_name, precinct, male_perp) %>% 
  summarize(victims = n(), murders = sum(statistical_murder_flag)) %>% 
  mutate(multi_victim = victims > 1) %>% 
  ungroup()

lm_dates_fit <- lm(murders ~ victims + male_perp + multi_victim, data = incidents_dates_cnts)
summary(lm_dates_fit)

# use the model to predict the number of murders
murder_ratio$predicted <- predict(lm_fit, newdata = murder_ratio)

# plot the difference between the actual and predicted number of murders
ggplot(murder_ratio, aes(x = victims)) +
  geom_point(aes(y = mean_murders), color = 'blue') + 
  # add the linear model
  geom_line(aes(y = predicted), color = 'blue', linetype = 'dashed') +  
  # add the linear model
  #geom_line(aes(y = log_predicted), color = 'purple', linetype = 'dashed') +  
  # add the average model
  geom_line(aes(y = global_rate), color = 'red', linetype = 'dotted') +  
  # emphasize the outliers
  geom_point(
    data = filter(murder_ratio, mean_murders == 0),
    aes(y = mean_murders),
    color = 'red',
    shape = 1, 
    size = 5
  ) +
  labs(title = 'Number of murders by victims-per-incident', 
       y = 'number of murders', 
       x = 'victims-per-incident')


# calculate the R^2 using actual and predicted values
r_squared <- function(actual_values, predicted_values) {
  residual_values <- actual_values - predicted_values
  SSR <- sum(residual_values ^ 2)
  SST <- sum((actual_values - mean(actual_values)) ^ 2)
  r_squared <- 1 - (SSR / SST)
  r_squared
}

# lm model
r_squared(incident_cnts$murders, predict(lm_fit, incident_cnts))
# global_rate
r_squared(incident_cnts$murders, incident_cnts$victims * global_rate)

# more direct methods 
summary(lm_fit)$r.squared
cor(incident_cnts$victims, incident_cnts$murders) ^ 2


# Project Step 4: Add Bias Identification
#
# Write the conclusion to your project report and include any possible sources 
# of bias.  Be sure to identify what your personal bias might be and how you 
# have mitigated that.

# what counts as a murder? 
# sample size 
# any analysis on a monthly basis needs to account for the number of days in the month
