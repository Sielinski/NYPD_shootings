#################################
## Zero-inflated Poisson Model
#################################

data <- incident_cnts$murders

# Remove zeros from the dataset
non_zero_data <- data[data > 0]

# Estimate lambda by calculating the mean of the non-zero data
lambda_hat <- mean(non_zero_data)

# Print the estimated lambda
cat("Estimated lambda (Poisson mean) for non-zero data:", lambda_hat, "\n")

# Zero-inflation probability estimate
pi_hat <- sum(data == 0) / length(data)

# Print the estimated zero-inflation probability
cat("Estimated pi (Zero-inflation probability):", pi_hat, "\n")

# Calculate the predicted probabilities
unique_values <- 0:max(data)
predicted_prob <- numeric(length(unique_values))
predicted_prob[1] <- pi_hat + (1 - pi_hat) * exp(-lambda_hat)  # Probability of zero
predicted_prob[-1] <- (1 - pi_hat) * dpois(unique_values[-1], lambda_hat)  # Probabilities of non-zeros

# Calculate the predicted frequencies
predicted_freq <- predicted_prob * length(data)

# Compare observed and predicted frequencies
observed_freq <- table(factor(data, levels = 0:max(data)))

# Plot observed vs predicted frequencies
barplot(rbind(observed_freq, predicted_freq), beside = TRUE, 
        col = c("red", "blue"), legend = c("Observed", "Predicted"),
        main = "Observed vs Predicted Frequencies", xlab = "Number of Victims", ylab = "Frequency")


########################################
## Everybody, half the people, nobody
########################################

# plot the number of murders against the shootings per incident
# it looks like there are three likely outcomes: everybody gets murdered, half 
# people get murdered, or nobody gets murdered
ggplot(incident_cnts, aes(x = victims, y = murders)) +
  geom_point(position = 'jitter', alpha = 0.1) +
  scale_y_continuous('Murders per Incident', 
                     breaks = 0:max(incident_cnts$murders)) +
  labs(title = 'Murders-per-incident by Shootings-per-incident', 
       x = 'Shootings per Incident')


#########
## R^2
#########

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