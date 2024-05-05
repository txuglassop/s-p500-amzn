# ACTL2131 Assignment Code - z5480859 - Tadhg Xu-Glassop

# Install and load packages
install.packages('dplyr')
install.packages('ggplot2')
install.packages('moments')
install.packages('gridExtra')
library(dplyr)
library(ggplot2)
library(moments)
library(gridExtra)

# Import data

setwd("C:/Users/txugl/OneDrive/Desktop/UNSW/ACTL2131")
prices <- read.csv("C:/Users/txugl/OneDrive/Desktop/UNSW/ACTL2131/actl2131_assignment_data.csv", header = T)

# Task 1: Calculate and Plot Daily Log Returns

# Add two new columns to prices with the log return for each stock
prices <- prices %>% mutate(log_SPY = log(SPY) - log(lag(SPY)),
                            log_AMZN = log(AMZN) - log(lag(AMZN)))

# Set date column in data to date format for plotting
prices$Date <- as.Date(prices$Date, format = "%d/%m/%Y")

# Plot data
ggplot( data = prices, aes(x = Date) ) +
  geom_line( aes(y = log_SPY, colour = "SPY"), size = 0.8, alpha = 0.6 ) +
  geom_line( aes(y = log_AMZN, colour = "AMZN"), size = 0.8, alpha = 0.6) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Log Return of Stock", title = "Log Returns of AMZN and SPY") +
  theme_minimal() +
  scale_colour_manual(values = c("SPY" = "#2396DC", "AMZN" = "#F55353"), name = "Stocks")

# Task 2: Get Summary Statistics for Log Returns

# Calculate each moment for both stocks' log return
SPY_mean <- mean(prices$log_SPY, na.rm = TRUE)
SPY_var <- var(prices$log_SPY, na.rm = TRUE)
SPY_skew <- skewness(prices$log_SPY, na.rm = TRUE)
SPY_kurt <- kurtosis(prices$log_SPY, na.rm = TRUE)

AMZN_mean <- mean(prices$log_AMZN, na.rm = TRUE)
AMZN_var <- var(prices$log_AMZN, na.rm = TRUE)
AMZN_skew <- skewness(prices$log_AMZN, na.rm = TRUE)
AMZN_kurt <- kurtosis(prices$log_AMZN, na.rm = TRUE)

# Make a data frame containing the moments for each stock
log_r_stats <- data.frame(
  SPY = c(SPY_mean, SPY_var, SPY_skew, SPY_kurt),
  AMZN = c(AMZN_mean, AMZN_var, AMZN_skew, AMZN_kurt)
)
row.names(log_r_stats) <- c("Mean", "Variance", "Skewness", "Kurtosis")

# Print data frame to console
log_r_stats

# Task 3: Create a histogram of each log return

# For SPY
SPY_hist <- ggplot(prices, aes(x = log_SPY)) +
  geom_histogram(aes(y = ..density..),binwidth = 0.003, fill = "#2396DC", colour = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = log_r_stats["Mean", "SPY"],
                                         sd = sqrt(log_r_stats["Variance", "SPY"])),
                colour = "#F55353", size = 1) +
  labs(title = "Log Return of SPY with Normal Curve", x = "SPY Log Return", y = "Density") +
  theme_minimal() +
  ylim(0,50) +
  xlim(-0.15, 0.15)

# For AMZN
AMZN_hist <- ggplot(prices, aes(x = log_AMZN)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.003, fill = "#2396DC", colour = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = log_r_stats["Mean", "AMZN"],
                                         sd = sqrt(log_r_stats["Variance", "AMZN"])),
                colour = "#F55353", size = 1) +
  labs(title = "Log Return of AMZN with Normal Curve", x = "AMZN Log Return", y = "Density") +
  theme_minimal() +
  ylim(0,50) +
  xlim(-0.15, 0.15)

# Show both histograms side by side
grid.arrange(SPY_hist, AMZN_hist, ncol = 2)

# Task 4: Examine if the log returns fit a normal distribution, and suggest a better distribution

# Numerical Method: Get excess kurtosis
exc_k_SPY <- log_r_stats["Kurtosis", "SPY"] - 3
exc_k_AMZN <- log_r_stats["Kurtosis", "AMZN"] - 3

# Print both
exc_k_SPY
exc_k_AMZN

# Visual Method: Generating QQ plots for both log returns

# To plot both QQ plots besides each other
par(mfrow=c(1, 2))

# For SPY

# Get quantiles from theoretical normal with mean and variance from log_r_stats
n_SPY <- length(prices$log_SPY)
theoretical_q_SPY <- qnorm(ppoints(n_SPY), mean = log_r_stats["Mean", "SPY"], sd = sqrt(log_r_stats["Variance", "SPY"]))

# Plot QQ plot
qqplot(theoretical_q_SPY, sort(prices$log_SPY),
       main = "SPY Log Returns vs Normal Dist",
       xlab = "Theoretical Quantiles from Normal",
       ylab = "Sample Quantiles of SPY Log Returns")
abline(0, 1, col = "#F55353") 

# For AMZN

# Get quantiles from theoretical normal with mean and variance from log_r_stats
n_AMZN <- length(prices$log_AMZN)
theoretical_q_AMZN <- qnorm(ppoints(n_AMZN), mean = log_r_stats["Mean", "AMZN"], sd = sqrt(log_r_stats["Variance", "AMZN"]))

# Plot QQ plot
qqplot(theoretical_q_AMZN, sort(prices$log_AMZN),
       main = "AMZN Log Returns vs Normal Dist",
       xlab = "Theoretical Quantiles from Normal",
       ylab = "Sample Quantiles of AMZN Log Returns")
abline(0, 1, col = "#F55353") 

# Reset plot layout
par(mfrow=c(1, 1))

# Task 5: Explore dependencies between SPY and AMZN

# Numerically: Find correlation between the two stocks
cor_AMZN_SPY <- cor(na.omit(prices$log_AMZN), na.omit(prices$log_SPY))

# Print the value of correlation to console
cor_AMZN_SPY

# Graphical: Show returns on a scatter plot

# Plot data with a scatter plot
ggplot(prices, aes(x = log_SPY, y = log_AMZN)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "#F55353") +
  labs(x = "SPY", y = "AMZN", title = "Log Returns of AMZN and SPY") +
  theme_minimal()

# Task 6: Find if there is a statistical difference between the annualised log returns

# Calculate our test statistic
ann_log_SPY <- 250 * log_r_stats["Mean", "SPY"]
ann_log_AMZN <- 250 * log_r_stats["Mean", "AMZN"]
ann_sd_SPY <- 250 * sqrt(log_r_stats["Variance", "SPY"])
ann_sd_AMZN <- 250 * sqrt(log_r_stats["Variance", "AMZN"])

T <- (ann_log_SPY - ann_log_AMZN) / (sqrt(  (ann_sd_SPY^2/n_SPY ) + (ann_sd_AMZN^2/n_AMZN ) ))

# Print test statistic
T

# Find two tailed p value
p = 2 * (1 - pnorm(T))

# Print p value
p
