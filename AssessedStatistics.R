calculate_mean <- function(data) {
    mean(data)
}

calculate_sd <- function(data) {
    sd(data)
}


calculate_se <- function(sd, n) {
    sd / sqrt(n)
}

calculate_ci <- function(mean, se, confidence_level) {
    z <- qnorm((1 + confidence_level) / 2)
    lower_bound = mean - z * se
    upper_bound = mean + z * se
    c(lower_bound, upper_bound)
}

hypothesis_test <- function(sample_mean, mu, se) {
    z_score = (sample_mean - mu) / se
    p_value = 2 * pnorm(-abs(z_score))
    list(z_score = z_score, p_value = p_value)
}

plot_histogram_qq <- function(data) {
    histogram <- hist(data, prob = TRUE)
    lines(density(data))
    qqnorm(data)
    qqline(data)
    list(histogram = histogram)
}

load(file = "PROJ.Rdata")
mydata = PROJ[257,]
n = 40

# Using the functions
mean_val = calculate_mean(mydata)
sd_val = calculate_sd(mydata)
se_val = calculate_se(sd_val, n)

# Confidence Intervals
ci_95 = calculate_ci(mean_val, se_val, 0.95)
ci_99 = calculate_ci(mean_val, se_val, 0.99)

# Hypothesis Testing
h_test = hypothesis_test(mean_val, mu = 0, se_val)

# Plotting
plot_histogram_qq(mydata)

# Print results
print(ci_95)
print(ci_99)
print(h_test)

  
