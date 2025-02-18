# Set the parameters for the normal distribution
mean <- 5
sd <- 2

# Generate a sequence of numbers
x <- seq(mean - 4*sd, mean + 4*sd, length=100)

# Calculate the density of the normal distribution
y <- dnorm(x, mean=mean, sd=sd)

# Plot the normal distribution
plot(x, y, type="l", main="Normal Distribution (mean=5, sd=2)", xlab="x", ylab="Density")