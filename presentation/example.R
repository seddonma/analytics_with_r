2 + 2

# Comments start with #

# Assign objects to names for later
rand <- rnorm(1000)

# Many built in functions
hist(rand)

# Get help with ?
?hist

# Linear model ------------------------------------------------------------

x <- rnorm(1000, mean = 100)
y <- rnorm(1000, mean = 700)
plot(x, y)
model <- lm(y ~ x)
summary(model)
# Add the line of best fit
abline(model, col = "red")
