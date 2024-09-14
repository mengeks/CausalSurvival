# Command: 
# Make a plot: from 0 to a, plot function value
# lambda_0. After a, plot lambda_1
setwd("~/Dropbox (Harvard University)/Xiang_Iav/code")

# Define the constants
lambda_0 <- 1/2
a <- 1.5
lambda_1 <- 1/1

# Define the function values
x <- seq(0, 2.5, by = 0.01)
y <- ifelse(x <= a, lambda_0, lambda_1)

# Plot the function
png(filename = "./figures/hazard-viz.png")
plot(x, y, type = "l", col = "blue", lwd = 2,
     xlab = "x", ylab = "Hazard value",
     main = "Treatment Effect Visualized as Piecewise Hazard",
     ylim=c(0,1.5))
abline(v = a, col = "red", lwd = 2, lty = 2)
# Add text for the regions
text(a/2, lambda_0 + 0.1, expression(lambda[0]), col = "blue")
text((3 + a)/2, lambda_1 + 0.1, expression(lambda[1]), col = "blue")
dev.off()

png(filename = "./figures/prop-hazard-viz.png")
x <- seq(0, 2.5, by = 0.01)
y <- ifelse(x <= a, x^2 * lambda_0, x^2*lambda_1)
plot(x, y, type = "l", col = "blue", lwd = 2,
     xlab = "x", ylab = "Hazard value",
     main = "Treatment Effect Visualized as Piecewise Hazard",
     ylim=c(0,7))
abline(v = a, col = "red", lwd = 2, lty = 2)
dev.off()
# Add text for the regions
# text(a/2, lambda_0 + 0.1, expression(lambda[t] ), col = "blue")
# text((3 + a)/2, lambda_1 + 0.1, expression(lambda[1]), col = "blue")

