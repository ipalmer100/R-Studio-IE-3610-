# File: Final Exam Plotter.R
# Course: IE 3610


# Clear plots
dev.off()  # But only if there IS a plot
# Clear console
cat("\014")  # ctrl+L
# Clear global environment
rm(list = ls())

library(ggplot2)

## IMPORT data1
x <- c(0.718, 0.808, 0.924, 1.000, 0.667, 0.529, 0.514, 0.559, 0.766, 0.470, 0.726, 0.762, 0.666, 0.562, 0.378, 0.779, 0.674, 0.858, 0.406, 0.927, 0.311, 0.319, 0.518, 0.687, 0.907, 0.638, 0.234, 0.781, 0.326, 0.433, 0.319, 0.238)
y <- c(0.428, 0.480, 0.493, 0.978, 0.318, 0.298, -0.224, 0.198, 0.326, -0.336, 0.765, 0.190, 0.066, -0.221, -0.898, 0.836, 0.126, 0.305, -0.577, 0.779, -0.707, -0.610, -0.648, -0.145, 1.007, -0.090, -1.132, 0.538, -1.098, -0.581, -0.862, -0.551)

data1 <- data.frame(x, y)

alpha=0.05


## data1 ANALYSIS
# Plots
plot(data1$x, data1$y, # Plot function
     pch = 19,         # Solid circle
     cex = 1,        # Make 1x size
     col = "#cc0000",  # Red
     main = "Astringency in Wine", # Graph title
     xlab = "Tannin Concentration", # X label
     ylab = "Perceived Astringency") # Y label

# Regression
abline(lm(y~x,data=data1),col='blue') # Add regression line to graph
model = lm(y~x,data=data1) # Slope and intercept

# Summary Statistics
sse <- sum((fitted(model) - data1$y)^2)
ssr <- sum((fitted(model) - mean(data1$y))^2)
sst <- ssr + sse
n = sum(table(data1))
s = sqrt(sse/(n-2))
r_squared = ssr/sst

# Output
summary(model) # Summary statistics for data1 set (r^2 = 'Multiple R-squared')
writeLines(c(paste("SSE:",sse),
             paste("SSR:",ssr),
             paste("SST:",sst),
             paste("n =",n),
             paste("Standard Deviation Point Estimate:",s),
             paste("R-Squared:",r_squared)))

fit=lm(y~x)
summary(fit)
anova(fit)
fitted=fitted(fit)
residuals=resid(fit)
par(mfrow=c(2,2))
plot(fit)

temp_var <-predict(fit, interval="prediction")
new_df <- cbind(data1, temp_var)

ggplot(new_df, aes(x=x, y=y)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_line(aes(y=lwr), color="red", linetype="dashed")+
  geom_line(aes(y=upr), color="red", linetype="dashed")+
  geom_smooth(method=lm, color='#2C3E50') 


confint(fit, level=1-alpha)

new_x <- data.frame(x=c(0.6,0.7)) 
predict(fit, newdata = new_x, interval = 'confidence', level=1-alpha)
predict(fit, newdata = new_x, interval = 'prediction', level=1-alpha)


mean.pred.intervals <- function(x, y, pred.x) {
  n <- length(y) # Find sample size
  lm.model <- lm(y ~ x) # Fit linear model
  y.fitted <- lm.model$fitted.values # Extract the fitted values of y
  S_xx=sum((x - mean(x))^2)
  S_yy=sum((y - mean(y))^2) 
  S_xy=sum((x - mean(x))*(y - mean(y)))
  cat(sprintf("S_xx=: %.3f\n", S_xx))
  cat(sprintf("S_yy=: %.3f\n", S_yy))
  cat(sprintf("S_xy=: %.3f\n", S_xy))
  
  # Coefficients of the linear model, beta0 and beta1
  b0 <- lm.model$coefficients[1]
  b1 <- lm.model$coefficients[2]
  cat(sprintf("beta0=: %.3f\n", b0))
  cat(sprintf("beta1=: %.3f\n", b1))
  alt_b1=S_xy/S_xx #alteranttive computing formula 
  alt_b0=mean(y)-alt_b1*mean(x) #alteranttive computing formula 
  cat(sprintf("alt_beta0=: %.3f\n", alt_b0))
  cat(sprintf("alt_beta1=: %.3f\n", alt_b1))
  
  # Find SSE and MSE
  sse <- sum((y - y.fitted)^2)
  cat(sprintf("SSE=: %.3f\n", sse))
  alt_sse=S_yy-alt_b1*S_xy #alteranttive computing formula 
  cat(sprintf("alt_SSE=: %.3f\n", alt_sse))
  mse <- sse / (n - 2)
  cat(sprintf("s^2=: %.3f\n", mse))
  alt_mse=alt_sse/(n-2) #alteranttive computing formula 
  cat(sprintf("alt_s^2=: %.3f\n", alt_mse))
  s <- sqrt(sse / (n - 2))
  cat(sprintf("s=: %.3f\n", s))
  alt_s=sqrt(alt_mse) #alteranttive computing formula 
  cat(sprintf("alt_s=: %.3f\n", alt_s))
  
  
  pred.y <- b1 * pred.x + b0 # Predict y at the given value of x (argument pred.x)
  cat(sprintf("hatY=: %.3f\n", pred.y))
  
  
  t.val <- qt(1-alpha/2, n - 2) # Critical value of t
  cat(sprintf("t=: %.3f\n", t.val))
  
  beta1.se.fit <-s / sqrt(S_xx) # standard error of beta 1
  cat(sprintf("s_beta1=: %.3f\n", beta1.se.fit))
  mean.se.fit <- s*sqrt((1 / n + (pred.x - mean(x))^2 / S_xx)) # Standard error of the mean estimate
  cat(sprintf("smeanY=: %.3f\n", mean.se.fit))
  pred.se.fit <- s*sqrt((1 + (1 / n) + (pred.x - mean(x))^2 / S_xx))  # Standard error of the prediction
  cat(sprintf("spredY=: %.3f\n", pred.se.fit))
  
  # Beta 1 Upper and Lower Confidence limits at alpha Confidence
  b1.conf.upper <- b1 + t.val * beta1.se.fit
  b1.conf.lower <- b1 - t.val * beta1.se.fit
  
  # Mean Estimate Upper and Lower Confidence limits at alpha Confidence
  mean.conf.upper <- pred.y + t.val * mean.se.fit
  mean.conf.lower <- pred.y - t.val * mean.se.fit
  
  # Prediction Upper and Lower Confidence limits at alpha Confidence
  pred.conf.upper <- pred.y + t.val * pred.se.fit
  pred.conf.lower <- pred.y - t.val * pred.se.fit
  
  
  
  # Build data.frame of upper and lower limits calculated above, as well as the predicted y and beta 1 values
  upper <- data.frame(rbind(round(b1.conf.upper, 3), round(mean.conf.upper, 3), round(pred.conf.upper, 3)))
  lower <- data.frame(rbind(round(b1.conf.lower, 3), round(mean.conf.lower, 3), round(pred.conf.lower, 3)))
  fit <- data.frame(rbind(round(b1, 3), round(pred.y, 3), round(pred.y, 3)))
  
  # Collect all into data.frame and rename columns
  results <- data.frame(cbind(lower, upper, fit), row.names = c('Coefficient', 'Mean', 'Prediction'))
  colnames(results) <- c('Lower', 'Upper', 'Fit')
  
  return(results)
}

mean.pred.intervals(x, y, 0.6)

mean.pred.intervals(x, y, 0.7)


