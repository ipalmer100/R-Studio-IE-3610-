### Midterm 2 Two-Sample Summary Statistics Calculator (IE 3610) ###
# ctrl+L to clear console
rm(list = ls()) # Run to clear global environment

### INPUT ###
## First Sample (Use this also for case 5) ##
xbar = 7.5
s1 = 18.5769
σ1 = 0 # population std dev
m = 6
## Second Sample ##
ybar = 59.8
s2 = 0 # sample s2
σ2 = 1.1 # population s2
n = 38
## Setup ##
Δ_null = 0 # Ho difference in means variable
Ha = "<>" # Change value in parentheses: >, <, or <>
Δ_hat = 0 # Variable used as in type II error and sample size
alpha = 0.05
### END INPUT ###

# Determine alpha or alpha/2
if (Ha == "<>"){
  bounds = 2 # Ha: if not equal to (<>), then 2 ... if > or < , then 1
} else if (Ha == ">"){
  bounds = 1
} else if (Ha == "<"){
  bounds = 1
}


## Case 1: z test for two population means (known σ)
z_test = (xbar - ybar - Δ_null) / sqrt(((σ1^2)/m) + ((σ2^2)/n))
if (bounds == 2){
  z_critical = qnorm(alpha/2,lower.tail = FALSE)
  p_value = 2*pnorm(abs(z_test),lower.tail=FALSE)
} else {
  z_critical = qnorm(alpha,lower.tail = FALSE)
  p_value = pnorm(abs(z_test),lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject Ho!"
} else {
  Eval = "Fail to reject Ho!"
}


## Case 2: Non-normal population, large samples (unknown σ)
z_test = (xbar - ybar - Δ_null) / sqrt(((s1^2)/m) + ((s2^2)/n))
# p-value
if (bounds == 2){
  z_critical = qnorm(alpha/2,lower.tail = FALSE)
  p_value = 2*pnorm(abs(z_test),lower.tail=FALSE)
} else {
  z_critical = qnorm(alpha,lower.tail = FALSE)
  p_value = pnorm(abs(z_test),lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject Ho!"
} else {
  Eval = "Fail to reject Ho!"
}
# two-sided confidence interval
# one-sided confidence bounds


## Case 3: Two-sample t-tests for population mean (unknown σ) (Chapter 8.3)


## Case 5: Paired t-test Procedure
t_test = (xbar - Δ_null) / (s1/sqrt(m))
# p-value
if (bounds == 2){
  t_critical = qt(alpha/2,m-1,lower.tail = FALSE)
  p_value = 2*pt(abs(t_test),m-1,lower.tail = FALSE)
} else {
  t_critical = qt(alpha,m-1,lower.tail = FALSE)
  p_value = pt(abs(t_test),m-1,lower.tail = FALSE)
}
# reject?
if (p_value <= alpha){
  Eval = "Reject Ho!"
} else {
  Eval = "Fail to reject Ho!"
}
# confidence intervals
CI_lowerB = xbar - (qt(alpha,m-1,lower.tail = FALSE))*(s1/sqrt(m))
CI_upperB = xbar + (qt(alpha,m-1,lower.tail = FALSE))*(s1/sqrt(m))
CI_low = xbar - (qt(alpha/2,m-1,lower.tail = FALSE))*(s1/sqrt(m))
CI_high = xbar + (qt(alpha/2,m-1,lower.tail = FALSE))*(s1/sqrt(m))
PI_low = xbar - (qt(alpha/2,m-1,lower.tail = FALSE))*s1*sqrt(1 + (1/m))
PI_high = xbar + (qt(alpha/2,m-1,lower.tail = FALSE))*s1*sqrt(1 + (1/m))
# Evaluate
writeLines(c(paste("Case 5: Paired t-test"),
             paste("Test Stat =",t_test,"| Critical Value = ±",t_critical),
             paste("p-value =",p_value,"|","alpha =",alpha),Eval,
             paste("Two-sided CI: [",CI_low,",",CI_high,"]"),
             paste("One-sided Upper: (-∞,",CI_upperB,"]"),
             paste("One-sided Lower: [",CI_lowerB,", +∞)"),
             paste("Prediction Interval:[",PI_low,",",PI_high,"]")))

