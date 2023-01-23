### Midterm 2 Two-Sample Summary Statistics Calculator (IE 3610) ###

dev.off()  # But only if there IS a plot
# Clear console
cat("\014")  # ctrl+L
# Clear global environment
rm(list = ls())

# ------------------------------- INPUT ------------------------------------- #
## Setup ##
Δ_null = 0 # Ho and Ha difference in means variable
Ha = "<" # Alternative hypothesis input - change value to >, <, or <>
alpha = 0.05
## First Data Sample (Use this also for case 5) ##
xbar = 801
s1 = 117
σ1 = 1.2 # population std dev
m = 490
## Second Data Sample ##
ybar = 780
s2 = 72 # sample s2
σ2 = 1.1 # population s2
n = 580
## Optional ##
Δ_hat = 0 # Case 1 type II error and sample size 
mu1 = 0 # Case 4 pooled true mean 1
mu2 = 0 # Case 4 pooled true mean 2
beta = 0.01 # sample size calculations
## Case 6 Only ##
p_hat_1 = 170/490
p_hat_2 = 240/580
p1 = 0.5
p2 = 0.25
m = m
n = n
# ----------------------------- END INPUT --------------------------------- #

# Determine alpha or alpha/2
if (Ha == "<>"){
  bounds = 2 # Ha: if not equal to (<>), then 2 ... if > or < , then 1
} else if (Ha == ">"){
  bounds = 1
} else if (Ha == "<"){
  bounds = 1
}


## Case 1: z test for two population means (known σ)
{
  z_test = (xbar - ybar - Δ_null) / sqrt(((σ1^2)/m) + ((σ2^2)/n))
  σ = sqrt((σ1^2/m) + (σ2^2/n)) # population sd for type II error & sample size
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
  # confidence intervals
  CI_high = (xbar - ybar) + 
    ((qnorm(alpha/2,lower.tail = FALSE))*(sqrt(((σ1^2)/m) + ((σ2^2)/n)))) # 2 hi
  CI_low = (xbar - ybar) - 
    ((qnorm(alpha/2,lower.tail = FALSE))*(sqrt(((σ1^2)/m) + ((σ2^2)/n)))) # 2 lo
  CI_lowerB = (xbar - ybar) - 
    ((qnorm(alpha,lower.tail = FALSE))*(sqrt(((σ1^2)/m) + ((σ2^2)/n)))) # 1 side lo
  CI_upperB = (xbar - ybar) + 
    ((qnorm(alpha,lower.tail = FALSE))*(sqrt(((σ1^2)/m) + ((σ2^2)/n)))) # 1 side hi
  
  # type II error
  if (Ha == "<>"){
    Type_II_crit = (z_critical + ((Δ_null - Δ_hat/σ)))
    Type_II_crit_2 = (-z_critical + ((Δ_null - Δ_hat/σ)))
    TypeIIError = pnorm(Type_II_crit) - pnorm(Type_II_crit_2)
  } else if (Ha == ">"){
    Type_II_crit = (z_critical + ((Δ_null - Δ_hat/σ)))
    TypeIIError = pnorm(Type_II_crit)
  } else if (Ha == "<"){
    Type_II_crit = (-z_critical + ((Δ_null - Δ_hat/σ)))
    TypeIIError = 1 - pnorm(Type_II_crit)
  }
  # sample size
  z_beta = qnorm(beta,lower.tail = FALSE)
  sample_n = ceiling((((σ1^2)+(σ2^2))*((z_critical+z_beta)^2)/(Δ_hat-Δ_null))^2)
  # evaluate
  writeLines(c(paste("Case 1: Two Sample z-test"),
               paste("Test Stat =",z_test,"| Critical Value = ±",z_critical),
               paste("p-value =",p_value,"|","alpha =",alpha),Eval,
               paste("Two-sided CI: [",CI_low,",",CI_high,"]"),
               paste("One-sided Upper: (-∞,",CI_upperB,"]"),
               paste("One-sided Lower: [",CI_lowerB,", +∞)"),
               paste("Type II Error:",TypeIIError),
               paste("Sample size:",sample_n)))
}

## Case 2: Non-normal population, large samples (unknown σ)
{
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
  # confidence intervals
  CI_high = (xbar - ybar) + 
    ((qnorm(alpha/2,lower.tail = FALSE))*(sqrt(((s1^2)/m) + ((s2^2)/n)))) # 2 hi
  CI_low = (xbar - ybar) - 
    ((qnorm(alpha/2,lower.tail = FALSE))*(sqrt(((s1^2)/m) + ((s2^2)/n)))) # 2 lo
  CI_lowerB = (xbar - ybar) - 
    ((qnorm(alpha,lower.tail = FALSE))*(sqrt(((s1^2)/m) + ((s2^2)/n)))) # 1 side lo
  CI_upperB = (xbar - ybar) + 
    ((qnorm(alpha,lower.tail = FALSE))*(sqrt(((s1^2)/m) + ((s2^2)/n)))) # 1 side hi
  writeLines(c(paste("Case 2: Large Two Sample z-test"),
               paste("Test Stat =",z_test,"| Critical Value = ±",z_critical),
               paste("p-value =",p_value,"|","alpha =",alpha),Eval,
               paste("Two-sided CI: [",CI_low,",",CI_high,"]"),
               paste("One-sided Upper: (-∞,",CI_upperB,"]"),
               paste("One-sided Lower: [",CI_lowerB,", +∞)")))
}

## Case 3: Two-sample t-tests for population mean (unknown σ) (Chapter 8.3)
{
  t_test = (xbar - ybar - Δ_null) / sqrt(((s1^2)/m) + ((s2^2)/n))
  v = ceiling(((((s1^2)/m) + ((s2^2)/n))^2)/
    ((((((s1^2)/m))^2)/(m-1)) + (((((s2^2)/n))^2)/(n-1)))) # degrees of freedom :(
  # p-value
  if (bounds == 2){
    t_critical = pt(alpha/2,v,lower.tail = FALSE)
    p_value = 2*pt(abs(t_test),v,lower.tail=FALSE)
  } else {
    t_critical = pt(alpha,v,lower.tail = FALSE)
    p_value = pt(abs(t_test),v,lower.tail=FALSE)
  }
  if (p_value <= alpha){
    Eval = "Reject Ho!"
  } else {
    Eval = "Fail to reject Ho!"
  }
  # confidence intervals
  CI_high = (xbar - ybar) + 
    ((qt(alpha/2,v,lower.tail = FALSE))*(sqrt(((s1^2)/m) + ((s2^2)/n)))) # 2 hi
  CI_low = (xbar - ybar) - 
    ((qt(alpha/2,v,lower.tail = FALSE))*(sqrt(((s1^2)/m) + ((s2^2)/n)))) # 2 lo
  CI_lowerB = (xbar - ybar) - 
    ((qt(alpha,v,lower.tail = FALSE))*(sqrt(((s1^2)/m) + ((s2^2)/n)))) # 1 side lo
  CI_upperB = (xbar - ybar) + 
    ((qt(alpha,v,lower.tail = FALSE))*(sqrt(((s1^2)/m) + ((s2^2)/n)))) # 1 side hi
  # evaluate
  writeLines(c(paste("Case 3: Two Sample t-test"),
               paste("Test Stat =",t_test,"| Critical Value = ±",t_critical),
               paste("p-value =",p_value,"|","alpha =",alpha),Eval,
               paste("Two-sided CI: [",CI_low,",",CI_high,"]"),
               paste("One-sided Upper: (-∞,",CI_upperB,"]"),
               paste("One-sided Lower: [",CI_lowerB,", +∞)")))
}

## Case 4: Pooled t-test (both populations have the same variance)
{
  s_pool = (((m-1)/(m+n-2))*s1^2) + (((n-1)/(m+n-2))*s2^2)
  t_test = ((xbar - ybar) - (mu1 - mu2)) / sqrt(s_pool*((1/m) + (1/n)))
  # p-value
  if (bounds == 2){
    t_critical = pt(alpha/2,n+m-2,lower.tail = FALSE)
    p_value = 2*pt(abs(t_test),n+m-2,lower.tail=FALSE)
  } else {
    t_critical = pt(alpha,n+m-2,lower.tail = FALSE)
    p_value = pt(abs(t_test),n+m-2,lower.tail=FALSE)
  }
  if (p_value <= alpha){
    Eval = "Reject Ho!"
  } else {
    Eval = "Fail to reject Ho!"
  }
  # confidence intervals
  CI_low = (xbar-ybar) - (qt(alpha,n+m-2,lower.tail = FALSE))* # 2-sided low
    (sqrt(s_pool*((1/m)+(1/n))))
  CI_high = (xbar-ybar) + (qt(alpha,n+m-2,lower.tail = FALSE))* # 2-sided high
    (sqrt(s_pool*((1/m)+(1/n))))
  CI_lowerB = (xbar-ybar) - (qt(alpha/2,n+m-2,lower.tail = FALSE))* # 1-sided low
    (sqrt(s_pool*((1/m)+(1/n))))
  CI_upperB = (xbar-ybar) + (qt(alpha/2,n+m-2,lower.tail = FALSE))* # 1-sided high
    (sqrt(s_pool*((1/m)+(1/n))))
  # evaluate
  writeLines(c(paste("Case 4: Pooled t-test"),
               paste("Test Stat =",t_test,"| Critical Value = ±",t_critical),
               paste("p-value =",p_value,"|","alpha =",alpha),Eval,
               paste("Two-sided CI: [",CI_low,",",CI_high,"]"),
               paste("One-sided Upper: (-∞,",CI_upperB,"]"),
               paste("One-sided Lower: [",CI_lowerB,", +∞)")))
}

## Case 5: Paired t-test Procedure
{
  t_test = (xbar - Δ_null) / (s1/sqrt(m))
  # p-value
  if (bounds == 2){
    t_critical = qt(alpha/2,m-1,lower.tail = FALSE)
    p_value = 2*pt(abs(t_test),m-1,lower.tail = FALSE)
  } else {
    t_critical = qt(alpha,m-1,lower.tail = FALSE)
    p_value = pt(abs(t_test),m-1,lower.tail = FALSE)
  }
  if (p_value <= alpha){
    Eval = "Reject Ho!"
  } else {
    Eval = "Fail to reject Ho!"
  }
  # confidence intervals
  CI_lowerB = xbar - (qt(alpha,m-1,lower.tail = FALSE))*(s1/sqrt(m)) # 1 side lo
  CI_upperB = xbar + (qt(alpha,m-1,lower.tail = FALSE))*(s1/sqrt(m)) # 1 side hi
  CI_low = xbar - (qt(alpha/2,m-1,lower.tail = FALSE))*(s1/sqrt(m)) # 2 side lo
  CI_high = xbar + (qt(alpha/2,m-1,lower.tail = FALSE))*(s1/sqrt(m)) # 2 side hi
  PI_low = xbar - (qt(alpha/2,m-1,lower.tail = FALSE))*s1*sqrt(1 + (1/m)) # 2 side lo
  PI_high = xbar + (qt(alpha/2,m-1,lower.tail = FALSE))*s1*sqrt(1 + (1/m)) # 2 side hi
  # Evaluate
  writeLines(c(paste("Case 5: Paired t-test"),
               paste("Test Stat =",t_test,"| Critical Value = ±",t_critical),
               paste("p-value =",p_value,"|","alpha =",alpha),Eval,
               paste("Two-sided CI: [",CI_low,",",CI_high,"]"),
               paste("One-sided Upper: (-∞,",CI_upperB,"]"),
               paste("One-sided Lower: [",CI_lowerB,", +∞)"),
               paste("Prediction Interval:[",PI_low,",",PI_high,"]")))
  }

## Case 6: Difference in Proportions
{
  q1 = 1-p1
  q2 = 1-p2
  pbar = ((m*p1)+(n*p2))/(m+n)
  qbar = ((m*q1)+(n*q2))/(m+n)
  σ = sqrt(((p1*q1)/m)+((p2*q2)/n))
  p_hat = ((m/(m+n))*p_hat_1) + ((n/(m+n))*p_hat_2)
  q_hat = 1-p_hat
  z_test = (p_hat_1 - p_hat_2)/sqrt((p_hat*(1-p_hat))*((1/m)+(1/n)))
  # p-value
  if (bounds == 2){
    z_critical = qnorm(alpha/2,lower.tail = FALSE)
    p_value = 2*pnorm(abs(z_test),lower.tail = FALSE)
  } else {
    z_critical = qnorm(alpha,lower.tail = FALSE)
    p_value = pnorm(abs(z_test),lower.tail = FALSE)
  }
  if (p_value <= alpha){
    Eval = "Reject Ho!"
  } else {
    Eval = "Fail to reject Ho!"
  }
  # confidence intervals
  CI_lowerB = (p_hat_1 - p_hat_2) - (qnorm(alpha,lower.tail = FALSE))*
    sqrt(((p_hat_1*(1-p_hat_1))/m) + ((p_hat_2*(1-p_hat_2))/n)) # 1 side lo
  CI_upperB = (p_hat_1 - p_hat_2) + (qnorm(alpha,lower.tail = FALSE))*
    sqrt(((p_hat_1*(1-p_hat_1))/m) + ((p_hat_2*(1-p_hat_2))/n)) # 1 side hi
  CI_low = (p_hat_1 - p_hat_2) - (qnorm(alpha/2,lower.tail = FALSE))*
    sqrt(((p_hat_1*(1-p_hat_1))/m) + ((p_hat_2*(1-p_hat_2))/n)) # 2 side lo
  CI_high = (p_hat_1 - p_hat_2) + (qnorm(alpha/2,lower.tail = FALSE))*
    sqrt(((p_hat_1*(1-p_hat_1))/m) + ((p_hat_2*(1-p_hat_2))/n)) # 2 side hi
  # type II error formula chosen on alternative hypothesis input
  if (Ha == "<>"){
    Type_II_crit = ((z_critical*(sqrt(pbar*qbar*((1/m)+(1/n)))))-(p1-p2))/σ
    Type_II_crit_2 = ((-z_critical*(sqrt(pbar*qbar*((1/m)+(1/n)))))-(p1-p2))/σ
    TypeIIError = pnorm(Type_II_crit) - pnorm(Type_II_crit_2)
  } else if (Ha == ">"){
    Type_II_crit = ((z_critical*(sqrt(pbar*qbar*((1/m)+(1/n)))))-(p1-p2))/σ
    TypeIIError = pnorm(Type_II_crit)
  } else if (Ha == "<"){
    Type_II_crit = ((-z_critical*(sqrt(pbar*qbar*((1/m)+(1/n)))))-(p1-p2))/σ
    TypeIIError = 1 - pnorm(Type_II_crit)
  }
  # sample size
  sample_n = ceiling((((qnorm(alpha,lower.tail = FALSE)*(sqrt((p1+p2)*(q1+q1))/2))+
    (qnorm(beta,lower.tail = FALSE)*(sqrt((p1*q1)+(p2*q2))))^2)/((p1-p2)^2)))
  # evaluate
  writeLines(c(paste("Case 6: Difference in Proportions"),
               paste("Test Stat =",z_test,"| Critical Value = ±",z_critical),
               paste("p-value =",p_value,"|","alpha =",alpha),Eval,
               paste("Two-sided CI: [",CI_low,",",CI_high,"]"),
               paste("One-sided Upper: (-∞,",CI_upperB,"]"),
               paste("One-sided Lower: [",CI_lowerB,", +∞)"),
               paste("Type II Error:",TypeIIError),
               paste("Power of Test:",1 - TypeIIError),
               paste("Sample size:",sample_n)))
}
