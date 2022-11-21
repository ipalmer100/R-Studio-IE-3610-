### Midterm 2 Summary Statistics Calculator (IE 3610) ###
# ctrl+L to clear console
rm(list = ls()) # Run to clear global environment


### INPUT ###
## Cases 1 - 3 ##
  # Fill in Ho (null hypothesis) and other parameters
  Ho = 10 # Null hypothesis
  Ha = ">" # Change value in parentheses: >, <, or <>
  mean = 801 # Also known as x̄ or μ'
  n = 28 # Sample Size
  alpha = 0.01
  beta = 0.01 # Only needed for sample size questions
  # sample (s) OR population (s_pop = σ (Case 1 only))
  s = 117
  s_pop = 0.3
## Case 4 (proportion test)
  p_Ho = 0.5
  p_hat = 15/100
### END INPUT ### 

# RUN THIS: variable conditionals 
if (Ha == "<>"){
  bounds = 2 # Ha: if not equal to (<>), then 2 ... if > or < , then 1
} else if (Ha == ">"){
  bounds = 1
} else if (Ha == "<"){
  bounds = 1
}


## Case 1: z test for population mean (known σ) (Chapter 8.2)
test_stat = (mean - Ho)/(s_pop/sqrt(n)) #Statistic to reference by hand
abs_stat = abs((mean - Ho)/(s_pop/sqrt(n))) #Statistic for computation
# Evaluate p-value
if (bounds == 2){
  z_critical = qnorm(alpha/2,lower.tail = FALSE)
  p_value = 2*pnorm(abs_stat,lower.tail=FALSE)
} else {
  z_critical = qnorm(alpha,lower.tail = FALSE)
  p_value = pnorm(abs_stat,lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject Ho!"
} else {
  Eval = "Fail to reject Ho!"
}
# Evaluate type II error
if (Ha == "<>"){
  Type_II_crit = (z_critical + ((Ho - mean)/(s_pop/(sqrt(n)))))
  Type_II_crit_2 = (-z_critical + ((Ho - mean)/(s_pop/(sqrt(n)))))
  TypeIIError = pnorm(Type_II_crit) - pnorm(Type_II_crit_2)
} else if (Ha == ">"){
  Type_II_crit = (z_critical + ((Ho - mean)/(s_pop/(sqrt(n)))))
  TypeIIError = pnorm(Type_II_crit)
} else if (Ha == "<"){
  Type_II_crit = (-z_critical + ((Ho - mean)/(s_pop/(sqrt(n)))))
  TypeIIError = 1 - pnorm(Type_II_crit)
}
# Evaluate sample size
z_beta = qnorm(beta,lower.tail = FALSE)
sample_n = ceiling(((s_pop*(z_critical+z_beta))/(Ho-mean))^2)
# Evaluate rejection
writeLines(c(paste("Case 1 (z, KNOWN σ, estimate mean) =>"),
             paste("Test Stat = ±",test_stat,"| Critical Value = ±",z_critical),
             paste("p-value =",p_value,"|","alpha =", alpha),Eval,
             paste("Type II Error:",TypeIIError),
             paste("Sample size:",sample_n)))


## Case 2: Non-normal population, large sample (unknown σ) (Chapter 8.2)
test_stat = (mean - Ho)/(s/sqrt(n)) #Statistic to reference by hand
abs_stat = abs((mean - Ho)/(s/sqrt(n))) #Statistic for computation
# Evaluate p-value
if (bounds == 2){
  z_critical = qnorm(alpha/2,lower.tail = FALSE)
  p_value = 2*pnorm(abs_stat,lower.tail=FALSE)
} else {
  z_critical = qnorm(alpha,lower.tail = FALSE)
  p_value = pnorm(abs_stat,lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject Ho!"
} else {
  Eval = "Fail to reject Ho!"
}
# Evaluate type II error
if (Ha == "<>"){
  Type_II_crit = (z_critical + ((Ho - mean)/(s/(sqrt(n)))))
  Type_II_crit_2 = (-z_critical + ((Ho - mean)/(s/(sqrt(n)))))
  TypeIIError = pnorm(Type_II_crit) - pnorm(Type_II_crit_2)
} else if (Ha == ">"){
  Type_II_crit = (z_critical + ((Ho - mean)/(s/(sqrt(n)))))
  TypeIIError = pnorm(Type_II_crit)
} else if (Ha == "<"){
  Type_II_crit = (-z_critical + ((Ho - mean)/(s/(sqrt(n)))))
  TypeIIError = 1 - pnorm(Type_II_crit)
}
# Evaluate sample size
z_beta = qnorm(beta,lower.tail = FALSE)
sample_n = ceiling(((s*(z_critical+z_beta))/(Ho-mean))^2)
# Evaluate 
writeLines(c(paste("Case 2 (z, unknown σ, estimate mean) =>"),
             paste("Test Stat = ±",test_stat,"| Critical Value = ±",z_critical),
             paste("p-value =",p_value,"|","alpha =", alpha),Eval))


## Case 3: One-sample t test for population mean (unknown σ) (Chapter 8.3)
test_stat = (mean - Ho)/(s/sqrt(n))
abs_stat = abs((mean - Ho)/(s/sqrt(n)))
# Evaluate p-value
if (bounds == 2){
  t_critical = qt(alpha/2,n-1,lower.tail = FALSE)
  p_value = 2*pt(abs_stat,n-1,lower.tail=FALSE)
} else {
  t_critical = qt(alpha,n-1,lower.tail = FALSE)
  p_value = pt(abs_stat,n-1,lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject Ho!"
} else {
  Eval = "Fail to reject Ho!"
}
# confidence intervals
CI_high = mean + ((qt(alpha/2,n-1,lower.tail = FALSE))*s/sqrt(n)) # 2 sided hi
CI_low = mean - ((qt(alpha/2,n-1,lower.tail = FALSE))*s/sqrt(n)) # 2 sided lo
CI_lowerB = mean - ((qt(alpha,n-1,lower.tail = FALSE))*s/sqrt(n)) # 1 sided lo
CI_upperB = mean + ((qt(alpha,n-1,lower.tail = FALSE))*s/sqrt(n)) # 1 sided hi
# Evaluate rejection
writeLines(c(paste("Case 3 (t, estimate mean) =>"),
             paste("Test Stat = ±",test_stat,"| Critical Value = ±",t_critical),
             paste("p-value =",p_value,"|","alpha =", alpha),Eval,
             paste("Two-sided CI: [",CI_low,",",CI_high,"]"),
             paste("One-sided Upper: (-∞,",CI_upperB,"]"),
             paste("One-sided Lower: [",CI_lowerB,", +∞)")))


## Case 4: One-sample test for proportion (Chapter 8.4)
q_Ho = 1-p_Ho
test_stat = (p_hat - p_Ho)/(sqrt((p_Ho*q_Ho)/n))
abs_stat = abs((p_hat - p_Ho)/(sqrt((p_Ho*q_Ho)/n)))
# Evaluate p-value
if (bounds == 2){
  z_critical = qnorm(alpha/2,lower.tail = FALSE)
  p_value = 2*pnorm(abs_stat,lower.tail=FALSE)
} else {
  z_critical = qnorm(alpha,lower.tail = FALSE)
  p_value = pnorm(abs_stat,lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject Ho!"
} else {
  Eval = "Fail to reject Ho!"
}
# Type II Error Probability (Beta)
if (Ha == "<>"){
  Type_II_crit = ((p_Ho - p_hat) + (z_critical*(sqrt((p_Ho*q_Ho)/n))))/
    sqrt((p_hat*(1-p_hat))/n) - 
    ((p_Ho - p_hat) - (z_critical*(sqrt((p_Ho*q_Ho)/n))))/
    sqrt((p_hat*(1-p_hat))/n)
  TypeIIError = pnorm(Type_II_crit)
} else if (Ha == ">"){
  Type_II_crit = ((p_Ho - p_hat) + (z_critical*(sqrt((p_Ho*q_Ho)/n))))/
              sqrt((p_hat*(1-p_hat))/n)
  TypeIIError = pnorm(Type_II_crit)
} else if (Ha == "<"){
  Type_II_crit = ((p_Ho - p_hat) - (z_critical*(sqrt((p_Ho*q_Ho)/n))))/
    sqrt((p_hat*(1-p_hat))/n)
  TypeIIError = 1 - pnorm(Type_II_crit)
}
# Sample Size (n) - not yet coded
# Confidence Intervals (Homework 6 Question 4b)
one_lowerB = p_hat - qnorm(alpha,lower.tail = FALSE)*(sqrt((p_Ho*(q_Ho)/n)))
one_upperB = p_hat + qnorm(alpha,lower.tail = FALSE)*(sqrt((p_Ho*(q_Ho)/n)))
CI_low = p_hat - qnorm(alpha/2,lower.tail = FALSE)*(sqrt((p_Ho*(q_Ho)/n)))
CI_high = p_hat + qnorm(alpha/2,lower.tail = FALSE)*(sqrt((p_Ho*(q_Ho)/n)))
# Evaluate rejection
writeLines(c(paste("Case 4 (z, estimate proportion) =>"),
             paste("Test Stat = ±",test_stat,"| Critical Value = ±",z_critical),
             paste("Type II Error Probability:",TypeIIError),
             paste("p-value =",p_value,"|","alpha =",alpha),Eval,"*Bonus*",
             paste("Two-sided CI: [",CI_low,",",CI_high,"]"),
             paste("One-sided confidence bounds: Lower =",one_lowerB,
                   "Upper =",one_upperB)))

