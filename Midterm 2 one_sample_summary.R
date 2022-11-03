### Midterm 2 Summary Statistics Calculator (IE 3610) ###
# ctrl+L to clear console
rm(list = ls()) # Run to clear global environment


### DATA ENTRY ###
# Fill in Ho (null hypothesis) and otherparameters
Null = 1
bounds = 1 # Ha: if =/, then 2 ... if > or < , then 1
n = 20
alpha = 0.01
p_hat = 41/51 # (Case 4 only)
# Fill in mean and standard deviation
mean = 1.24
# sample (s) OR population (s_pop = σ (Case 1 only))
s = 0.45
s_pop = 0
### END DATA ENTRY ### 


## Case 1: z test for population mean (known σ) (Chapter 8.2)
critical = (mean - Null)/(s_pop/sqrt(n))
# Evaluate p-value
if (bounds == 2){
  z = qnorm(alpha/2,lower.tail = FALSE)
  p_value = 2*pnorm(critical,lower.tail=FALSE)
} else {
  z = qnorm(alpha,lower.tail = FALSE)
  p_value = pnorm(critical,lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject null!"
} else {
  Eval = "Fail to reject null!"
}
# Evaluate rejection
print(paste("Case 1 (z, KNOWN σ, estimate mean) =>"))
print(paste("critical =",critical,"| z =",z))
print(paste("p-value =",p_value,"|","alpha =", alpha))
print(Eval)


## Case 3: Non-normal population, large sample (unknown σ) (Chapter 8.2)
critical = (mean - Null)/(s/sqrt(n))
# Evaluate p-value
if (bounds == 2){
  z = qnorm(alpha/2,lower.tail = FALSE)
  p_value = 2*pnorm(critical,lower.tail=FALSE)
} else {
  z = qnorm(alpha,lower.tail = FALSE)
  p_value = pnorm(critical,lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject null!"
} else {
  Eval = "Fail to reject null!"
}
# Evaluate rejection
print(paste("Case 3 (z, unknown σ, estimate mean) =>"))
print(paste("critical =",critical,"| z =",z))
print(paste("p-value =",p_value,"|","alpha =", alpha))
print(Eval)


## Case 4: One-sample test for proportion (Chapter 8.4)
q_Null = 1-Null
critical = (p_hat - Null)/(sqrt((Null*q_Null)/n))
# Evaluate p-value
if (bounds == 2){
  z = qnorm(alpha/2,lower.tail = FALSE)
  p_value = 2*pnorm(critical,lower.tail=FALSE)
} else {
  z = qnorm(alpha,lower.tail = FALSE)
  p_value = pnorm(critical,lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject null!"
} else {
  Eval = "Fail to reject null!"
}
# Confidence Intervals (Homework 6 Question 4b)
one_lowerB = p_hat - qnorm(alpha,lower.tail = FALSE)*(sqrt((Null*(q_Null)/n)))
one_upperB = p_hat + qnorm(alpha,lower.tail = FALSE)*(sqrt((Null*(q_Null)/n)))
CI_low = p_hat - qnorm(alpha/2,lower.tail = FALSE)*(sqrt((Null*(q_Null)/n)))
CI_high = p_hat + qnorm(alpha/2,lower.tail = FALSE)*(sqrt((Null*(q_Null)/n)))
# Evaluate rejection
print(paste("Case 4 (z, estimate proportion) =>"))
print(paste("critical =",critical,"| z =",z))
print(paste("p-value =",p_value,"|","alpha =", alpha))
print(Eval)
print(paste("*Bonus* two-sided CI: [",CI_low,",",CI_high,"]"))
print(paste("*Bonus* one-sided bounds: Lower Bound =",one_lowerB,
            "Upper Bound =",one_upperB))


## Case 5: One-sample t test for population mean (unknown σ) (Chapter 8.3)
critical = (mean - Null)/(s/sqrt(n))
# Evaluate p-value
if (bounds == 2){
  t = qt(alpha/2,n-1,lower.tail = FALSE)
  p_value = 2*pt(critical,n-1,lower.tail=FALSE)
} else {
  t = qt(alpha,n-1,lower.tail = FALSE)
  p_value = pt(critical,n-1,lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject null!"
} else {
  Eval = "Fail to reject null!"
}
# Evaluate rejection
print(paste("Case 5 (t, estimate mean) =>"))
print(paste("critical =",critical,"| t =",t))
print(paste("p-value =",p_value,"|","alpha =", alpha))
print(Eval)

