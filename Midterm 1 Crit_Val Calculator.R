### Critical Value Calculator (IE 3610) ###
# Extra file to help understand R's critical value functions
# ctrl+L to clear console
rm(list = ls()) # Run to clear global environment


## z, t, chi-squared Critical Value calculator
# Inputs (type here)
Confidence = 0.95 # Enter central area
n = 20 # Enter n

# Extra calculations (no input needed)
df = n-1
alpha = 1 - Confidence
alpha_0.5 = (1 - Confidence)/2
# Outputs (z)
z = qnorm(alpha_0.5,lower.tail = FALSE) # two-sided CI
z_1 = qnorm(alpha,lower.tail = FALSE) # one-sided CI
# Outputs (t)
t = qt(alpha_0.5,df,lower.tail=FALSE) # two-sided
t_1 = qt(alpha,df,lower.tail=FALSE) # one-sided
# Outputs (chi squared)
chiHI = qchisq(1-alpha_0.5,df,lower.tail = FALSE) # two-sided upper
chiLO = qchisq(alpha_0.5,df,lower.tail = FALSE) # two-sided lower
chi_1HI = qchisq(1-alpha,df,lower.tail = FALSE) # one-sided upper
chi_1LO = qchisq(alpha,df,lower.tail = FALSE) # one-sided lower


## Examples: z, t, chi-squared Critical Values
qnorm(0.05,lower.tail=FALSE) # 90% two-sided CI
qnorm(0.025,lower.tail=FALSE) # 95% two-sided CI
qnorm(0.01,lower.tail = FALSE) # 98% two-sided CI
qnorm(0.005,lower.tail = FALSE) # 99% two-sided CI

qt(0.1,15,lower.tail=FALSE) # t (98% confidence, n = 16 ==> df = 15)
qt(0.025,6,lower.tail=FALSE) # Central area = 0.95, df = 10,
qt(0.025,20,lower.tail=FALSE) # Central area = 0.95, df = 20
qt(0.01,25,lower.tail=FALSE) # Upper-tail area = 0.01, df = 25
qt(0.005,10,lower.tail=TRUE)

qchisq(0.1,15,lower.tail=FALSE) # chi square (alpha = 0.1, df = 15)
qchisq(0.1,25,lower.tail=FALSE)
qchisq(0.01,25,lower.tail=FALSE)
qchisq(0.005,25,lower.tail=FALSE)
qchisq(0.99,25,lower.tail=FALSE)
qchisq(0.995,25,lower.tail=FALSE)

# 95% Confidence, two-sided interval (alpha/2) for z
qnorm(0.975) # Use this
qnorm(0.025) # Do NOT use this
qnorm(0.025,lower.tail=FALSE) # Use this instead

# 95% Confidence, one-sided interval (alpha) for z
qnorm(0.95) # Use this
qnorm(0.05) # Do NOT use this
qnorm(0.05,lower.tail=FALSE) # Use this instead


