# File: Final Exam Plotter.R
# Course: IE 3610


# Clear plots
dev.off()  # But only if there IS a plot
# Clear console
cat("\014")  # ctrl+L
# Clear global environment
rm(list = ls())


## IMPORT DATA
Data = data.frame(Xhead=c(0.718,0.808,0.924,1.000,0.667,0.529,0.514,0.559,0.766,
                          0.470,0.726,0.762,0.666,0.562,0.378,0.779,0.674,0.858,
                          0.406,0.927,0.311,0.319,0.518,0.687,0.907,0.638,0.234,
                          0.781,0.326,0.433,0.319,0.238),
                   Yhead=c(0.428,0.480,0.493,0.978,0.318,0.298,-0.224,0.198,
                           0.326,-0.336,0.765,0.190,0.066,-0.221,-0.898,0.836,
                           0.126,0.305,-0.577,0.779,-0.707,-0.610,-0.648,-0.145,
                           1.007,-0.090,-1.132,0.538,-1.098,-0.581,-0.862,
                           -0.551))
Data # display data frame


## DATA ANALYSIS
# Plots
plot(Data$Xhead, Data$Yhead, # Plot function
     pch = 19,         # Solid circle
     cex = 1,        # Make 1x size
     col = "#cc0000",  # Red
     main = "Astringency in Wine", # Graph title
     xlab = "Tannin Concentration", # X label
     ylab = "Perceived Astringency") # Y label

# Regression
abline(lm(Yhead~Xhead,data=Data),col='blue') # Add regression line to graph
model = lm(Yhead~Xhead,data=Data) # Slope and intercept

# Summary Statistics
sse <- sum((fitted(model) - Data$Yhead)^2)
ssr <- sum((fitted(model) - mean(Data$Yhead))^2)
sst <- ssr + sse
n = sum(table(Data))
s = sqrt(sse/(n-2))
r_squared = ssr/sst

# Output
summary(model) # Summary statistics for data set (r^2 = 'Multiple R-squared')
writeLines(c(paste("SSE:",sse),
             paste("SSR:",ssr),
             paste("SST:",sst),
             paste("n =",n),
             paste("Standard Deviation Point Estimate:",s),
             paste("R-Squared:",r_squared)))
            
