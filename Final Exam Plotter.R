# File: Final Exam.R
# Course: IE 3610


# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L


## IMPORT DATA
XData = data.frame(Xhead=c(99.0,101.1,102.7,103.0,105.4,107.0,108.7,110.8,
                            112.1,112.4,113.6,113.8,115.1,115.4,120.0))
YData = data.frame(Yhead=c(28.8,27.9,27.0,25.2,22.8,21.5,20.9,19.6,
                             17.1,18.9,16.0,16.7,13.0,13.6,10.8))
QData = data.frame(XData,YData)
QData
## PLOTS

# Add some options
plot(QData$Xhead, QData$Yhead,
     pch = 19,         # Solid circle
     cex = 1,        # Make 100% size
     col = "#cc0000",  # Red
     main = "Title",
     xlab = "x-axis", # X label
     ylab = "y-axis") # Y label

# plot a regression line
abline(lm(Yhead~Xhead,data=QData),col='blue') # Add regression line to graph
lm(Yhead~Xhead,data=QData) # Regression statistics for equation

