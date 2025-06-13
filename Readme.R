library(readxl)

rawxlsx <- as.data.frame(read_excel("Kalibráció bükk.xlsx", skip = 1))

Measured <- rawxlsx[1:25,1:2]
names(Measured) <- c("Prec", "Interc")

Kucsara <- 2.1*(1-2.718^(-Measured[,1]/2.1))+0.1*Measured[,1]

Para <- 1.911
ParametEq <- Para*(1-2.718^(-Measured[,1]/Para))+0.1*Measured[,1]
ParametEqFun <- function(x) {1.911*(1-2.718^(-x/1.911))+0.1*x}
ParametEqFun(Measured[,1])

## Error
sqrt(sum((Measured[,2]-ParametEq)^2)/nrow(Measured))

## Figure
pdf(width = 16/2.54, height = 9/2.54, pointsize = 9)
par(las = 1, mar = c(4.1,4.1,0.1,0.5),
    xaxs = "i", yaxs = "i")
plot(Measured, pch = 16, col = "#4f81bd",
     xlab = "Precipitation sum [mm]",
     ylab = "Interception [mm]",
     xlim = c( 0, 30), ylim = c(0,9))
curve(1.911*(1-2.718^(-x/1.911))+0.1*x, 0, 29, add = TRUE, lwd = 2)
curve(1.9323*(1-2.718^(-x/1.9323))+0.1*x, 0, 29, add = TRUE, lwd = 2, col = "lightgray")
legend("topleft",c("Measured data", "Modified Merriam model RMSE", "Modified Merriam model MAE"), pch = c(16,NA,NA), lwd = c(NA, 2, 2),col = c("#4f81bd", "black", "lightgray"))
dev.off()

## Lack-of-fit
Model1 <- 1.911*(1-2.718^(-Measured$Prec/1.911))+0.1*Measured$Prec
plot(Model1, Measured$Interc - Model1, ylab = "Residuals", xlab = "Fitted")
axis(2, at = 0, tck = 1, lty = "dotted")
