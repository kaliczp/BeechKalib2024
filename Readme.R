library(readxl)

rawxlsx <- as.data.frame(read_excel("Kalibráció bükk.xlsx", skip = 1))

Measured <- rawxlsx[1:25,1:2]

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
     xlim = c( 0, 30), ylim = c(0,9))
curve(1.911*(1-2.718^(-x/1.911))+0.1*x, 0, 29, add = TRUE, lwd = 2)
legend("topleft",c("Data", "Model"), pch = c(16,NA), lwd = c(NA, 2),col = c("#4f81bd", "black"))
dev.off()
