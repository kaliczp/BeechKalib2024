library(readxl)

rawxlsx <- as.data.frame(read_excel("Kalibráció bükk.xlsx", skip = 1))

Measured <- rawxlsx[1:25,1:2]

Kucsara <- 2.1*(1-2.718^(-Measured[,1]/2.1))+0.1*Measured[,1]

Para <- 1.911
ParametEq <- 1.911*(1-2.718^(-Measured[,1]/Para))+0.1*Measured[,1]

# Error
sqrt(sum((Measured[,2]-ParametEq)^2)/nrow(Measured))

## Figure
par(las = 1)
plot(Measured, pch = 16, col = "red",
     xlim = c( 0, 30), ylim = c(0,9))
points(cbind(Measured[,1], ParametEq), col = 2)
