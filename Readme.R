library(readxl)

rawxlsx <- as.data.frame(read_excel("Kalibráció bükk.xlsx", skip = 1))

Measured <- rawxlsx[,1:2]

Kucsara <- 2.1*(1-2.718^(-Measured[,1]/2.1))+0.1*Measured[,1]

Para <- 1.911
ParametEq <- 1.911*(1-2.718^(-Measured[,1]/Para))+0,1*Measured[,1]
