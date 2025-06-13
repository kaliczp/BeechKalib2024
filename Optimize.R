optInt <- function(Para, Prec, Int, Opt = "RMSE") {
    IntModel <- Para*(1-2.718^(-Prec/Para))+0.1*Prec
    if(Opt == "RMSE") {
        sqrt(mean((Int - IntModel)^2))
    }
}

optimize(optInt, interval = c(1.5,2.5), Prec = Measured$Prec, Int = Measured$Int)
