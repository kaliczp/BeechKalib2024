optInt <- function(Para, Prec, Int, Opt = "RMSE") {
    IntModel <- Para*(1-2.718^(-Prec/Para))+0.1*Prec
    if(Opt == "RMSE") {
        sqrt(mean((Int - IntModel)^2))
    } else {
        mean(abs(Int - IntModel)) #MAE
    }
}

## RMSE
optimize(optInt, interval = c(1.5,2.5), Prec = Measured$Prec, Int = Measured$Int)

## MAE
optimize(optInt, interval = c(1,3), Prec = Measured$Prec, Int = Measured$Int, Opt = "MAE")
