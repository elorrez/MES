library(SoilR)


# We choose the ThreepSeriesModel() because the three pools are linked in series
# and there is no feedback between the different pools.

ThreepSeriesModel()


time <- seq(1/12, 100, 1/12)

input <- 10

ks <- c(0.8, 0.7, 0.09)

a21 <- 0.95 * 0.8
a32 <- 0.95 * 0.7

init <- c(0,0,0)

model_out <- ThreepSeriesModel(t = time, ks = ks, a21 = a21,
                               a32 = a32, C0 = init, In = 10)

n_pools <- getC(model_out)

for(i in 1:3){
  if(i == 1){
    plot(time, n_pools[, i],
         col = i, ylim = range(n_pools), type = "l",
         xlab = "Years", ylab = "Amount of Nitrogen")
    # axis(side = 1, at=seq(1,1212, by = 12), labels = seq_along(seq(1,1212, by = 12)))
    legend(x = 40, y = 80, legend = c("Ammonium", "Nitrate", "Plant"), fill = 1:3)
  } 
  if(i > 1) lines(time, n_pools[, i], col = i)
}
