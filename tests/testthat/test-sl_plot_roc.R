library(SuperLearner)
library(ck37r)

data(Boston, package = "MASS")

set.seed(1)
sl = SuperLearner(Boston$chas, subset(Boston, select = -chas),
                  family = binomial(), cvControl = list(V = 2),
                  SL.library = c("SL.mean", "SL.glm"))

sl

sl_plot_roc(sl, Y = Boston$chas)
