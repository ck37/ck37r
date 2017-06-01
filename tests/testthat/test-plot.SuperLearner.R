library(SuperLearner)
library(ck37r)

data(Boston, package = "MASS")

set.seed(1)
sl = SuperLearner(Boston$medv, subset(Boston, select = -medv), family = gaussian(),
                  SL.library = c("SL.mean", "SL.glmnet"))

sl
plot(sl, Y = Boston$chas)
