library(SuperLearner)
library(ck37r)

data(Boston, package = "MASS")

set.seed(1)
cvsl = CV.SuperLearner(Boston$chas, subset(Boston, select = -chas), family = binomial(),
                       cvControl = list(V = 2, stratifyCV = T),
                       SL.library = c("SL.mean", "SL.glmnet"))
cvsl_auc(cvsl)
