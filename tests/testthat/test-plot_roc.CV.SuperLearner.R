library(SuperLearner)
library(ck37r)

data(Boston, package = "MASS")

set.seed(1)

# This will generate some warnings about glm fitted probabilities.
suppressWarnings({
  cvsl = CV.SuperLearner(Boston$chas, subset(Boston, select = -chas),
                         family = binomial(),
                         cvControl = list(V = 2, stratifyCV = T),
                         SL.library = c("SL.mean", "SL.glm"))
})

plot_roc(cvsl, y = Boston$chas)

# Deprecated version, will generate a warning.
suppressWarnings({
  cvsl_plot_roc(cvsl, y = Boston$chas)
})
