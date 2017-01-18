#' @title Modify TMLE to support parallel computation for g and Q.
#' @description
#' This is needed to use Savio or any multicore system effectively.
#'
#' @param Y Outcome
#' @param A Treatment indicator
#' @param W Covariates
#' @param family Gaussian or binomial
#' @param g.SL.library SL library for estimating g
#' @param Q.SL.library SL library for estimating Q
#' @param id Optional list of subject-specific ids.
#' @param verbose If TRUE outputs additional information during execution.
#' @param V Number of cross-validation folds to use when estimating g and Q.
#' @param sl_fn SuperLearner function to use for estimation of g and Q. By
#'   default this uses the normal SuperLearner function which is sequential.
#'   Other options would be to pass in mcSuperLearner, snowSuperLearner, or
#'   CV.SuperLearner. For functions that require additional arguments (e.g. the
#'   cluster argument of for snowSuperLearner) one should create a new function
#'   that overloads the call and sets that argument. This is what
#'   setup_parallel_tmle() does.
#' @param ... Remaining arguments are passed through to tmle::tmle().
#'
#' @export
#' @seealso setup_parallel_tmle
#'
# TODO: add examples to the code, document return object.
tmle_parallel = function(Y, A, W, family,
                         g.SL.library, Q.SL.library,
                         id = 1:length(Y), verbose = F,
                         V = 5,
                         sl_fn = SuperLearner::SuperLearner,
                         ...) {

  # Time our function execution.
  time_start = proc.time()

  X = cbind(A = A, W)

  # Estimate Q
  if (verbose) cat("Estimating Q using custom SuperLearner.\n")
  Q_init = sl_fn(Y = Y, X = X, family = family,
                SL.library = g.SL.library, id = id,
                cvControl = list(V = V))

  if (verbose) {
    cat("Q init fit:\n\n")
    print(Q_init)
  }

  # Create stacked dataframe with A = 1 and A = 0
  stacked_df = rbind(cbind(A = 1, W), cbind(A = 0, W))

  # Predict Q_1 and Q_0
  pred = predict(Q_init, stacked_df, onlySL = T)
  # Q should be an nx2 matrix (E(Y|A=0,W), E(Y|A=1,W))
  Q = cbind(pred$pred[seq(nrow(W) + 1, nrow(stacked_df))], pred$pred[1:nrow(W)])

  # Estimate g
  if (verbose) cat("Estimating g using custom SuperLearner.\n")
  g_fit = sl_fn(Y = A, X = W, family = "binomial",
                SL.library = g.SL.library, id = id,
                cvControl = list(V = V))

  if (verbose) {
    cat("g fit:\n\n")
    print(g_fit)
  }

  # Predict g1W: P(A = 1 | W)
  g1W = predict(g_fit, W, onlySL = TRUE)$pred

  # Pass results to tmle
  if (verbose) cat("Passing results to tmle.\n")
  tmle_result = tmle::tmle(Y = Y, A = A, W = W, family = family, id = id,
       g.SL.library = g.SL.library, Q.SL.library = Q.SL.library,
       # Pass in the predicted values that we fit manually.
       # Q should be an nx2 matrix (E(Y|A=0,W), E(Y|A=1,W))
       Q = Q,
       # Vector of P(A = 1 | W)
       g1W = g1W,
       verbose = verbose,
       ...
  )

  time_end = proc.time()

  # Add additional elments to tmle_result object.
  tmle_result$time = time_end - time_start
  tmle_result$sl_Q = Q_init
  tmle_result$sl_g = g_fit

  # Return our results.
  return(tmle_result)
}
