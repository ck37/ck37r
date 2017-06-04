#' @title Modify TMLE to support parallel computation for g and Q.
#' @description
#' This is needed to use Savio or any multicore system effectively.
#'
#' Another benefit is that the SuperLearner objects for the Q and g estimation
#' are saved. This allows one to examine the risk estimates for example.
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
#'   Defaults to 5 as tmle package does.
#' @param sl_fn SuperLearner function to use for estimation of g and possibly Q.
#'   By default this uses the normal SuperLearner function which is sequential.
#'   Other options would be to pass in mcSuperLearner, snowSuperLearner, or
#'   CV.SuperLearner. For functions that require additional arguments (e.g. the
#'   cluster argument of for snowSuperLearner) one should create a new function
#'   that overloads the call and sets that argument. This is what
#'   setup_parallel_tmle() does.
#' @param cvsl_fn CV.SuperLearner equivalent, can be used for estimating Q.
#' @param cvQinit If T, estimate Q using cvsl_fn, otherwise use sl_fn.
#' @param conserve_memory If T, remove the fitLibrary elements to save memory
#'   after we have done the relevant prediction.
#' @param ... Remaining arguments are passed through to tmle::tmle().
#'
#' @importFrom stats predict
#' @importFrom pryr object_size
#' @importFrom utils object.size
#' @export
#' @seealso setup_parallel_tmle
#'
# TODO: add examples to the code, document return object.
tmle_parallel = function(Y, A, W, family,
                         g.SL.library, Q.SL.library,
                         id = 1:length(Y), verbose = F,
                         V = 5,
                         sl_fn = SuperLearner::SuperLearner,
                         cvsl_fn = SuperLearner::CV.SuperLearner,
                         cvQinit = F,
                         conserve_memory = T,
                         ...) {

  # Time our function execution.
  time_start = proc.time()

  # Dataframe used for initial Q estimation.
  X = cbind(A = A, W)

  if (verbose) {
    cat("X dataframe object size: ",
        prettyNum(pryr::object_size(X) / 1024^2,
                  big.mark = ",", digits = 1), " MB\n")
  }

  # TODO: get cvQinit working if anyone requests it.
  if (cvQinit) stop("cvQinit = T not supported yet unfortunately.\n")

  # Create stacked dataframe with A = 1 and A = 0
  stacked_df = rbind(cbind(A = 1, W), cbind(A = 0, W))

  if (verbose) {
    cat("Stacked df dimensions:",
        prettyNum(dim(stacked_df), big.mark = ","), "\n")
    cat("Stacked dataframe object size: ",
        prettyNum(pryr::object_size(stacked_df) / 1024^2,
                  big.mark = ",", digits = 1), " MB\n")
  }

  # Estimate Q
  if (verbose) cat("Estimating Q using custom SuperLearner.\n")

  # NOTE: tmle::tmle() can optionally run CV.SuperLearner for Q but not g.
  Q_init = sl_fn(Y = Y, X = X, family = family,
                SL.library = Q.SL.library, id = id,
                cvControl = list(V = V), verbose = verbose)

  if (verbose) {
    cat("Q init fit:\n\n")
    print(Q_init)
    cat("\nQ init times:\n")
    print(Q_init$times)
    # pryr::object_size() fails on RF, so use object.size().
    cat("\nQ object size: ")
    print(object.size(Q_init), units = "MB")
  }

  # Predict Q_1 and Q_0
  pred = predict(Q_init, stacked_df, onlySL = T)

  # Q should be an nx2 matrix (E(Y|A=0,W), E(Y|A=1,W))
  Q = cbind(pred$pred[seq(nrow(W) + 1, nrow(stacked_df))],
            pred$pred[1:nrow(W)])

  # Clean up to conserve memory.
  rm(stacked_df, pred, X)

  if (conserve_memory) {
    # Remove fit library, which uses a lot of RAM.
    Q_init$fitLibrary = NULL
    # Other items also contribute.
    Q_init$library.predict = NULL
    Q_init$SL.predict = NULL
  }

  # Free up memory, esp. for clusters like Savio.
  gc()

  # Estimate g
  if (verbose) cat("Estimating g using custom SuperLearner.\n")
  g_fit = sl_fn(Y = A, X = W, family = "binomial",
                SL.library = g.SL.library, id = id,
                cvControl = list(V = V), verbose = verbose)

  if (verbose) {
    cat("g fit:\n\n")
    print(g_fit)
    cat("\ng times:\n")
    print(g_fit$times)
    cat("\ng object size: ")
    print(object.size(g_fit), units = "MB")
  }

  # Predict g1W: P(A = 1 | W)
  g1W = predict(g_fit, W, onlySL = TRUE)$pred

  if (conserve_memory) {
    # Remove fitLibrary, which uses a lot of memory.
    g_fit$fitLibrary = NULL
    # Other items also contribute.
    g_fit$library.predict = NULL
    g_fit$SL.predict = NULL
  }

  # Free up memory, esp. for clusters like Savio.
  gc()

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
