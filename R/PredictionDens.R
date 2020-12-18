#' @title Prediction Object for Density
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerDens].
#'
#' The `task_type` is set to `"dens"`.
#'
#' @family Prediction
#' @export
#' @examples
#' library(mlr3)
#' task = mlr_tasks$get("precip")
#' learner = mlr_learners$get("dens.hist")
#' p = learner$train(task)$predict(task)
#' head(as.data.table(p))
PredictionDens = R6Class("PredictionDens",
  inherit = Prediction,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([TaskSDens])\cr
    #'   Task, used to extract defaults for `row_ids`.
    #'
    #' @param row_ids (`integer()`)\cr
    #'   Row ids of the predicted observations, i.e. the row ids of the test set.
    #'
    #' @param pdf (`numeric()`)\cr
    #'   Numeric vector of estimated probability density function, evaluated at values in test set.
    #'   One element for each observation in the test set.
    #'
    #' @param cdf (`numeric()`)\cr
    #'   Numeric vector of estimated cumulative distribution function, evaluated at values in test
    #'   set. One element for each observation in the test set.
    #'
    #'  @param pdfSquared2norm (`numeric()`)\cr
    #'   Numeric vector of estimated cumulative distribution function, evaluated at values in test
    #'   set. One element for each observation in the test set.
    #'
    #' @param check (`logical(1)`)\cr
    #'   If `TRUE`, performs argument checks and predict type conversions.
    initialize = function(task = NULL, row_ids = task$row_ids, pdf = NULL,
                          cdf = NULL, pdfSquared2norm = NULL, check = TRUE) {
      pdata = list(row_ids = row_ids, pdf = pdf, cdf = cdf, pdfSquared2norm = pdfSquared2norm)
      pdata = discard(pdata, is.null)
      class(pdata) = c("PredictionDataDens", "PredictionData")

      if (check) {
        pdata = check_prediction_data(pdata)
      }

      self$task_type = "dens"
      self$man = "mlr3proba::PredictionDens"
      self$data = pdata
      self$predict_types = intersect(c("pdf", "cdf", "pdfSquared2norm"), names(pdata))
    }
  ),

  active = list(
    #' @field pdf (`numeric()`)\cr
    #' Access the stored predicted probability density function.
    pdf = function() {
      self$data$pdf %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field cdf (`numeric()`)\cr
    #' Access the stored predicted cumulative distribution function.
    cdf = function() {
      self$data$cdf %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field pdfSquared2norm (`numeric()`)\cr
    #' Access the stored predicted cumulative distribution function.
    pdfSquared2norm = function() {
      self$data$pdfSquared2norm %??% rep(NA_real_, length(self$data$row_ids))
    }
  )
)


#' @export
as.data.table.PredictionDens = function(x, ...) { # nolint
  tab = as.data.table(x$data[c("row_ids", "pdf", "cdf", "pdfSquared2norm")])
  setnames(tab, "row_ids", "row_id")[]
}
