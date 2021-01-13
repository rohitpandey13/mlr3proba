#' @template dens_measure
#' @templateVar title Int Brier loss
#' @templateVar inherit [MeasureDens]
#' @templateVar fullname MeasureDensIntBrierloss
#'
#' @description
#' Calculates the squared-loss for density estimation.
#'
#' @family Density estimation measures
#' @export
MeasureDensIntBrierloss = R6::R6Class("MeasureDensIntBrierloss",
        inherit = MeasureDens,
        public = list(
        #' @description
        #' Creates a new instance of this [R6][R6::R6Class] class.
        initialize = function() {
        super$initialize(
        id = "dens.ibl",
        range = c(- Inf, Inf),
        minimize = TRUE,
        predict_type = "pdf",
        properties = c("requires_learner", "requires_task", "requires_train_set"),
        man = "mlr3proba::mlr_measures_dens.ibl"
        )
        }
        ),
        active = list(
        #' @field eps
        #' Returns `eps` parameter, see `initialize`.
        eps = function(eps) {
           if (missing(eps)) {
              return(private$.eps)
           } else {
              assertNumeric(eps)
              private$.eps = eps
            }
          }),

          private = list(
           .eps = numeric(0),
           .score = function(prediction, learner, task, train_set, ...) {
          # return NA if learner not compatible
          # change `c("dens.kde")` to list of compatible learners

          train =  task$data(train_set)[[1]]
          x =  task$data()[[1]][!task$data()[[1]] %in% train]
          bw = unlist(prediction$distr$parameters("bandwidth")[[2]])


          kernel = get(as.character(subset(
                  distr6::listKernels(),
                  ShortName == unlist(prediction$distr$parameters("kernel")[[2]]),
                  ClassName)))$new(bw = bw)

          if (unlist(prediction$distr$parameters("kernel")[[2]]) == "Sigm") {
                   score = NA
           } else if (unlist(prediction$distr$parameters("kernel")[[2]]) == "Norm") {

                test_mat = lapply(x, function (x) x - train)
                cdfInt = mapply(function(x) kernel$cdfIntegral(upper = x), test_mat)
                ccdfInt = mapply(function(x) kernel$ccdfIntegral(lower = x), test_mat)
                energy = sum(kernel$energyBrier(train) / length(train)^2)

                if (length(train) == 1) {
                        score = mean(ccdfInt) + mean(cdfInt) - 1/2 * energy
                } else {
                        score = mean(colMeans(cdfInt) + colMeans(ccdfInt) - 1 / 2 * energy)
                }

                } else {

                        x_mat = sapply(x, function (x) x-train)
                        x_mat = as.numeric(x_mat)
                        train_mat = sapply(train, function(x) x - train)
                        train_mat = as.numeric(train_mat)
                        res = matrix(NA, length(x_mat), length(train_mat))
                        for (i in 1:length(x_mat)) {
                                for (j in 1:length(train_mat)) {
                                        res[i, j] = (kernel$cdfSquared2Norm(upper = x_mat[i], x = train_mat[j]) +
                                                             kernel$cdfSquared2Norm(upper = - x_mat[i], x = -train_mat[j]))
                                }
                        }
                        score =  sum(res) / (length(train)^3 * length(x))
          }

          return(score)

            }
            ))
