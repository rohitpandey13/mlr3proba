#' @template dens_measure
#' @templateVar title Squared loss
#' @templateVar inherit [MeasureDens]
#' @templateVar fullname MeasureDensSquaredloss
#'
#' @description
#' Calculates the squared-loss for density estimation.
#'
#' @family Density estimation measures
#' @export
MeasureDensSquaredloss = R6::R6Class("MeasureDensSquaredloss",
        inherit = MeasureDens,
        public = list(
        #' @description
        #' Creates a new instance of this [R6][R6::R6Class] class.
        initialize = function() {
         super$initialize(
        id = "dens.squared",
        range = c(- Inf, Inf),
        minimize = TRUE,
        predict_type = "distr",
        properties = c("requires_learner", "requires_task", "requires_train_set"),
        man = "mlr3proba::mlr_measures_dens.squared"
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
           }
        ),

        private = list(
        .eps = numeric(0),
        .score = function(prediction, learner, task, train_set, ...) {
         # return NA if learner not compatible
         # change `c("dens.kde")` to list of compatible learners

        train =  task$data(train_set)[[1]]
        bw = unlist(prediction$distr$parameters("bandwidth")[[2]])

        kernel = get(as.character(subset(
                        distr6::listKernels(),
                        ShortName == unlist(prediction$distr$parameters("kernel")[[2]]),
                        ClassName)))$new(bw = bw)

        dat = sapply(train, function(x) (x - train))
        pdf = prediction$pdf

        pdf2norm = sum(kernel$pdfSquared2Norm(x = dat)) / (length(train)^2)

        score = - 2 * mean(pdf) + pdf2norm

          return(score)
          }
        ))
