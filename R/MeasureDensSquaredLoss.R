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
           # bw = learner$train(task)$model$bw
           # dat <- sapply(train, function (x, y) (x - y), y = train)

           # kernel = get(as.character(subset(
           #        distr6::listKernels(),
           #        ShortName == strprint(prediction$distr),
           #        ClassName)))$new(bw = bw)
# #
#         kernel = learner$train(task)$model$kernel
#           squarednorm = switch(kernel,
#                            "Norm" = distr6::NormalKernel$new(bw = bw)$pdfSquared2Norm(x = dat),
#                            "Epan" = distr6::Epanechnikov$new(bw = bw)$pdfSquared2Norm(x = dat),
#                            "Unif" = distr6::UniformKernel$new(bw = bw)$pdfSquared2Norm(x = dat),
#                            "Triw" = distr6::Triweight$new(bw = bw)$pdfSquared2Norm(x = dat),
#                            "Logis" = distr6::LogisticKernel$new(bw = bw)$pdfSquared2Norm(x = dat),
#                            "Quart" = distr6::Quartic$new(bw = bw)$pdfSquared2Norm(x = dat),
#                            "Sigm" = distr6::Sigmoid$new(bw = bw)$pdfSquared2Norm(x = dat),
#                            "Silv" = distr6::Silverman$new(bw = bw)$pdfSquared2Norm(x = dat),
#                            "Tric" = distr6::Tricube$new(bw = bw)$pdfSquared2Norm(x = dat),
#                            "Tri" = distr6::TriangularKernel$new(bw = bw)$pdfSquared2Norm(x = dat),
#                            "Cos" = distr6::Cosine$new(bw = bw)$pdfSquared2Norm(x = dat)
#           )

          pdf = prediction$pdf

          pdf2norm = learner$train(task, train_set)$model$pdf2norm

          # pdfSquared2norm = sum(kernel$pdfSquared2Norm(x = dat)) / (length(train)^2)

          score = - 2 * mean(pdf) + pdf2norm

          # if (is.null(bw)) {
          #    score = NA
          # } else {
          #     score = - 2 * mean(pdf) + sum(pdf2norm)
          # }

          return(score)
          }
        ))
