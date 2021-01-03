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


           bw = learner$model$bw
           train =  task$data(train_set)[[1]]
           dat <- sapply(train, function (x, y) ((x - y) / bw), y = train)

           kernel = get(as.character(subset(
                  distr6::listKernels(),
                  ShortName == learner$model$kernel,
                  ClassName)))$new(bw = bw)


          # squarednorm = switch(kernel,
          #                  "Norm" = distr6::NormalKernel$new()$pdfSquared2Norm(x = dat),
          #                  "Epan" = distr6::Epanechnikov$new()$pdfSquared2Norm(x = dat),
          #                  "Unif" = distr6::UniformKernel$new()$pdfSquared2Norm(x = dat),
          #                  "Triw" = distr6::Triweight$new()$pdfSquared2Norm(x = dat),
          #                  "Logis" = distr6::LogisticKernel$new()$pdfSquared2Norm(x = dat),
          #                  "Quart" = distr6::Quartic$new()$pdfSquared2Norm(x = dat),
          #                  "Sigm" = distr6::Sigmoid$new()$pdfSquared2Norm(x = dat),
          #                  "Silv" = distr6::Silverman$new()$pdfSquared2Norm(x = dat),
          #                  "Tric" = distr6::Tricube$new()$pdfSquared2Norm(x = dat),
          #                  "Tri" = distr6::TriangularKernel$new()$pdfSquared2Norm(x = dat),
          #                  "Cos" = distr6::Cosine$new()$pdfSquared2Norm(x = dat)
          # )

          pdf = prediction$pdf

          pdfSquared2norm = sum(kernel$pdfSquared2Norm(x = dat, upper = Inf)) / (length(train)^2)

          score = - 2 * mean(pdf) + pdfSquared2norm

          # if (is.null(bw)) {
          #    score = NA
          # } else {
          #     score = - 2 * mean(pdf) + sum(pdf2norm)
          # }

          return(score)
          }
        ))
