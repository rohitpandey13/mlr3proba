#' @template dens_measure
#' @templateVar title Squared loss
#' @templateVar inherit [MeasureDens]
#' @templateVar fullname MeasureDensSquared
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
          }),

          private = list(
           .eps = numeric(0),
           .score = function(prediction, learner, task, train_set, test_set, ...) {
          # return NA if learner not compatible
          # change `c("dens.kde")` to list of compatible learners

          train =  task$data(train_set)[[1]]
          x =  task$data()[[1]][! task$data()[[1]] %in% train]
          bw = learner$train(task)$model$bw

          kernel = get(as.character(subset(
                  distr6::listKernels(),
                  ShortName == learner$train(task)$model$kernel,
                  ClassName)))$new(bw = bw)

          if (learner$train(task)$model$kernel == "Sigm") {

           score = NA
           } else if (learner$train(task)$model$kernel == "Norm") {

           test_mat = lapply(x, function (x) x - train)
           cdfInt = mapply(function(x) kernel$cdfIntegral(upper = x), test_mat)
           ccdfInt = mapply(function(x) kernel$ccdfIntegral(lower = x), test_mat)
           energy = sum(kernel$energyBrier(train) / length(train)^2)
           score = mean(colSums(ccdfInt)/length(train) +
                   colSums(cdfInt)/length(train) -
                   1/2 * energy)

           } else {

           x_mat = lapply(x, function (x) matrix(x - train, nrow = length(train),
                                 ncol = length(train), byrow = T))
           train_mat = sapply(train, function(x) x - train)
           cdf2norm = mapply(function(x) kernel$cdfSquared2Norm(upper = x, x = train_mat),
                           x_mat)
           ccdf2norm = mapply(function(x) kernel$cdfSquared2Norm(upper = - x, x = - train_mat),
                              x_mat)
           score = mean(colSums(cdf2norm)  / (length(train)^2) + colSums(ccdf2norm) / (length(train)^2))

           }
          return(score)

            }
            ))
