library("paradox")
library("mlr3tuning")

logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")

logger_mlr3 = lgr::get_logger("mlr3")
logger_mlr3$set_threshold("warn")

learner = lrn("classif.rpart")
learner$parallel_predict = TRUE
tune_ps = ParamSet$new(list(
  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
  ParamInt$new("minsplit", lower = 1, upper = 10)
))
terminator = trm("evals", n_evals = 2)
tuner = tnr("random_search")
