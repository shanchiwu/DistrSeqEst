progress_controller <- function(type = c("init", "tick", "done"),
                                verbose = 1,
                                total = NULL, d2 = NULL,
                                pb_env = NULL,
                                tokens = list()) {
  type <- match.arg(type)

  if (type == "init") {
    pb_format <- switch(as.character(verbose),
      "0" = NULL,  # silent
      "1" = "(:current/:total) [Elapsed: :elapsedfull] ", # default
      "2" = "(:current/:total) [Elapsed: :elapsedfull Last update: :tps_end] - sample id: :idx - :tps secs/sample ",
      "3" = "(:current/:total) [Elapsed: :elapsedfull Last update: :tps_end] - sample id: :idx - :tps secs/sample - stop rule1: :st1_l < :st1_r ",
      NULL
    )

    if (verbose >= 3 && !is.null(d2)) {
      pb_format <- paste0(pb_format, "- stop rule2: :auc.var < :st2 ")
    }

    if (!is.null(pb_format) && !is.null(total)) {
      pb <- progress_bar$new(format = pb_format, total = total, clear = FALSE, width = 300)
      return(pb)
    } else {
      return(NULL)
    }

  } else if (type == "tick") {
    if (!is.null(pb_env) && verbose >= 1) pb_env$tick(tokens = tokens)

  } else if (type == "done") {
    if (!is.null(pb_env)) {
      pb_env$terminate()
      remove(pb_env)
    }
  }
}
