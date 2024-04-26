collect_if_duckplyr_differs <- function(call, env, calls, value) {
  withr::local_envvar(DUCKPLYR_OUTPUT_ORDER = TRUE)

  call_name <- as.character(call[[1]])
  if (length(call_name) > 1) {
    return()
  }

  call_name <- paste0(gsub("[.]data[.]frame$", "", call_name), ".duckplyr_df")
  duckplyr_call <- call
  duckplyr_call[[1]] <- call2(":::", sym("duckplyr"), sym(call_name))

  withr::local_envvar(DUCKPLYR_OUTPUT_ORDER = TRUE)
  local_options(duckdb.materialize_message = FALSE)

  duckplyr <- try({ out <- eval(duckplyr_call, env); NROW(out); out })

  if (!identical(duckplyr, value)) {
    collector::default_collector_fun(
      call = call,
      env = env,
      calls = calls,
      value = list(duckplyr_call = duckplyr_call, dplyr = value, duckplyr = duckplyr)
    )
  }
}

.onLoad <- function(libname, pkgname) {
  if (Sys.getenv("COLLECTOR_PATH") != "") {
    dir.create(Sys.getenv("COLLECTOR_PATH"), recursive = TRUE, showWarnings = FALSE)
    collector::set_collector(
      funs = c(
        "add_count",
        "anti_join",
        "arrange",
        "count",
        "cross_join",
        "distinct",
        "filter",
        "full_join",
        "inner_join",
        "intersect.data.frame",
        "left_join",
        "mutate",
        "pull",
        "relocate",
        "rename",
        "right_join",
        "select",
        "semi_join",
        "setdiff.data.frame",
        "setequal.data.frame",
        "summarise",
        "symdiff",
        "transmute",
        "union.data.frame",
        "union_all"
      ),

      # collector_fun = collect_if_duckplyr_differs,
      path = Sys.getenv("COLLECTOR_PATH")
    )
  }

  ns_dplyr <- ns_env(pkgname)

  op <- options()
  op.dplyr <- list(
    dplyr.show_progress = TRUE
  )
  toset <- !(names(op.dplyr) %in% names(op))
  if (any(toset)) options(op.dplyr[toset])

  .Call(dplyr_init_library, ns_dplyr, ns_env("vctrs"), ns_env("rlang"))

  # TODO: For `arrange()`, `group_by()`, `with_order()`, and `nth()` until vctrs
  # changes `vec_order()` to the new ordering algorithm, at which point we
  # should switch from `vec_order_radix()` to `vec_order()` so vctrs can remove
  # it.
  env_bind(
    .env = ns_dplyr,
    vec_order_radix = import_vctrs("vec_order_radix")
  )

  run_on_load()

  invisible()
}

.onAttach <- function(libname, pkgname) {
  setHook(packageEvent("plyr", "attach"), function(...) {
    packageStartupMessage(rule())
    packageStartupMessage(
      "You have loaded plyr after dplyr - this is likely ",
      "to cause problems.\nIf you need functions from both plyr and dplyr, ",
      "please load plyr first, then dplyr:\nlibrary(plyr); library(dplyr)"
    )
    packageStartupMessage(rule())
  })
}

.onDetach <- function(libpath) {
  setHook(packageEvent("plyr", "attach"), NULL, "replace")
}

import_vctrs <- function(name, optional = FALSE) {
  import_from(name, "vctrs", optional = optional)
}
import_from <- function(name, package, optional = FALSE) {
  ns <- getNamespace(package)

  if (!exists(name, mode = "function", envir = ns, inherits = FALSE)) {
    if (optional) {
      return(NULL)
    }
    abort(sprintf("No such '%s' function: `%s()`.", package, name))
  }

  get(name, mode = "function", envir = ns, inherits = FALSE)
}
