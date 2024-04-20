.onLoad <- function(libname, pkgname) {
  if (Sys.getenv("COLLECTOR_PATH") != "") {
    dir.create(Sys.getenv("COLLECTOR_PATH"), recursive = TRUE, showWarnings = FALSE)
    collector::set_collector(
      funs = c(
        "add_count.data.frame",
        "anti_join.data.frame",
        "arrange.data.frame",
        "count.data.frame",
        "cross_join.data.frame",
        "distinct.data.frame",
        "full_join.data.frame",
        "inner_join.data.frame",
        "intersect.data.frame",
        "left_join.data.frame",
        "mutate.data.frame",
        "pull.data.frame",
        "relocate.data.frame",
        "rename.data.frame",
        "right_join.data.frame",
        "select.data.frame",
        "semi_join.data.frame",
        "setdiff.data.frame",
        "setequal.data.frame",
        "summarise.data.frame",
        "symdiff.data.frame",
        "transmute.data.frame",
        "union.data.frame",
        "union_all.data.frame"
      ),
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
