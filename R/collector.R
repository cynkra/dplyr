# initiate global tibble to store the results
globals <- new.env()
globals$archive <- tibble(
  call = list(),
  env = list()
)

# note: env_clone doesn't do deep copy, i.e we're not cloning environments that
# are bound in `e`, or nested in lists, or found in attributes, or as function enclosures
env_clone_lazy <- function(env) {
  # we stop the recursion when we find a special env, defined by having a name
  if (!is.null(attr(env, "name")) || identical(env, emptyenv())) return(env)
  parent_clone <- env_clone_lazy(env_parent(env))
  clone <- rlang::env_clone(env, parent = parent_clone)
  for (nm in names(clone)) {
    #FIXME: assumes env contains no active or lazy bindings, this could be fixed
    env_bind_lazy(clone, !!nm := !!env[[nm]])
  }
  clone
}

# drop lazy bindings, since these were not used
env_cleanup <- function(env) {
  if (!is.null(attr(env, "name")) || identical(env, emptyenv())) return(env)
  env_cleanup(env_parent(env))
  lazy_lgl <- env_binding_are_lazy(env)
  rm(list = names(lazy_lgl)[lazy_lgl], envir = env)
}

archive <- function(
    call,
    env
) {
  globals$archive  <- rbind(
    globals$archive,
    tibble(
      call = list(call),
      env = list(env)
    )
  )
}
