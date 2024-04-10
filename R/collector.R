# initiate global tibble to store the results
globals <- new.env()
globals$archive <- tibble(
  call = list(),
  env = list()
)

# Add collect_and_rethrow() before the original body
# and set the original function as the "unmodified" attribute
set_collector <- function(fun, force = FALSE) {
  new_fun <- fun
  body(new_fun) <- call(
    "{",
    quote(collect_and_rethrow()),
    body(fun)
  )
  # note: trace() calls it "original" so we picked a new name
  attr(new_fun, "unmodified") <- fun
  new_fun
}

# * copy caller env and all the chain up to a special env, but every binding is lazy
# * store the call and env in the global tibble
# * on.exit, go through the envs and remove every binding that is still lazy, it's
#   not been used, by reference it will update the global tibble too
# * use the original function to eval the call, and do it in the new env, return
#   this value from the original function call
collect_and_rethrow <- function() {
  new_caller_env <- env_clone_lazy(parent.frame(2))
  call = sys.call(-1)
  archive(
    call = call,
    env = new_caller_env
  )
  rlang::eval_bare(bquote(on.exit(env_cleanup(.(new_caller_env)))), parent.frame())

  call[[1]] <-  attr(sys.function(-1), "unmodified")
  rlang::return_from(parent.frame(), eval(call, new_caller_env))
}



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
