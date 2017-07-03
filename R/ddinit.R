
dd_init_fixed = function(dd,...) {
  dd$init.fixed = c(...)
  dd
}


dd_init_steady_state = function(dd, ..., .vars=NULL) {
  vars = eval(substitute(alist(...)))
  dd$init_ss_vars = unique(c(.vars, sapply(vars, as.character)))
  #restore.point("init.steady.state")
  return(dd)
}

dd_init_eqs = function(dd,...) {
  dd$init_eqs = eval(substitute(alist(...)))
  dd
}

dd_compile_inital_cluster = function(dd,...) {
  restore.point("dd_compile_initial_values")

    
  dd$par.names = get_dd_par.names(dd)
  dd$var.names = get_dd_var.names(dd)

  # Gather all equations to solve for initial values
  
  # Equations for the variables that start as steady state
  # If x starts in a steady state, we need to have
  # lag_x == x
  ss_eqs = lapply(dd$init_ss_vars, function(var) {
    substitute(lag_x == x, list(x = as.name(var), lag_x = as.name(paste0("lag_",var))))
  })

  # Equations from explicit variables
  expl.eqs = lapply(names(dd$explicit), function(var) {
    substitute(x == rhs, list(x=as.name(var), rhs = dd$explicit[[var]]))
  })
  
  # Combine all equations
  dd$all.init.eqs = eqs = c(dd[["init_eqs"]], dd[["eqs"]], ss_eqs, expl.eqs)
  
  # All variables and parameters contained in the equations
  syms = unique(unlist(lapply(eqs, find.variables)))
  
  # Specify exogeneous variables and parameters
   
  exo.vars = dd$init.fixed

  is.exo = unlist(lapply(dd$pars, function(par) {
    if (is_lazy_value(par)) return(TRUE)
    !is.na(par[[1]])
  }))
  exo.pars = lagged.exo.pars = dd$pars[is.exo]
  names(lagged.exo.pars) =  paste0("lag_", names(exo.pars))
  
  exo = c(exo.vars, exo.pars, lagged.exo.pars)
  exo.names = names(exo)
  endo.names = setdiff(syms,exo.names)
  
  if (dd$verbose)
    cat("\nFind initial values...")
  
  # cluster.equations so that we can solve the more easily
  df = suppressWarnings(symbeqs::cluster.equations(eqs, endo=endo.names,verbose = dd$verbose))
  dd$init.equation.clusters = df
  
  dd$init.exo = exo
  dd
}

dd_compute_initial_values = function(dd,recompile=TRUE,...) {
  
  
  # In general we need to solve a system of
  # equations for endogenous parameter and variable
  # values in periods t=0 and t=1
  # 
  # The values in period 0 will get a lag_ prefix
  
  # Endogeneous parameters are specified with NA
  # Variables are by default endogeneous, unless
  # a value is specified in init_fixed
  
  # We can ignore symbols that will not be referenced
  # as lags
  
  restore.point("dd_compute_initial_values")
  
  if (recompile) {
    dd = dd_compile_inital_cluster(dd)
  }
  
  values = first.non.null(dd$pars,list())
  exo = dd$init.exo

  needs.eval = sapply(exo, function(val) {
    is(val, "formula") | is(val,"call") | is(val,"name") | is(val,"expression")
  })
  
  # eval exogenous variables given by a formula (parameter)
  for (var in names(exo)[needs.eval]) {
    exo[[var]] = values[[var]] = compute.value(exo[[var]], exo)
  }
  
  df = dd$init.equation.clusters
  vals = suppressWarnings(symbeqs::eval.cluster.df(df, exo=exo))
  
  init.vals = c(vals, unlist(exo))
  dd$init.vars = init.vals[dd$var.names]
  dd$init.pars = init.vals[dd$par.names]
  dd
}


compute.value = function(val, values, eval.string=TRUE) {
  restore.point("compute.value")
  if (is(val, "formula")) val = val[[2]]
  if (eval.string & is.character(val)) val = parse(text=val)
  if (is(val,"call") | is(val,"name") | is(val,"expression")) {
    val = eval(val, values)
  }
  return(val)
}

is_lazy_value = function(val, eval.string=FALSE) {
  is(val,"formula") | is(val,"call") | is(val,"name") | is(val,"expression") | (eval.string & is(val,"character"))  
}