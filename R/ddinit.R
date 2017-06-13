
dd_init_fixed = function(dd,...) {
  dd$init_fixed = c(...)
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

dd_compute_initial_values = function(dd,...) {
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
  exo.vars = dd$init_fixed
  is.exo = unlist(lapply(dd$param, function(par) !is.na(par[[1]])))
  exo.pars = lagged.exo.pars = dd$param[is.exo]
  names(lagged.exo.pars) =  paste0("lag_", names(exo.pars))
  
  exo = c(exo.vars, exo.pars, lagged.exo.pars)
  exo.names = names(exo)
  endo = setdiff(syms,exo.names)
  
  # cluster.equations so that we can solve the more easily
  df = symbeqs::cluster.equations(eqs, endo=endo)
  
  # compute the values of all endogenous symbols
  vals = symbeqs::eval.cluster.df(df, exo=exo)
}
