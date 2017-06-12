examples.ddsim = function() {
  dd = ddsim() %>%
    dd_param(G=50,I=10,C0=2,c=0.8, tau = 0.4) %>%
    dd_init0(Y=100) %>%
    dd_explicit(
      EV = (1-lag_tau)*lag_Y,
      C = C0 + c*EV,
      Y = C + G + I
    ) %>% 
    dd_run(T=100)
  sim = dd_data(dd)
  
}

ddsim = function(...) {
  dd = nlist(...)
  class(dd) = c("ddsim","list")
  dd
}

dd_param = function(dd,...) {
  li = list(...)
  restore.point("dd_param")
  dd$param = do.call(cbind, li)
  dd$par.names = names(li)
  dd
}

dd_explicit = function(dd,...) {
  vars = eval(substitute(alist(...)))
  restore.point("dd_explicit")
  dd$explicit = vars
  dd
}

dd_init0 = function(dd,...) {
  dd$init0 = c(...)
  dd
}

dd_init1 = function(dd,...) {
  dd$init1 = c(...)
  dd
}


dd_run = function(dd, T = NROW(dd$param)) {
  restore.point("dd_run")
  
  if (is.null(dd$explicit) & is.null(dd$eqs)) {
    rows = (0:(T-1) %% NROW(dd$param))+1
    par.mat = dd$param[rows,]
    dd$data = dd$par.mat
    return(dd)
  }

  if (!is.null(dd$eqs))
    stop("Equation solving not yet implemented.")

  vars = names(dd$explicit)
  dd$var.names = vars

  if (is.null(dd$compute.fun)) {
    dd = dd_make_computefun(dd)
  }
  
  dat = dd$compute.fun(T=T, dd=dd)
  dd$data = dat[2:(T+1),]
  dd
}

dd_data = function(dd,...) {
  if (is.null(dd[["data"]]))
    dd = dd_run(dd,...)
  dd$data
}