examples.ddsim = function() {
  
  dd = ddsim() %>%
    dd_param(G=NA,I=10,C0=2,c=0.8, tau = 0.4, decay=1) %>%
    dd_init_fixed(Y=100) %>%
    dd_init_steady_state(Y,EV) %>%
    dd_explicit(
      EV = decay*(1-lag_tau)*lag_Y + (1-decay)*lag_EV,
      C = C0 + c*EV,
      Y = C + G + I
    ) %>%
    dd_shock(G=G+20, start=3, length=1, name="Staatsausgabenerhöhung.") %>%
    dd_run(T=10)
  sim = dd_data(dd)

  show = c("C","Y","G")
  dd_dyplot(dd,sim,show)
  
  
  dyplot(sim[1:10,],xcol="t",ycol=show) %>%
    dy.annotate.shock()

  d = dd_data(dd, long=TRUE)
  d = filter(d, var %in% show)
  
  library(ggplot2)
  gg = ggplot(d,aes(t, value, color=var)) + geom_line() + theme_bw() + facet_wrap(~var,scales = "free") + xlab("") + ylab("")
  gg
  ggplotly(gg) %>% config(displayModeBar = F)
  
  
  d = filter(d, var=="Y" | var=="C") %>% mutate(time=t, Kennzahl = var)
  library(ggplot2)
  library(plotly)
  gg = ggplot(d,aes(t, value, fill=Kennzahl,frame=t, ids=var)) + geom_point() 
  gg
  ggplotly(gg) %>% config(displayModeBar = F)
  
  data(gapminder, package = "gapminder")
  d = gapminder
gg <- ggplot(d, aes(year, lifeExp)) +
  geom_point(aes(size = pop, frame = year, ids = country))
ggplotly(gg)
}

ddsim = function(timevar="t",verbose=FALSE) {
  dd = nlist(timevar,verbose,shocks=list())
  class(dd) = c("ddsim","list")
  dd
}

dd_param = function(dd,...) {
  dd$pars = list(...)
  dd$par.names = names(dd$pars)
  dd
}

dd_eqs = function(dd,...) {
  dd$eqs = eval(substitute(alist(...)))
  dd
}


dd_explicit = function(dd,...) {
  vars = eval(substitute(alist(...)))
  restore.point("dd_explicit")
  dd$explicit = vars
  dd
}

dd_run = function(dd, T = first.none.null(dd[["T"]],NROW(dd$pars))) {
  restore.point("dd_run")

  dd = dd_compute_initial_values(dd)
  
  
  dd$T = T
  if (is.null(dd$explicit) & is.null(dd$eqs)) {
    par.mat = make.dd.par.mat(dd=dd,T=T)
    dd$data = as_data_frame(dd$par.mat[2:(T+1),])
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
  dd$data = as_data_frame(dat[1:T,])
  dd
}

dd_data = function(dd,long=FALSE) {
  if (!long)
    return(dd$data)
  
  library(tidyr)
  vars = setdiff(colnames(dd$data),dd$timevar)
  dat = gather_(dd$data,key_col = "var",value_col = "value",gather_cols = vars)
  dat
}

dd_shock = function(dd,..., start=1, end=start+length-1, length=1, name=NULL) {
  shock = eval(substitute(alist(...)))
  pars = names(shock)
  calls = shock
  
  shock = list(pars=pars, calls=calls, start=start, end=end, length=length, name=name)
  if (!is.null(name)) {
    dd$shocks[[name]] = shock 
  } else {
    dd$shocks = c(dd$shocks, list(shock))
  }
  dd
}

dd_clear_shocks = function(dd) {
  dd$shocks = list()
  dd
}

get_dd_var.names = function(dd) {
  names(dd$explicit)
}

get_dd_par.names = function(dd) {
  dd$par.names

}