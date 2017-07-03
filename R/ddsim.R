examples.ddsim = function() {

  
library(ddsim)
dd = ddsim(verbose=TRUE) %>%
  dd_param(alpha = 0.45,s = 0.2,delta = 0.1,eta = 1, k0=1,y0=1) %>%
  dd_init_fixed(y=~y0) %>%
  dd_explicit(
    k = (1-delta)*lag_k + s*lag_y,
    y = eta*k^alpha
   ) %>%
  dd_expost("Netto Investitionen" = s*y-delta*k,  k_star = (s*eta / delta)^(1/(1-alpha))) %>%
  dd_run_scens(pars=list(y0=c(1,2,3,4),T=50))

sim = dd_data(dd)  
  library(ddsim)
  dd = ddsim(verbose=TRUE) %>%
    dd_param(alpha = 0.7,s = 0.2,delta = 0.1,eta = 1, k0=1,y0=1, k_star = ~(s*eta / delta)^(1/(1-alpha))) %>%
    dd_init_fixed(y=~y0) %>%
    dd_explicit(
      k = (1-delta)*lag_k + s*lag_y,
      y = eta*k^alpha
     ) %>%
    dd_expost("Netto Investitionen" = s*y-delta*k) %>% 
    dd_run_scens(pars=list(y0=c(0.00001,0.1,1,3)),T=50)

  sim = dd_data(dd) %>%
    mutate("BIP_0"=as.factor(y0))
  
  library(ggplot2)
  p = ggplot(data=sim, aes(x=t,y=y,color=BIP_0)) + geom_line() + ylab("BIP") + theme_bw()
  p

  d = sim %>% group_by(BIP_0) %>%
    summarize(Wachstum=100*(y[10]/y[1]-1),y0=y[1])

  p = ggplot(data=d, aes(x=y0,y=Wachstum)) + geom_line() + ylab("Wachstum")  + theme_bw()
  p
  
    
  sim = dd_data(dd) %>%
    mutate("Startkapital"=as.factor(k0))
  
  library(ggplot2)
  p = ggplot(data=sim, aes(x=t,y=y,color=Startkapital, group=k0)) + geom_line() + ylab("BIP") + theme_bw()
  p
  
  d = sim %>% group_by(k0, Startkapital) %>%
    summarize(Wachstum=100*(y[n()]/y[1]-1),y0=y[1])

  p = ggplot(data=d, aes(x=y0,y=Wachstum)) + geom_line() + ylab("Wachstum")  + theme_bw()
  p

  dd = dd %>%
    dd_init_fixed(k=kstar) %>%
    dd_run_scens(par=list(eta=c(0.1,0.5,1,2,3)),T=50)
  
  sim = dd_data(dd) %>%
    mutate("Startkapital"=as.factor(k0))   
  
    
  
  dd = ddsim(verbose=TRUE) %>%
    dd_param(I=10,c0=0,c1=0.9) %>%
    dd_init_fixed(lag_Y=100, EY=100) %>%
    dd_explicit(
      EY = lag_Y,
      C = c0 + c1*EY,
      Y = C + I
     ) %>%
    dd_expost(S = Y-C, S_PLAN=EY-C, "Geplante Sparquote" = (1-c1)*100, "Reale Sparquote" = 100*S / Y) %>%
    dd_shock(c1=0.8, start=3, length=Inf, name="Sparschock") %>%
    dd_run(T=20)
    sim = dd_data(dd)

  
  
    
  dd = ddsim() %>%
    dd_param(G=0,I=10,C0=0,c=0.8, tau = 0, decay=1) %>%
   # dd_init_fixed(Y=100) %>%
    dd_init_steady_state(Y,EV) %>%
    dd_explicit(
      EV = decay*(1-lag_tau)*lag_Y + (1-decay)*lag_EV,
      C = C0 + c*EV,
      Y = C + G + I
     ) %>%
    dd_expost(V = (1-tau)*Y, S = V-C, S_PLAN=EV-C, "Geplante Sparquote" = (1-c)*100, "Reale Sparquote" = 100*S / V) %>%
    #dd_shock(G=G+20, start=3, length=1, name="Staatsausgabenerhöhung.") %>%
    dd_shock(c=0.6, start=3, length=Inf, name="Sparschock") %>%
    dd_run(T=20)
  sim = dd_data(dd)

  show = c("C","Y","S","S_PLAN","Geplante Sparquote")
  dd_dyplot(dd,sim,show)
  
  show = c("Y","Geplante Sparquote", "Reale Sparquote")
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
#  pars = eval(substitute(alist(...)))
#  restore.point("dd_param")
#  values = list()
#  for (par in names(pars)) {
#    values[[par]] = eval(pars[[par]], values)
#  }
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

dd_expost = function(dd,...) {
  dd$expost.vars =  eval(substitute(alist(...)))
  dd
}

dd_compile = function(dd, compile=TRUE, compile.initial=TRUE) {
  if (compile.initial)
    dd = dd_compile_inital_cluster(dd)
  if (!compile)
    return(dd)

  if (!is.null(dd$eqs))
    stop("Equation solving not yet implemented...")
    
  dd = dd_make_computefun(dd)
}

#' Simulate multiple scenarios that can differ
#' by their parameters
dd_run_scens = function(dd,pars=list(),par.df=NULL, T = first.none.null(dd$pars[["T"]],dd[["T"]],NROW(dd$pars)), compile=TRUE, compile.initial=compile) {
  restore.point("dd_run_scens")

  if (is.null(par.df)) {
    par.df = as_data_frame(expand.grid(pars, stringsAsFactors = FALSE))
  }
  
  dd = dd_compile(dd, compile=compile, compile.initial=compile.initial)

  li = lapply(seq_len(NROW(par.df)), function(row) {
    restore.point("sdnsdj")
    par = as.list(par.df[row,])
    dd$init.exo[names(par)] = dd$pars[names(par)] = par
    if (T %in% names(par))
      T = par$T
    dd_run(dd,T=T, compile=FALSE, return.data = TRUE)
  })
  data = bind_rows(li)  
  dd$data = data
  dd
}


dd_run = function(dd, T = first.none.null(dd$pars[["T"]],dd[["T"]],NROW(dd$pars)), compile=TRUE, compile.initial=compile, return.data=FALSE) {
  restore.point("dd_run")

  
  dd = dd_compile(dd, compile=compile, compile.initial=compile.initial)
  
  dd = dd_compute_initial_values(dd,recompile = FALSE)
  
  dd$T = T
  if (is.null(dd$explicit) & is.null(dd$eqs)) {
    par.mat = make.dd.par.mat(dd=dd,T=T)
    dd$data = as_data_frame(dd$par.mat[2:(T+1),])
    return(dd)
  }

  vars = names(dd$explicit)
  dd$var.names = vars
  dat = dd$compute.fun(T=T, dd=dd)
  data = as_data_frame(dat[1:T,])
  for (var in names(dd$expost.vars)) {
    data[[var]] = eval(dd$expost.vars[[var]], data)
  }
  if (return.data) return(data)
  dd$data = data
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