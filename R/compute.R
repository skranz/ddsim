examples.var.par.mat.subst.li = function() {
  par.names = c("G","I")
  var.names = c("Y","C")
  matrix.subst.li(c(var.names, par.names))
}

dd_make_computefun = function(dd) {
  restore.point("dd_create_simfun")
  
  syms = c(dd$par.names, dd$var.names)
  subst.li = matrix.subst.li(syms)

  li = lapply(names(dd$explicit), function(expl) {
    call = substitute(var <- rhs, list(var=as.name(expl),rhs=dd$explicit[[expl]]))
    substitute.call(call,subst.li)
  })
  explicit.assign = as.call(c(list(as.symbol("{")),li))

  fun.body = substitute({
    if (is.null(mat)) {
      mat = make.dd.parvar.mat(dd=dd,T=T)
    }
    
    for (ti in 2:T){
      explicit.assign
    }
    mat
  }, list(explicit.assign = explicit.assign))
  
  fun = function(T,dd=NULL,mat=NULL) {}
  body(fun) <- fun.body

  # compile function with byte code complier
  compfun = compiler::cmpfun(fun)
  dd$compute.fun = compfun

  dd
  
}

make.dd.par.mat = function(dd, T=dd$T) {
  restore.point("make.dd.par.mat")
  # we create T+2 rows
  pars = dd$pars
  if (!is.null(dd$init.pars)) {
    for (i in names(dd$init.pars)) {
      if (is_lazy_value(pars[[i]])) {
        pars[[i]] = dd$init.pars[[i]]
      } else {
        pars[[i]][1] = dd$init.pars[[i]]        
      }
    }
  }

  li = c(list(.TIME.VAR = 1:(T+1)),pars)  
  par.df = do.call(data_frame, li)
  if (!is.null(dd$timevar))
    colnames(par.df)[1] = dd$timevar
  
  if (length(dd$shocks)>0) {
    for (shock in dd$shocks) {
      par.df = compute.shock.on.par.df(par.df = par.df, shock=shock,T=T)
    }
  }
  par.mat = as.matrix(par.df)
  par.mat
}

compute.shock.on.par.df = function(par.df, shock, T=NROW(par.df)-2) {
  restore.point("compute.shock.on.par.df")  
  if (shock$start>T+1) return(par.df)
  shock.t = shock$start:min(shock$end, T+1)
  for (par in shock$pars) {
    val = eval(shock$calls[[par]], par.df[shock.t,])
    par.df[[par]][shock.t] = val
  }
  par.df
}


make.dd.parvar.mat = function(dd, T=dd$T, par.mat=make.dd.par.mat(dd,T)) {
  vars = names(dd$explicit)
  
  # we create T+2 rows
  var.mat = matrix(NA_real_,T+1,length(vars))
  colnames(var.mat) = vars

  mat = cbind(var.mat, par.mat)
  mat[1, names(dd$init.vars)] = dd$init.vars
  mat
}

matrix.subst.li = function(names=NULL, df="mat", ti="ti") {
  restore.point("var.par.mat.subst.li")

  make.subst.li = function(names, df="mat", ti="ti", df.names=names) {
    subst = paste0(df,'[',ti,',"',df.names,'"]')
    li = lapply(subst, parse.as.call)
    names(li) = names
    li
  }
  li1 = make.subst.li(names, df = "mat",ti="ti")
  li2 = make.subst.li(paste0("lag_",names), df = "mat",ti="ti-1", df.names=names)
  li3 = make.subst.li(paste0("lead_",names), df = "mat",ti="ti+1",df.names=names)
  li4 = make.subst.li(paste0("lag_lead_",names), df = "mat",ti="ti",df.names=names)

  c(li1,li2,li3,li4)
}
