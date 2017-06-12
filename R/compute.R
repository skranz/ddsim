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
      mat = make.dd.parvar.mat(dd=dd,T=T, par.mat=par.mat)
    }
    
    for (ti in 2:(T+1)){
      explicit.assign
    }
    mat
  }, list(explicit.assign = explicit.assign))
  
  fun = function(T,dd=NULL,mat=NULL, par.mat=NULL) {}
  body(fun) <- fun.body

  # compile function with byte code complier
  compfun = compiler::cmpfun(fun)
  dd$compute.fun = compfun

  dd
  
}

make.dd.parvar.mat = function(dd, T, par.mat=NULL) {
  # we create T+2 rows
  
  if (is.null(par.mat)) {
    rows = c(1, (0:T %% NROW(dd$param))+1)
    par.mat = dd$param[rows,]
  }
  vars = names(dd$explicit)
  
  var.mat = matrix(NA_real_,T+2,length(vars))
  colnames(var.mat) = vars

  mat = cbind(par.mat, var.mat)
  
  # set initial values
  if (!is.null(dd$init0)) {
    init0 = unlist(dd$init0)
    mat[1,names(init0)] = init0
  }
  if (!is.null(dd$init1)) {
    init1 = unlist(dd$init1)
    mat[2,names(init1)] = init1
  }
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
