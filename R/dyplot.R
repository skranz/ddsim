
examples.dyplot = function() {
  T = 10
  df = data.frame(t=1:T, y=runif(T),z=runif(T))
  df$y[4:10] = NA
  df
  dyplot(df)
}

dyplot = function (data, xcol = colnames(data)[1], ycol = setdiff(colnames(data), xcol), interval = NULL,...) 
{
    restore.point("dyplot")
    dat = data[,c(xcol,ycol)]
    dy = dygraph(dat,...)
    dy
 }

dd_dyplot = function(dd, dat=dd_data(dd), cols=dd$var.names, main="",xlab=dd$time.var,ylab="", shocks=dd$shocks, show.shocks = TRUE, rows=1:NROW(dat),xlim=c(1,NROW(dat)),...) {
  restore.point("dd_dyplot")
  #dat$t = t.to.date(dat$t)
  p = dyplot(dat[rows,], xcol="t",ycol=cols,
      xlab=xlab,ylab=ylab,main=main) %>% 
    dyOptions(stepPlot = FALSE,strokeWidth = 2) %>%
    #dyRangeSelector() %>%
    dyAxis("x", drawGrid = TRUE)  
  if (show.shocks) {
    p = dy_annotate_shocks(p, shocks, rows=rows)
  }
  if (!is.null(xlim)) {
    p$x$attrs$dateWindow=xlim
  }
  p
}
dy_annotate_shocks = function(dygraph, shocks=dd$shocks,dd=NULL, T = first.non.null(dd$T,NROW(dygraph$x$data[[1]])), rows = 1:T) {
  restore.point("dy_annotate_shocks")
  shock = shocks[[1]]
  for (shock in shocks) {
    if (shock$start > max(rows)) next
    #start = t.to.date(shock$start)
    name = shock$name
    
    dygraph = dygraph %>% 
      dyEvent(x = shock$start, label=name, labelLoc = "bottom")
    
    if (shock$length>1) {
      #end = t.to.date(shock$end)
      dygraph = dygraph%>% dyShading(from = shock$start, to = min(shock$end,T))
    }
      
  }
  dygraph
} 

t.to.date = function(t) {
  as.Date(paste0(t+2000,"-01-01"))
}
