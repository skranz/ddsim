
ddbb.examples = function() {
library(ddsim)
dd = ddsim() %>%
  dd_param(I=10,c0=0,c1=0.9) %>%
  dd_init_steady_state(Y) %>%
  dd_explicit(
    EY = lag_Y,
    C = c0 + c1*EY,
    Y = C + I
   ) %>%
  dd_expost(S = Y-C, S_PLAN=EY-C, "Geplante Sparquote" = (1-c1)*100, "Reale Sparquote" = 100*S / Y) %>%
  dd_shock(c1=0.8, start=3, length=Inf, name="Sparschock") %>%
  dd_run(T=20)
  sim = dd_data(dd)
select(sim[1:5,],"t","Y","C","I")
show = c("Y","C","I")
bb = dd_bbplot(dd,sim,show, rows=1:5,xlim=c(1,7))
view.bb(bb)
}


dd_bbplot = function(dd, dat=dd_data(dd), cols=dd$var.names, main="",xlab=dd$time.var,ylab="", shocks=dd$shocks, show.shocks = TRUE, rows=1:NROW(dat),xlim=c(1,NROW(dat)),colors=ddcolors(),...) {
  restore.point("dd_bbplot")
  #dat$t = t.to.date(dat$t)
  library(bbsvg)
  bb = bb_pane(data=dat,xrange=xlim,show.ticks = TRUE)

  for (i in seq_along(cols)) {
    col = cols[i]
    bb = bb_series(bb,xvar = "t",yvar=col, color=colors[i])
  }
  if (show.shocks) {
    #bb = dd_bb_annotate_shocks(bb, shocks, rows=rows, dd=dd, T=max(xlim))
  }
  bb
}
dd_bb_annotate_shocks = function(bb, shocks=dd[["shocks"]],dd=NULL, T = first.non.null(dd$T,NROW(bb$data[[1]])), rows = 1:T) {
  restore.point("dy_annotate_shocks")
  #shock = shocks[[1]]
  for (shock in shocks) {
    if (shock$start > max(rows)) next
    #start = t.to.date(shock$start)
    name = shock$name
    
    bb = bb %>%
      bb_period(bb, from=shock$start,to=min(shock$end,T),label = name)
  }
  bb
} 

ddcolors = function(n) {
  c(blue1="#0C5BB0FF",red="#EE0011FF",green="#A1C720FF",
    blue2="#149BEDFF",   turquoise ="#16A08CFF",
    pink="#EC579AFF", orange="#FA6B09FF", yellow="#FEC10BFF", green2="#15983DFF", brown= "#9A703EFF")    
}