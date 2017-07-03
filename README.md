Simple discrete dynamic simulations in R. Mainly used to illustrate simple dynamic economic models. No fancy stuff like solving DSGE equilibria, though.

An example
```r
library(ddsim)

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

show = c("C","Y","S","S_PLAN","Geplante Sparquote")
dd_dyplot(dd,sim,show)

```