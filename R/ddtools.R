
set_df_num_rows = function(df, num.rows) {
  if (is.null(df)) return(NULL)
  if (NROW(df)==T) return(df)
  if (NROW(df)>T) {
    return(df[1:T,])
  }
  li = lapply(as.data.frame(df), function(col) {
    rep(col, length.out=T)
  })
  do.call(data_frame(li))
}
