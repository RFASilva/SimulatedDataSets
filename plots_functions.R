

plot3d_time <- function(xyt) {

  data_to_plot <- xyt;

  nbcol = 100
  color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
  zcol  = cut(data_to_plot[,3], nbcol)

  plot3d(data_to_plot[,1], data_to_plot[,2], data_to_plot[,3], xlab = "longitude", ylab= "latitude", zlab ="time",  col =color[zcol])

}