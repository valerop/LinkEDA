library(LinkEDA)

set.seed(1)
d <- data.frame(x = rnorm(100000), y = rnorm(100000))
system.time({
  p <- ls_scatter(d, "x", "y", group = "bench100k")
})
ls_close_all()
