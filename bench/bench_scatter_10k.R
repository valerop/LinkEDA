library(LinkEDA)

set.seed(1)
d <- data.frame(x = rnorm(10000), y = rnorm(10000))
system.time({
  p <- ls_scatter(d, "x", "y", group = "bench10k")
})
ls_close_all()
