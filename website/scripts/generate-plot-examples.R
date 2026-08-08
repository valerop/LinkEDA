out <- file.path("website", "assets", "plot-examples")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

ink <- "#263746"
muted <- "#72808a"
grid <- "#dde4e8"
blue <- "#008ec4"
blue_pale <- "#dcecf4"
orange <- "#e87500"
paper <- "#fbfcfd"

open_example <- function(name, width = 1200, height = 760) {
  png(file.path(out, paste0(name, ".png")), width = width, height = height,
      res = 144, bg = paper)
  par(family = "sans", fg = ink, col.axis = ink, col.lab = ink,
      mar = c(4.4, 4.5, 3.4, 1.4), las = 1)
}

grid_y <- function() abline(h = axTicks(2), col = grid, lwd = 1)

# Histogram
open_example("histogram")
h <- hist(mtcars$mpg, breaks = 8, plot = FALSE)
plot(h, freq = FALSE, col = blue_pale, border = ink, lwd = 1.2,
     main = "Distribution of fuel economy", xlab = "mpg", ylab = "Density")
lines(density(mtcars$mpg), col = blue, lwd = 3)
rug(mtcars$mpg, col = muted, lwd = 1.4)
dev.off()

# Bar chart
open_example("bar-chart")
counts <- table(factor(mtcars$cyl, levels = c(4, 6, 8)))
bp <- barplot(counts, col = blue_pale, border = ink, lwd = 1.2,
              main = "Cars by cylinder count", xlab = "cyl", ylab = "Number of cars")
text(bp, counts, labels = counts, pos = 3, col = ink)
dev.off()

# Time series
open_example("time-series")
months <- sort(unique(airquality$Month))
cols <- c("#0072B2", "#009E73", "#D55E00", "#7B61A8", "#5D6D7E")
plot(range(airquality$Day), range(airquality$Temp, na.rm = TRUE), type = "n",
     main = "Daily temperature by month", xlab = "Day", ylab = "Temperature (°F)")
grid_y()
for (i in seq_along(months)) {
  d <- airquality[airquality$Month == months[i], ]
  lines(d$Day, d$Temp, col = cols[i], lwd = 2.2)
  points(d$Day, d$Temp, col = cols[i], pch = 16, cex = .65)
}
legend("topleft", legend = month.abb[months], col = cols, lty = 1, pch = 16,
       lwd = 2, bty = "n", ncol = 3, cex = .82)
dev.off()

# Scatterplot matrix: names only on the diagonal.
open_example("scatterplot-matrix", 1100, 900)
vars <- mtcars[c("mpg", "wt", "hp", "qsec")]
pairs(vars, labels = names(vars), pch = 16, cex = .62, col = "#4c555a",
      gap = .35, main = "Scatterplot matrix")
dev.off()

# Parallel coordinates, using standardized variables.
open_example("parallel-coordinates")
pv <- c("mpg", "disp", "hp", "drat", "wt", "qsec")
z <- scale(mtcars[pv])
matplot(seq_along(pv), t(z), type = "l", lty = 1, col = "#b8c2c8",
        lwd = 1.1, xaxt = "n", xlab = "", ylab = "Standardized value",
        main = "Parallel coordinates")
axis(1, at = seq_along(pv), labels = pv)
abline(h = 0, col = grid, lwd = 1)
selected <- which(mtcars$mpg >= quantile(mtcars$mpg, .85))
for (i in selected) lines(seq_along(pv), z[i, ], col = orange, lwd = 2.3)
dev.off()

# Trellis scatterplot.
open_example("trellis-scatterplot", 1350, 620)
par(mfrow = c(1, 3), mar = c(4.2, 4, 3.2, 1.1), oma = c(0, 0, 2.3, 0))
for (g in c(4, 6, 8)) {
  d <- mtcars[mtcars$cyl == g, ]
  plot(d$wt, d$mpg, xlim = range(mtcars$wt), ylim = range(mtcars$mpg),
       pch = 16, col = "#4c555a", xlab = "wt", ylab = "mpg", main = paste("cyl =", g))
  grid_y()
  points(d$wt, d$mpg, pch = 16, col = "#4c555a")
  abline(lm(mpg ~ wt, data = d), col = blue, lwd = 2.5)
}
mtext("Fuel economy and weight, conditioned by cylinders", outer = TRUE,
      side = 3, line = .5, cex = 1.25, font = 2)
dev.off()

# Trellis time series.
open_example("trellis-time-series", 1350, 900)
par(mfrow = c(2, 3), mar = c(3.8, 3.8, 3, 1), oma = c(0, 0, 2.1, 0))
for (m in months) {
  d <- airquality[airquality$Month == m, ]
  plot(d$Day, d$Temp, type = "o", pch = 16, cex = .55, lwd = 2,
       col = blue, xlim = range(airquality$Day), ylim = range(airquality$Temp),
       xlab = "Day", ylab = "Temp", main = month.abb[m])
  grid_y(); lines(d$Day, d$Temp, col = blue, lwd = 2)
}
plot.new()
mtext("Daily temperature, conditioned by month", outer = TRUE,
      side = 3, line = .35, cex = 1.25, font = 2)
dev.off()

# Trellis boxplots.
open_example("trellis-boxplot", 1200, 650)
par(mfrow = c(1, 2), mar = c(4.2, 4, 3.1, 1), oma = c(0, 0, 2.1, 0))
for (a in c(0, 1)) {
  d <- mtcars[mtcars$am == a, ]
  boxplot(mpg ~ factor(cyl), data = d, col = blue_pale, border = ink,
          xlab = "cyl", ylab = "mpg", main = if (a == 0) "Automatic" else "Manual")
  stripchart(mpg ~ factor(cyl), data = d, vertical = TRUE, method = "jitter",
             pch = 16, col = "#4c555a", add = TRUE)
}
mtext("Fuel economy by cylinders, conditioned by transmission", outer = TRUE,
      side = 3, line = .35, cex = 1.2, font = 2)
dev.off()

# Trellis bar charts.
open_example("trellis-bar-chart", 1200, 650)
par(mfrow = c(1, 2), mar = c(4.2, 4, 3.1, 1), oma = c(0, 0, 2.1, 0))
for (a in c(0, 1)) {
  d <- mtcars[mtcars$am == a, ]
  tab <- table(factor(d$cyl, levels = c(4, 6, 8)))
  barplot(tab, col = blue_pale, border = ink, ylim = c(0, max(table(mtcars$cyl))),
          xlab = "cyl", ylab = "Number of cars",
          main = if (a == 0) "Automatic" else "Manual")
}
mtext("Cylinder counts, conditioned by transmission", outer = TRUE,
      side = 3, line = .35, cex = 1.2, font = 2)
dev.off()

# Trellis histograms.
open_example("trellis-histogram", 1350, 620)
par(mfrow = c(1, 3), mar = c(4.2, 4, 3.1, 1), oma = c(0, 0, 2.1, 0))
breaks <- seq(10, 35, by = 5)
for (g in c(4, 6, 8)) {
  hist(mtcars$mpg[mtcars$cyl == g], breaks = breaks, col = blue_pale,
       border = ink, xlim = range(breaks), xlab = "mpg", ylab = "Frequency",
       main = paste("cyl =", g))
}
mtext("Fuel-economy distributions, conditioned by cylinders", outer = TRUE,
      side = 3, line = .35, cex = 1.2, font = 2)
dev.off()
