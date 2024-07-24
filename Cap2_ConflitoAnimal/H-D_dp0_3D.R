library(plotly)
seq <- seq(1, 10, by = .1)
w_0 <- c()
v <- c()
d <- c()
x <- c()
y <- c()
d <- c()

for (i in seq) {
  w_0<-i
  for (f in seq) {
    v<-f
    d <- c((v/(v+2*w_0)) + 1, d)
    x <- c(w_0, x)
    y <- c(f, y)
  }
}

w_0 <- x
v <- y

fig <- plotly::plot_ly(x = ~w_0, y = ~v, z = ~d, 
                       type = 'mesh3d',
                       xlab = "w0",
                       ylab = "v")
print(fig)

