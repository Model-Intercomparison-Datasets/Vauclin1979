n=4
lim =  pi * c(-1, 1)
nlen=100
ff <- function(x, n){
  r <- sin(x * n)
}

fx <- function(x,y, n){
  n = n
  z = ff(x,n) + ff(y, n)
  if( n > 1){
    z= z + fx(x, y, n - 1) / (n - 1)
  }else{
    z = z
  }
  z
}
#
x=seq(lim[1], lim[2], length.out = nlen)
y=seq(lim[1], lim[2], length.out = nlen)
xy=expand.grid(x,y)
z=fx(xy[,1], xy[,2], n)
library(rasterVis)
r=rasterFromXYZ(cbind(xy,z) )
plot(r)
plot3D(r)
