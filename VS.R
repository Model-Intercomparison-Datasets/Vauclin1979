
obs = as.matrix(read.csv('/Users/leleshu/Dropbox/PIHM/Benchmark/Vauclin/Ref/v1.csv'))
xo = data.frame(x=obs[,1]/100, z=2-obs[,2]/100, col=obs[,3])


VS.obs.sim <- function(obs, sim){
  x1=sim[,1]
  y1=sim[, -1]
  uid = sort(unique(obs[,3]))
  nx=length(uid)
  # matplot(x1, y1, type='l')
  yo=xo$z
  ys=NULL
  for(i in 1:nx){
    message(i, '/', nx)
    id=which(xo[,3] %in% uid[i])
    x0=xo[id, 1]
    yout = approx(x = x1, y=y1[, i], xout=x0)
    # points(yout$x, yout$y)
    ys=c(ys, yout$y)
  }
  md=data.frame(obs=yo, sim=ys, group=xo$col)
}
md=VS.obs.sim(obs = xo, 
           sim = cbind(x = seq(0, 3.1, length.out = nrow(rv)),
                       y = rv) )

RSQ(md$obs, md$sim)
RMSE(md$obs, md$sim)

cols=rainbow(8)
ggplot(data=md, aes(x=Vauclin1979, y=Simulation ) )+
  geom_point( aes(color=as.factor(Hours) ) ) +
  geom_abline() + geom_smooth(aesformula=y ~  x, method = glm, se=F) +
  scale_color_manual(values=cols)
# 
