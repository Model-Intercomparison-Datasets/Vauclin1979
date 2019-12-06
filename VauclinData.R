library(ggplot2)
library(hydroGOF)
fun.interp <- function(xo, xs){
  grp = sort(unique(xo[,3]))
  ng=length(grp)
  ret = cbind(xo[, 1:2], 0)
  for(i in 1:ng){
    id1=which(xo[,3] == grp[i])
    id2=which(xs[,3] == grp[i])
    
    tmp=approx(xs[id2,1], xs[id2,2], xout = xo[id1,1], method = 'linear')
    ret[id1,3] = tmp$y
  }
  colnames(ret) = c('x', 'obs', 'sim')
  return(ret)
}


x.obs=read.table('Ref/v2.csv', header = TRUE, sep=',')
colnames(x.obs)=c('X', 'Y', 'Time')
x.obs$Y= 2- x.obs$Y
saveRDS(x.obs, 'Vauclin1979.RDS')

x.sim=read.table('Ref/Vauclin_SimTime.csv', header = TRUE)
head(x.sim)
x.sim$Y = 2-x.sim$Y
saveRDS(x.sim, 'Vauclin1979_Sim.RDS')


x.obs$Time=as.factor(x.obs$Time)
x.sim$Time=as.factor(x.sim$Time)
ggplot()+geom_line(data=x.sim, aes(x=X, y=Y, color=Time))+
  xlim(c(0,3))+ylim(c(0,2))+
  geom_abline(slope=0, intercept = 0.65, color='blue')+
  geom_point(data=x.obs, aes(x=X, y=Y, shape=Time))

y=fun.interp(xo = x.obs, xs=x.sim)
gof(sim = y$sim, obs=y$obs)
er=sort(abs(y$sim - y$obs))
summary(er)
