library(ggplot2)
raw <- read.csv("canada.csv",header=TRUE)

library(dplyr)
data <- mutate(
 raw,
 ameas.ms2 = 2*x.m/t.s^2,
 ahat = ameas.ms2/9.81,
 mhat = m2.kg/(m1.kg+m2.kg+mc.kg),
 )

apredicted.ms2 <- function(m2,m1,mc){
  9.81*(m2)/(m2+m1+mc)
}

fig <- ggplot(data)+geom_point(aes(x=m1.kg,y=ameas.ms2))+
    ylim(0,0.2)+xlim(0,3)+
    geom_function(fun=apredicted.ms2,args=list(m2=0.020,mc=0.5),color='blue')+
    xlab('$m_1$ (\\unit{\\kilo\\gram})')+
    ylab('$a$ (\\unit{\\meter\\per\\second\\squared})')+
    theme_bw(base_size=8)

library(svglite)
svglite('fig5.svg',width=3,height=2,pointsize=8)
print(fig)
dev.off()

fig2 <- ggplot(data)+geom_point(aes(x=mhat,y=ahat))+
    ylim(0,0.02)+xlim(0,0.02)+
    geom_abline(slope=1.2150,intercept=-0.0015,color='blue')+
    xlab('$\\frac{m_2}{m_1+m_2+m_c}$')+
    ylab('$\\frac{a}{g}$')+
    theme_bw(base_size=8)
svglite('fig6.svg',width=3,height=2,pointsize=8)
print(fig2)
dev.off()


library(xtable)
results <- summarize(
	mean.t = mean(t.s),
	sd.t = sd(t.s),
	mean.a = mean(ameas.ms2),
	sd.a = sd(ameas.ms2),
	group_by(data,m1.kg)
	)
print(xtable(results,digits=4),include.rownames=FALSE,file='table1raw.tex')

fit <- lm(ahat~mhat,data=data)
summary(fit)
