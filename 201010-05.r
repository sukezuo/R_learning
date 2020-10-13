# date：2020-10-10
# author：suke
# task：折线图

# 包导入
library(ggplot2)
library(gcookbook)
library(plyr) 

## 简单的折线图
# 连续变量
ggplot(BOD,aes(x=Time,y=demand))+geom_line()
ggplot(BOD,aes(x=Time,y=demand))+geom_line()+geom_point()

#离散变量
BOD1= BOD
BOD1$Time = factor(BOD1$Time)
ggplot(BOD1,aes(x=Time,y=demand,group =1))+geom_line() #因子型的必须添加group参数

# 对坐标轴取对数
ggplot(worldpop,aes(x=Year,y=Population))+geom_line()+geom_point()+scale_y_log10()

## 多条折线图
# 连续变量
tg =ddply(ToothGrowth,c("supp","dose"),summarise,length=mean(len))
ggplot(tg,aes(x=dose,y=length,colour=supp))+geom_line()
ggplot(tg,aes(x=dose,y=length,linetype=supp))+geom_line()

       
# 离散变量
ggplot(tg,aes(x=factor(dose),y=length,colour=supp,group=supp))+geom_line()
ggplot(tg,aes(x=factor(dose),y=length,linetype=supp,group=supp))+geom_line()
# 如果不加分组的话，会变成折线图


ggplot(tg,aes(x=dose,y=length,shape=supp))+geom_line()+geom_point(size=4)
ggplot(tg,aes(x=dose,y=length,fill=supp))+geom_line()+geom_point(size=4,shape=21)
ggplot(tg,aes(x=dose,y=length,shape=supp))+geom_line(position = position_dodge(0.2))+geom_point(size=4,position = position_dodge(0.2))


## 点线样式
# 线型
ggplot(BOD,aes(x=Time,y=demand))+geom_line(linetype="dashed",size = 1,colour="blue")
# 线色
ggplot(tg,aes(x=dose,y=length,colour=supp))+
  geom_line(size=2)+scale_colour_brewer(palette="Set1")+
  geom_point(size=4,shape=22,colour="red",fill="pink")
# 点色
pd = position_dodge(0.2)
ggplot(tg,aes(x=dose,y=length,fill=supp))+geom_line(position = position_dodge(0.2))+
  geom_point(shape=21,size=2,position = position_dodge(0.2))+
  scale_fill_manual(values = c("black","white"))


## 面积图
sunpotyear = data.frame(
  Year = as.numeric(time(sunspot.year)),
  Sunspots = as.numeric(sunspot.year)
)
ggplot(sunpotyear,aes(x=Year,y=Sunspots))+geom_area(fill = "blue",alpha=0.2)+geom_line()


## 面积堆叠图
ggplot(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup,order=desc(AgeGroup)))+
  geom_area(colour="black",size=0.2,alpha=0.4)+
  scale_fill_brewer(palette="Blues",breaks=rev(levels(uspopage$AgeGroup)))


ggplot(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup,order=desc(AgeGroup)))+
  geom_area(colour=NA,alpha=0.4)+
  scale_fill_brewer(palette="Blues")+
  geom_line(position = "stack",size=0.2)

## 百分比堆叠图
uspopage_prop = ddply(uspopage,"Year",transform,Percent=Thousands/sum(Thousands)*100)
ggplot(uspopage_prop,aes(x=Year,y=Percent,fill=AgeGroup,order=desc(AgeGroup)))+
  geom_area(colour="black",size=0.2,alpha=0.4)+
  scale_fill_brewer(palette="Blues",breaks=rev(levels(uspopage$AgeGroup)))

## 置信区间
clim = subset(climate,Source == "Berkeley",select = c("Year","Anomaly10y","Unc10y"))
ggplot(clim,aes(x=Year,y=Anomaly10y))+
  geom_ribbon(aes(ymin=Anomaly10y-Unc10y,ymax=Anomaly10y+Unc10y),alpha=0.2)+
  geom_line()

ggplot(clim,aes(x=Year,y=Anomaly10y))+
  geom_line(aes(y=Anomaly10y-Unc10y),alpha=0.2,linetype="dotted")+
  geom_line(aes(y=Anomaly10y+Unc10y),alpha=0.2,linetype="dotted")+
  geom_line()











