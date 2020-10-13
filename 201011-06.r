# date：2020-10-11
# author：suke
# task：散点图

# 包导入
library(ggplot2)
library(gcookbook)
library(plyr) 
library(hexbin)
library(MASS)
## 基本散点图
ggplot(heightweight,aes(x=ageYear,y=heightIn,colour=sex))+geom_point(shape=20,size = 4)

ggplot(heightweight,aes(x=ageYear,y=heightIn,colour=sex,shape=sex))+
  geom_point(size = 4)+
  scale_shape_manual(values = c(1,3))+
  scale_colour_brewer(palette="Set1")

hw=heightweight
hw$weightGroup=cut(hw$weightLb,breaks = c(-Inf,100,Inf),labels=c("<100",">=100"))
ggplot(hw,aes(x=ageYear,y=heightIn,fill=weightGroup,shape=sex))+
  geom_point(size = 2)+
  scale_shape_manual(values = c(21,24))+
  scale_fill_manual(values = c(NA,"black"),guide=guide_legend(override.aes = list(shape=21)))

## 连续变量渐变
# 随形状变化
ggplot(heightweight,aes(x=ageYear,y=heightIn,size=weightLb))+geom_point() 

ggplot(heightweight,aes(x=ageYear,y=heightIn,size=weightLb,colour=sex))+
  geom_point(alpha=0.5)+
  scale_size_area()+
  scale_color_brewer(palette="Set1")

#随颜色变化，连续变化
ggplot(heightweight,aes(x=ageYear,y=heightIn,fill=weightLb))+ 
  geom_point(size = 2,shape=21)+
  scale_fill_gradient(low="black",high="red",breaks=seq(170,70,by=-20),guide=guide_legend())

#随颜色变化，阶梯变化
ggplot(heightweight,aes(x=ageYear,y=heightIn,fill=weightLb))+ 
  geom_point(size = 2,shape=21)+
  scale_fill_gradient(low="black",high="red",breaks=seq(170,70,by=-20),guide=guide_legend())

## 重叠处理
# 透明处理法
sp = ggplot(diamonds,aes(x=carat,y=price))
sp+geom_point(alpha=0.01)

# 分箱法
sp+stat_bin2d()

sp+stat_bin2d(bins=50)+ #分箱间距是50
  scale_fill_gradient(low="lightblue",high="green",limit=c(0,6000))


sp+stat_binhex(bins=50)+ 
  scale_fill_gradient(low="lightblue",high="green",breaks=c(0,250,500,1000,2000,4000,6000),limit=c(0,6000))
# 定义了间距尺度，到那时guide展示还有一些问题

sp1=ggplot(ChickWeight,aes(x=Time,y=weight))
sp1+geom_point()
sp1+geom_point(position="jitter") # 增加了一些扰动，让数据可以横向展开一点
sp1+geom_point(position=position_jitter(width=0.5,height=0)) 

sp1+geom_boxplot(aes(group=Time)) # 连续性变量要设定分组~~


## 增加回归模型
sp = ggplot(heightweight,aes(x=ageYear,y=heightIn))
sp+geom_point()+stat_smooth(method=lm,level=0.9)
sp+geom_point()+stat_smooth(method=lm,se=NA)
sp+geom_point()+stat_smooth(method=loess)

# 乳腺癌活体检测相关指标
b=biopsy
b$classn[b$class=="benign"]=0
b$classn[b$class=="malignant"]=1
ggplot(b,aes(x=V1,y=classn))+
  geom_point(position = position_jitter(width=0.3,height=0.06),alpha=0.4,shape=21,size=1.5)+
  stat_smooth(method=glm,family=binomial)

sps = ggplot(heightweight,aes(x=ageYear,y=heightIn,colour=sex))+
  geom_point()+scale_colour_brewer(palette = "Set1")
sps+geom_smooth(method = lm,se=FALSE,fullrange=TRUE)

# 已有模型的散点图添加拟合线
model = lm(heightIn ~ ageYear +I(ageYear^2),heightweight)
xmin=min(heightweight$ageYear)
xmax=max(heightweight$ageYear)
predicted = data.frame(ageYear=seq(xmin,xmax,length.out = 100))

predicted$heightIn = predict(model,predicted)
sp = ggplot(heightweight,aes(x=ageYear,y=heightIn))+geom_point(colour="grey40")
sp+geom_line(data=predicted,size=1)


























