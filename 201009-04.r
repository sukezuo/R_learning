# date：2020-10-09
# author：suke
# task：条形图

# 包导入
library(ggplot2)
library(gcookbook)
library(plyr) # 包安装tips 要记得在包名加上双引号

# 双变量柱状图
data=pg_mean
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat="identity")

ggplot(BOD,aes(x=Time,y=demand))+geom_bar(stat="identity")  #连续变量
ggplot(BOD,aes(x=factor(Time),y=demand))+geom_bar(stat="identity") #离散变量

ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat="identity",fill="lightblue",colour = "black")

# 簇状柱状图
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(stat="identity")  # 堆积图条形图
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(position = "dodge",stat="identity")  #簇状条形图
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(position = "dodge",stat="identity")+scale_fill_brewer(palette = "Pastell") # 添加了颜色

ce = cabbage_exp[1:5,]
ggplot(ce,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(position = "dodge",stat="identity")+scale_fill_brewer(palette = "Pastell") # 添加了颜色

# 频率条形图
# diamonds 每一行对应一个钻石的品质
ggplot(diamonds,aes(x=cut))+geom_bar() # 默认进行频数统计
ggplot(diamonds,aes(x=carat))+geom_bar() # 默认进行频数统计 ？如何按照固定间隔进行频数统计？ 
ggplot(diamonds,aes(x=carat))+geom_histogram(binwidth = 0.01) # 等同上句子
ggplot(diamonds,aes(x=carat))+geom_histogram(binwidth = 0.1)  # 调整分桶宽度

# 条形图着色
# subset 函数为过滤函数，选出增长速度大于40的州
upc = subset(uspopchange,rank(Change)>40)
ggplot(upc,aes(x=Abb,y=Change,fill = Region))+geom_bar(stat = "identity")
ggplot(upc,aes(x=Abb,y=Change,fill = Region))+geom_bar(stat = "identity")+scale_fill_manual(values=c("#669933","#FFCC66"))+xlab("State")
ggplot(upc,aes(x=Abb,y=Change,fill = Region))+geom_bar(stat = "identity")+scale_fill_manual(values=c("#FFCC66","#669933"))+xlab("State")
ggplot(upc,aes(x=reorder(Abb,Change),y=Change,fill = Region))+geom_bar(stat = "identity")+scale_fill_manual(values=c("#FFCC66","#669933"))+xlab("State")

# 阈值填充
csub = subset(climate,Source == "Berkeley"&Year>=1900)
csub$pos = csub$Anomaly10y<=0
ggplot(csub,aes(x=Year,y=Anomaly10y,fill = pos))+geom_bar(stat = "identity",position =  "identity")+scale_fill_manual(values=c("#FFCC66","#669933"))
ggplot(csub,aes(x=Year,y=Anomaly10y,fill = pos))+
  geom_bar(stat = "identity",position =  "identity",colour="black",size =0.2)+
  scale_fill_manual(values=c("#CCEEFF","#FFDDDD"),guide = FALSE) # size是表示线的粗细 guide是表示去掉图标

# 间距调整
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat="identity",width =0.5)
ggplot(pg_mean,aes(x=group,y=weight))+geom_bar(stat="identity",width =0.7)

ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(position = "dodge",stat="identity")
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(position = "dodge",stat="identity",width=0.5)
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(position = position_dodge(0.7),stat="identity",width=0.1) # 默认0.9


# 堆积图
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(stat="identity")
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar,order=desc(Cultivar)))+geom_bar(stat="identity")+guides(fill=guide_legend(reverse=TRUE)) # 图例顺序是否要逆序

# 百分比堆积图
ce = ddply(cabbage_exp,"Date",transform,percent_weight =Weight/sum(Weight)*100) # 数据处理 按照date分组处理数据，
ggplot(ce,aes(x=Date,y=percent_weight,fill= Cultivar))+geom_bar(stat="identity")


# y轴调整（最大值/自动调节）
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+geom_bar(stat = "identity")+
  geom_text(aes(label = Weight),vjust = -0.3,colour= "black")+
  ylim(0,max(cabbage_exp$Weight)*1.05) # 调整y轴
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+geom_bar(stat = "identity")+
  geom_text(aes(y=Weight+0.1,label = Weight),vjust = -0.3,colour= "black")


# 添加标签
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+geom_bar(stat = "identity")+geom_text(aes(label = Weight),vjust = 1.5,colour= "white")
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+geom_bar(stat = "identity")+geom_text(aes(label = Weight),vjust = -0.3,colour= "black")

ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+
  geom_bar(stat = "identity",position = "dodge")+
  geom_text(aes(label = Weight),vjust = 1.5,colour= "white",position = position_dodge(1.7),size=3) # size是字体的大小

ce = arrange(cabbage_exp,Date,Cultivar)
ce = ddply(ce,"Date",transform,label_y = cumsum(Weight)-0.5*Weight) # 通过label-y调整数据标记的
ggplot(ce,aes(x=Date,y=Weight,fill=Cultivar))+
  geom_bar(stat = "identity")+
  geom_text(aes(y=label_y,label = paste(format(Weight,nsmall=2),"kg")))


# Cleveland点图
tophit = tophitters2001[1:25,]
ggplot(tophit,aes(x=avg,y=reorder(name,avg)))+# reorder,对name 按照avg排序
  geom_point(size=3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=60,hjust=1),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.x=element_blank()
        )




nameorder = tophit$name[order(tophit$lg,tophit$avg)]
tophit$name = factor(tophit$name,levels = nameorder)
ggplot(tophit,aes(x=avg,y=name))+
  geom_segment(aes(yend=name),xend=0,colour="grey50")+
  geom_point(size=3,aes(colour=lg))+
  scale_colour_brewer(palette = "Set1",limits = c("NL","AL"))+# limits 是为什么
  theme_bw()+
  theme(panel.grid.major.y=element_blank(),
        legend.position = c(1,0.55),
        legend.justification = c(1,0.5))


ggplot(tophit,aes(x=avg,y=name))+
  geom_segment(aes(yend=name),xend=0,colour="grey50")+
  geom_point(size=3,aes(colour=lg))+
  scale_colour_brewer(palette = "Set1",limits = c("NL","AL"),guide=FALSE)+
  theme_bw()+
  theme(panel.grid.major.y=element_blank())+
  facet_grid(lg~.,scales="free_y",space="free_y")











