# date：2020-10-09
# author：suke
# task：ggplot基础学习,图形基础

#####
## 基础入门

# ggplot的包安装
install.packages(c("ggplot2","gcookbook"))
install.packages("xlsx")
install.packages("gdata") # xls文件需要的包
install.packages("foreign") # spss数据导入需要的包


# 包导入
library(ggplot2)
library(gcookbook)
library(xslx)
library(foreign)
ls("package:foreign") # 查看包内所有函数，foreign 包含了read。spss/octave/systat/xport/dta

# 数据加载
data = read.csv("datafile.csv",header = FALSE,seq ="\t",stringsAsFactors = FALSE)
data = read.xlsx("datafile.xlsx",sheetIndex = 2/sheetName = "Revenues")
data = read.xls("datafile.xls",sheet = 2)

?read.csv() # 查看函数帮助

# 重命名数据列名
name(data) = c("C1","C2","C3")


#####
## 散点图 
# 绘图plot(x,y),都是先放自变量，后放应变量
#以下四种方法都是散点图的绘制方法，第一种是原生的，后三种是ggplot包的绘图方式
plot(mtcars$wt,mtcars$mpg) # 原生画图
qplot(mtcars$wt,mtcars$mpg) # ggplot画图 
qplot(wt,mpg,data=mtcars) 
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point() 

## 折线图绘制
# way1原生绘图
plot(pressure$temperature,pressure$pressure,type = "l")
points(pressure$temperature,pressure$pressure)
lines(pressure$temperature,pressure$pressure/2,col = "red")
points(pressure$temperature,pressure$pressure/2,col = "red")
# 用points/lines画图，会把图画在原始图像上方，而不会重新打开新的图，但是只能画在plot上，无法画在ggplot的图上。

# way2 ggplot
qplot(pressure$temperature,pressure$pressure , geom = "line")
qplot(temperature,pressure,data = pressure , geom = "line") # 等价上句
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line() # 等价上句

qplot(pressure$temperature,pressure$pressure , geom = c("line","point"))
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()


## 条形图
# way1 原生
# 第一个变量用量设定柱状图高度，第二个用来定义名称
barplot(BOD$demand,names.arg = BOD$Time)
table(mtcars$cyl) # 统计频数
barplot(table(mtcars$cyl))

# way2 ggplot
qplot(BOD$Time,BOD$demand,geom = "bar",stat = "identity")


ggplot(BOD,aes(x=Time,y=demand))+geom_bar(stat = "identity") # 连续型变量
ggplot(BOD,aes(x=factor(Time),y=demand))+geom_bar(stat = "identity") #离散型变量

qplot(mtcars$cyl) # 自动转化为频数统计
qplot(factor(mtcars$cyl))

## 直方图
# way1 原生
hist(mtcars$mpg)

# way2 ggplot
qplot(mpg,data=mtcars,binwidth=5) # binwidth间隔为5
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 5)  # 等价上句

## 箱线图
# way1 原生
plot(ToothGrowth$supp,ToothGrowth$len) # x为因子变量，y为数值变量时，自动画箱线图
boxplot(len~supp,data = ToothGrowth) # 等价上句
boxplot(len~supp+dose,data = ToothGrowth)



# way2 ggplot
qplot(ToothGrowth$supp,ToothGrowth$len,geom = "boxplot")
qplot(supp,len,data = ToothGrowth,geom = "boxplot")
ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()

qplot(interaction(ToothGrowth$supp,ToothGrowth$dose),ToothGrowth$len,geom = "boxplot")
qplot(interaction(supp,dose),len,data = ToothGrowth,geom = "boxplot")
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+geom_boxplot()


##曲线图
# way1 原生
curve(x^3-5*x,from=-4,to=4)

myfun=function(xvar){ 1/(1+exp(-xvar+10))}
curve(myfun(x),from=0,to=20)
curve(1-myfun(x),add=TRUE,col = "red")

# way2 ggplot
ggplot(data.frame(x=c(0,20)),aes(x=x))+stat_function(fun=myfun,geom="line")






















