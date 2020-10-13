# date：2020-10-08
# author：suke
# task：内置数据查看，数据存储和加载

# 查看R语言所有的内置数据
data()

# 查看内置数据Orange
Orange

# 定义数据变量
count = Orange$Tree
age = Orange$age
circumference = Orange$circumference

# 存储数据
save(count,age,circumference,file ="mydata.rda")

# 移除当前工作空间的数据
rm(age,circumference,count)

# 加载已经存储的数据
load(file="mydata.rda")
