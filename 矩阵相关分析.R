#导入数据
data1 <- read.csv("E://办公/R语言代码笔记/数据库/运动矩阵.csv",header=T)

#查看缺失值
anyNA(data1)

#载入绘图包

library("corrplot")

#开始绘图
cor1 <- cor(data1,use = "everything", method = c("spearman"))
cor1
corrplot(cor1,method = c("square"))

corrplot.mixed(cor1,lower = "number",upper = "square",lower.col="black",number.cex=0.9,tl.cex=0.9)
