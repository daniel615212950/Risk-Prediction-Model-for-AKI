# 导入数据
ptbdata1 <- read.csv("E://办公/R语言代码笔记/数据库/PTBlogistics.csv")
ptbdata1

# 加载包
library("rms")

# 设定nomogram的参数
dd <- datadist(ptbdata1)
options(datadist='dd')

# logistic回归
f1 <- lrm(Group~UM+URBC+CYSC+ALB+BMI+eGFR+CA.125, data = ptbdata1,x=T,y=T)
f1

# nomogram回归制图
# 方法1

nom1 <- nomogram(f1,fun= function(x)1/(1+exp(-x)),abbrev=FALSE, minlength=2, maxscale=100, nint=10,fun.at=c(0.001,0.01,0.1,0.5,0.9,0.999),funlabel="Risk")

nom3 <- nomogram(f1,fun=plogis,abbrev=FALSE, minlength=1, maxscale=100, nint=10,fun.at=c(0.001,0.01,0.15,0.5,0.9,0.999),lp=F,funlabel="AKI Risk")


# 制图
plot(nom3,)
plot(nom3,cex=0.5)





# C-Statistics计算
library(foreign)
library(rms)
head(ptbdata1)

# 把数据加载到当前???作环境并打包数据
dd <- datadist(ptbdata1)
options(datadist='dd')
# 拟合Logistic回归模型

f2 <- lrm(Group~T.SPOT+URBC+NRI+UM+ALB+CYS.C+B2MG, data = ptbdata1,x=T,y=T)
f2


# 计算模型预测概率
ptbdata1$predvalue<-predict(f1)

ptbdata1

#载???ROCR包
install.packages("ROCR")
library(ROCR)
#计算过程
pred <- prediction(ptbdata1$predvalue, ptbdata1$Group)
perf <- performance(pred,"tpr","fpr")
perf
#绘制ROC曲线
plot(perf)
abline(0,1, col = 3, lty = 2)
auc <- performance(pred,"auc")
auc
# 加载Hmisc包计算C-Statistics值
install.packages("Himsc")
library("Hmisc")
c1 <- somers2(ptbdata1$predvalue, ptbdata1$Group)
c1

f1

#校正曲线
cal <- calibrate(f1,method = "boot",B=100)
cal
plot(cal,xlim=c(0,1.0),ylim=c(0,1.0))
