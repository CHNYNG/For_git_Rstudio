
###先加载数据

load("E:/黑石顶测菌根/菌根侵染率/数据整理/tmp/For_git_Rstudio/root_qrl_soil.RData")


###计算一下乔木生长速率
d$gr_rate <- (log(d$DBH2)-log(d$DBH1))


library(ggplot2)
#对样本总体分析
#####
#1.相关性分析
######
# 画散点图和回归线
library(dplyr)

####进行boxcox转换
library(MASS)
# 对d_AM$qr_AM列使用Box-Cox转换
d_AM$qr_AM_bc <- boxcox(as.data.frame(d_AM$qr_AM ~ 1))

boxcox(qr_AM ~ 1, data = d_AM,lambda = seq(4,5, length.out = 10))

hist(d_AM$qr_AM^(4.6))
r2=lm(gr_rate~qr_AM^4.6, data=d_AM)
hist(r2$residuals)
summary(r2)

a = (d_AM$qr_AM)^4.6

plot(d_AM$gr_rate~a)

hist(d_AM$gr_rate)
cor.test(d_AM$qr_AM, d_AM$gr_rate)
d_AM <- subset(d, !is.na(qr_AM))


# 拟合二项式回归模型
fit <- glm(gr_rate ~ qr_AM, data = d_AM, family = binomial(link = "logit"))

# 绘制散点图和拟合曲线
ggplot(dat, aes(x = qr_AM, y = y)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = FALSE) +
  ggtitle(paste0("Logistic Regression (AIC = ", round(AIC(fit), 2), ")"))



######
#2.主成分分析
######
# 画散点图
ggplot(data.frame(PC1=pca$x[,1], PC2=pca$x[,2])) +
  geom_point(aes(x=PC1, y=PC2))

#####
#3.线性回归分析
#####
# 画散点图和回归线
ggplot(d, aes(x=qr_AM, y=gr_rate)) + 
  geom_point() + 
  geom_smooth(method="lm", se=FALSE) +
  xlab("qr_AM") + 
  ylab("gr_rate") +
  theme_minimal()

#4.方差分析
#####
# 画箱线图
ggplot(d, aes(x=Species.x, y=qr_AM)) + 
  geom_boxplot() +
  xlab("Species.x") +
  ylab("qr_AM") +
  theme_minimal()

# 进行多重比较
TukeyHSD(model)

#对不同物种
#########
# 使用ggplot2包绘制箱线图，以乔木树种作为分组变量
library(ggplot2)

ggplot(d, aes(x=Species.x, y=gr_rate, fill=物种)) + 
  geom_boxplot() +
  xlab("Species.x") +
  ylab("gr_rate") +
  theme_minimal()

# 使用facet_grid()函数绘制多图合并，以乔木树种作为分组变量
ggplot(d, aes(x=qr_AM, y=gr_rate)) + 
  geom_point() + 
  geom_smooth(method="lm", se=FALSE) +
  facet_grid(~物种) +
  xlab("qr_AM") + 
  ylab("gr_rate") +
  theme_minimal()

# 使用tapply()函数计算不同物种的平均菌根侵染率
mean_by_species <- tapply(d$qr_AM, d$物种, mean)

# 使用barplot()函数绘制不同物种的平均菌根侵染率柱状图
barplot(mean_by_species, 
        main="不同物种的平均菌根侵染率", 
        xlab="物种", 
        ylab="平均菌根侵染率")

