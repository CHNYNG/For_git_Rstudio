
###先加载数据

load("E:/黑石顶测菌根/菌根侵染率/数据整理/tmp/For_git_Rstudio/root_qrl_soil.RData")




###计算一下乔木生长速率
gr_rate <- (log(d$DBH2)-log(d$DBH1))


library(ggplot2)
#对样本总体分析
#####
#1.相关性分析
######
# 画散点图和回归线
ggplot(df, aes(x=菌根侵染率, y=乔木生长速率)) + 
  geom_point() + 
  geom_smooth(method="lm", se=FALSE)

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
ggplot(df, aes(x=菌根侵染率, y=乔木生长速率)) + 
  geom_point() + 
  geom_smooth(method="lm", se=FALSE) +
  xlab("菌根侵染率") + 
  ylab("乔木生长速率") +
  theme_minimal()

#4.方差分析
#####
# 画箱线图
ggplot(df, aes(x=乔木树种, y=菌根侵染率)) + 
  geom_boxplot() +
  xlab("乔木树种") +
  ylab("菌根侵染率") +
  theme_minimal()

# 进行多重比较
TukeyHSD(model)

#对不同物种
#########
# 使用ggplot2包绘制箱线图，以乔木树种作为分组变量
library(ggplot2)

ggplot(df, aes(x=乔木树种, y=乔木生长速率, fill=物种)) + 
  geom_boxplot() +
  xlab("乔木树种") +
  ylab("乔木生长速率") +
  theme_minimal()

# 使用facet_grid()函数绘制多图合并，以乔木树种作为分组变量
ggplot(df, aes(x=菌根侵染率, y=乔木生长速率)) + 
  geom_point() + 
  geom_smooth(method="lm", se=FALSE) +
  facet_grid(~物种) +
  xlab("菌根侵染率") + 
  ylab("乔木生长速率") +
  theme_minimal()

# 使用tapply()函数计算不同物种的平均菌根侵染率
mean_by_species <- tapply(df$菌根侵染率, df$物种, mean)

# 使用barplot()函数绘制不同物种的平均菌根侵染率柱状图
barplot(mean_by_species, 
        main="不同物种的平均菌根侵染率", 
        xlab="物种", 
        ylab="平均菌根侵染率")

