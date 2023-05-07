# 加载所需包
library(gstat)
library(automap)
library(rgdal)
library(rgeos)
library(ggplot2)
library(sp)

#好像好了哎

# 读取土壤样本数据
HSD_env <- read.csv("E:/Chu Lab/HSD.origin/soil_origin.csv",header = T,fileEncoding = "GBK")

# 删除HSD_env中包含NA的行
HSD_env_s <- HSD_env[,c("gx","gy","pH")]

#将数据框转化为SpatialPointsDataFrame对象
#HSD_env_sp <- SpatialPointsDataFrame(coords=HSD_env_s[,c("gx", "gy")], data=HSD_env_s)
proj4string(HSD_env_sp) <- crs
HSD_env_sp <- SpatialPointsDataFrame(coords = HSD_env_s[,c("gx", "gy")], data = data.frame(pH = HSD_env_s$pH))
proj4string(HSD_env_sp) <- CRS("+proj=utm +zone=48 +datum=WGS84")


#创建插值点的坐标
HSD_line <- as.data.frame(root_qrl[,c("GX","GY")])
colnames(HSD_line) <- c("gx","gy")
HSD_line <- na.omit(HSD_line)

#定义插值模型
#model <- gstat::vgm(psill =1, model = "Sph", range = 2)
#自动拟合
fit_auto <- autofitVariogram(pH ~ 1, HSD_env_sp)

#进行克里金插值
#krige_result <- gstat::krige(param~1, HSD_env_sp, HSD_line, nmax = 20, model=model)
pred_auto <- autoKrige(pH ~ 1, HSD_env_sp,HSD_line)

# 绘制插值结果
ggplot() +
  geom_tile(data = krige_result, aes(x, y, fill = var1.pred)) +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_point(data = HSD_env_sp, aes(x, y, color = param), size = 2) +
  scale_color_gradient(low = "yellow", high = "red") +
  theme_bw

