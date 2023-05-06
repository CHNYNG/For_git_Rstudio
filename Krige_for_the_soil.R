# 加载所需包
library(gstat)
library(ggplot2)
library(sp)
# 读取土壤样本数据
HSD_env <- read.csv("E:/Chu Lab/HSD.origin/soil_origin.csv",header = T,fileEncoding = "GBK")
HSD_env_sp <- SpatialPointsDataFrame(coords=HSD_env[,c("gx", "gy")], data=HSD_env)
soil_data <- read.csv("soil_data.csv")  # 假设你的土壤样本数据保存在名为 "soil_data.csv" 的csv文件中

# 创建空间数据框（SpatialPointsDataFrame）
coordinates(HSD_env) <- c("gx", "gy")  # 假设你的经度和纬度数据分别保存在 "经度" 和 "纬度" 列中
proj4string(HSD_env) <- CRS("+proj=longlat +datum=WGS84")  # 设置地理坐标参考系统

# 克里金插值
vgm <- variogram(土壤元素 ~ 1, HSD_env)  # 假设你的土壤元素数据保存在 "土壤元素" 列中
fit <- fit.variogram(vgm, model = vgmModel("Exp"))  # 选择指数模型作为插值模型
soil_grid <- krige(土壤元素 ~ 1, HSD_env, fit)  # 进行克里金插值

# 绘制插值结果
ggplot() +
  geom_tile(data = as.data.frame(soil_grid), aes(x = x, y = y, fill = var1.pred)) +
  scale_fill_viridis() +  # 可根据需要选择不同的调色板
  xlab("经度") +
  ylab("纬度") +
  theme_minimal()
