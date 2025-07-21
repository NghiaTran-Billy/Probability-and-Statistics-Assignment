# install.packages("stringr")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("zoo")
# install.packages("Metrics")
# install.packages("caret")
# install.packages("MASS")
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("mltools")
# install.packages("DescTools")
# install.packages("plotly")

# Thư viện cần thiết
library(stringr)
library(tidyr)
library(dplyr)
library(zoo)
library(Metrics)
library(caret)
library(MASS)
library(ggplot2)
library(reshape2)
library(mltools)
library(DescTools)
library(plotly)

# Đọc dữ liệu
GPU = read.csv("D:/DHBK/HK243/XSTK/All_GPUs.csv",header=TRUE,na.strings=c("","\n- ","\n","\nUnknown Release Date "))

# Chọn biến
GPU_new = GPU[,c("Boost_Clock", "Core_Speed", "Memory_Speed", "ROPs", "TMUs", "Memory_Bus", "Memory", "Process", "Shader", "Pixel_Rate", "Texture_Rate")]

# Kiểm tra kiểu dữ liệu và số dữ liệu trống
print( summary(GPU_new) )
print( apply(is.na(GPU_new),2,sum) )

#--------------------------------------------------
# Tiền xử lí số liệu

# Định nghĩa helper
helper <- function(x){ 
  if(is.na(x)) return(NA)
  as.double(strsplit(as.character(x), " ")[[1]][1])
  #strsplit return về 1 list các list: vd 7 MHz -> list(list(7),list(MHz))
  #strsplit[[1]] để truy cập vào list đầu tiên ->list(7)
  #strsplit[[1]][[1]] truy cập phần tử đầu tiên trong list -> 7
}

# Boost_Clock
GPU_new$Boost_Clock <- sapply(GPU_new$Boost_Clock, helper)
GPU_new$Boost_Clock[is.na(GPU_new$Boost_Clock)] <- median(GPU_new$Boost_Clock, na.rm = TRUE)

# Core_Speed
GPU_new$Core_Speed <- sapply(GPU_new$Core_Speed, helper)
GPU_new$Core_Speed[is.na(GPU_new$Core_Speed)] <- median(GPU_new$Core_Speed, na.rm = TRUE)

# Memory_Speed
GPU_new$Memory_Speed <- sapply(GPU_new$Memory_Speed, helper)
GPU_new$Memory_Speed[is.na(GPU_new$Memory_Speed)] <- median(GPU_new$Memory_Speed, na.rm = TRUE)

# ROPs
# nếu còn ở dạng character, dùng helper; nếu đã numeric thì bỏ helper
GPU_new$ROPs <- as.double(GPU_new$ROPs)
# nếu NA (ví dụ do parse lỗi), thay bằng median
GPU_new$ROPs[is.na(GPU_new$ROPs)] <- median(GPU_new$ROPs, na.rm = TRUE)

# TMUs
GPU_new$TMUs <- as.double(GPU_new$TMUs)
GPU_new$TMUs[is.na(GPU_new$TMUs)] <- median(GPU_new$TMUs, na.rm = TRUE)

# Memory_Bus
GPU_new$Memory_Bus <- sapply(GPU_new$Memory_Bus, helper)
GPU_new$Memory_Bus[is.na(GPU_new$Memory_Bus)] <- median(GPU_new$Memory_Bus, na.rm = TRUE)

# Memory
GPU_new$Memory <- sapply(GPU_new$Memory, helper)
GPU_new$Memory[is.na(GPU_new$Memory)] <- median(GPU_new$Memory, na.rm = TRUE)

# Process
GPU_new$Process <- as.double(gsub("[^0-9\\.]", "", as.character(GPU_new$Process)))
GPU_new$Process[is.na(GPU_new$Process)] <- median(GPU_new$Process, na.rm = TRUE)

# Shaders
GPU_new$Shader <- as.double(GPU_new$Shader)
med_shader <- median(GPU_new$Shader, na.rm = TRUE)
GPU_new$Shader[is.na(GPU_new$Shader)] <- median(GPU_new$Shader, na.rm = TRUE)

# Pixel_Rate 
GPU_new$Pixel_Rate <- sapply(GPU_new$Pixel_Rate, helper) 
GPU_new$Pixel_Rate[is.na(GPU_new$Pixel_Rate)] <- median(GPU_new$Pixel_Rate, na.rm = TRUE)

# Texture_Rate
GPU_new$Texture_Rate <- sapply(GPU_new$Texture_Rate, helper)
GPU_new$Texture_Rate[is.na(GPU_new$Texture_Rate)] <- median(GPU_new$Texture_Rate, na.rm = TRUE)

# Loại bỏ dòng trùng lặp
GPU_new <- dplyr::distinct(GPU_new)

# Kiểm tra lại
str(GPU_new)
print(colSums(is.na(GPU_new)))

#--------------------------------------------------
# Thống kê mô tả

# Định nghĩa lại danh sách biến số
numerical <- c(
  "Boost_Clock", "Core_Speed", "Memory_Speed",
  "ROPs", "TMUs", "Memory_Bus",
  "Memory", "Process", "Shader"
)

# Tạo dataframe log-transformed
GPU_new_log <- GPU_new
GPU_new_log[, numerical] <- log(GPU_new_log[, numerical])

# Xem 6 dòng đầu của GPU_new_log
head(GPU_new_log)

# Kiểm tra xem có giá trị < 0 trong dữ liệu gốc
for (var in numerical) {
  has_negative <- any(GPU_new[[var]] < 0, na.rm = TRUE)
  message(var, " in original < 0? ", has_negative)
}

# Kiểm tra xem có giá trị < 0 sau khi log-transform
for (var in numerical) {
  has_negative_log <- any(GPU_new_log[[var]] < 0, na.rm = TRUE)
  message(var, " in log-transformed < 0? ", has_negative_log)
}

# Thống kê nhanh trên GPU_new (sau tiền xử lý, trước log)
print(summary(GPU_new))
print(colSums(is.na(GPU_new)))

# Xem 10 dòng đầu của GPU_new
head(GPU_new, 10)

# Lập bảng tính các giá trị thống kê mô tả cho GPU_new và GPU_new_log
mean<-apply(GPU_new[,numerical],2,mean)
sd<-apply(GPU_new[,numerical],2,sd)
min<-apply(GPU_new[,numerical],2,min)
max<-apply(GPU_new[,numerical],2,max)
median<-apply(GPU_new[,numerical],2,median)
data.frame(mean,sd,min,max,median)

mean<-apply(GPU_new_log[,numerical],2,mean)
sd<-apply(GPU_new_log[,numerical],2,sd)
min<-apply(GPU_new_log[,numerical],2,min)
max<-apply(GPU_new_log[,numerical],2,max)
median<-apply(GPU_new_log[,numerical],2,median)
data.frame(mean,sd,min,max,median)

# Chia layout thành 3 hàng và 3 cột
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

# Vẽ histogram cho từng biến numerical trong GPU_new
for (i in 1:length(numerical)) {
  hist_data <- GPU_new[[numerical[i]]]
  hist(hist_data, 
       xlab = names(GPU_new)[which(names(GPU_new) == numerical[i])], 
       main = paste("Histogram of", names(GPU_new)[which(names(GPU_new) == numerical[i])]), 
       labels = TRUE, 
       col = "blue")
}

# Chia layout thành 2 hàng và 4 cột
par(mfrow=c(3, 3))

# Vẽ histogram cho từng biến numerical trong GPU_new_log
for (i in 1:length(numerical)) {
  hist_data <- GPU_new_log[[numerical[i]]]
  hist(hist_data, 
       xlab = names(GPU_new_log)[which(names(GPU_new_log) == numerical[i])], 
       main = paste("Histogram of log(", names(GPU_new_log)[which(names(GPU_new_log) == numerical[i])], ")"), 
       labels = TRUE, 
       col = "red")
}
# Reset layout về mặc định
par(mfrow = c(1, 1))

# Chia khung vẽ thành 2 hàng × 2 cột
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# Histogram Pixel_Rate
hist(GPU_new$Pixel_Rate,
     main   = "Histogram of Pixel_Rate",
     xlab   = "Pixel_Rate (GPixel/s)",
     breaks = 30,
     col    = "lightblue")

# Histogram log(Pixel_Rate)
hist(GPU_new_log$Pixel_Rate,
     main   = "Histogram of log(Pixel_Rate)",
     xlab   = "log(Pixel_Rate)",
     breaks = 30,
     col    = "steelblue")

# Histogram Texture_Rate
hist(GPU_new$Texture_Rate,
     main   = "Histogram of Texture_Rate",
     xlab   = "Texture_Rate (GTexel/s)",
     breaks = 30,
     col    = "lightpink")

# Histogram log(Texture_Rate)
hist(GPU_new_log$Texture_Rate,
     main   = "Histogram of log(Texture_Rate)",
     xlab   = "log(Texture_Rate)",
     breaks = 30,
     col    = "tomato")

# Reset layout về mặc định
par(mfrow = c(1, 1))

# Vẽ bảng barplot thể hiện phân phối biến định luọng
#par(mfrow=c(1,3))
#barplot(table(GPU_new$Manufacturer), xlab="Manufacturer", ylab="Frequency", main="Barplot of Manufacturer", col=c("red","green","blue"))
#barplot(table(GPU_new$Memory_Type), xlab="Memory_Type", ylab="Frequency", main="Barplot of Memory_Type", col=c("red","green","blue"))
#barplot(table(GPU_new$Resolution_WxH), xlab="Resolution_WxH", ylab="Frequency", main="Barplot of Resolution_WxH", col=c("red","green","blue"))

# Vẽ boxplot diễn tả 5 vị trí phân bố dữ liệu của biến định lượng
#par(mfrow=c(1,2))
#boxplot(Memory~Manufacturer, data=GPU_new, main="boxplot of memory for manufacturer", col="blue")
#boxplot(Memory~Manufacturer, data=GPU_new_log, main="boxplot of log(memory) for manufacturer", col="red")

#par(mfrow=c(1,2))
#boxplot(Memory~Memory_Type, data=GPU_new, main="boxplot of memory for memory_type", col="blue")
#boxplot(Memory~Memory_Type, data=GPU_new_log, main="boxplot of log(memory) for memory_type", col="red")

#par(mfrow=c(1,2))
#boxplot(Memory~Resolution_WxH, data=GPU_new, main="boxplot of memory for memory_type", col="blue")
#boxplot(Memory~Resolution_WxH, data=GPU_new_log, main="boxplot of log(memory) for memory_type", col="red")

# Chia layout thành 9 hàng và 2 cột
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
#par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
# Vẽ scatter plot
# Boost_Clock vs Pixel_Rate
plot(GPU_new$Boost_Clock, GPU_new$Pixel_Rate,
     xlab = "Boost_Clock", ylab = "Pixel_Rate",
     main = "Pixel_Rate vs Boost_Clock", col = "blue", pch = 20)
fit1 <- lm(Pixel_Rate ~ Boost_Clock, data = GPU_new)
abline(fit1, col = "red")

# Boost_Clock vs Texture_Rate
plot(GPU_new$Boost_Clock, GPU_new$Texture_Rate,
     xlab = "Boost_Clock", ylab = "Texture_Rate",
     main = "Texture_Rate vs Boost_Clock", col = "darkgreen", pch = 20)
fit2 <- lm(Texture_Rate ~ Boost_Clock, data = GPU_new)
abline(fit2, col = "red")

# Core_Speed vs Pixel_Rate
plot(GPU_new$Core_Speed, GPU_new$Pixel_Rate,
     xlab = "Core_Speed", ylab = "Pixel_Rate",
     main = "Pixel_Rate vs Core_Speed", col = "blue", pch = 20)
fit3 <- lm(Pixel_Rate ~ Core_Speed, data = GPU_new)
abline(fit3, col = "red")

# Core_Speed vs Texture_Rate
plot(GPU_new$Core_Speed, GPU_new$Texture_Rate,
     xlab = "Core_Speed", ylab = "Texture_Rate",
     main = "Texture_Rate vs Core_Speed", col = "darkgreen", pch = 20)
fit4 <- lm(Texture_Rate ~ Core_Speed, data = GPU_new)
abline(fit4, col = "red")

# Memory_Speed vs Pixel_Rate
plot(GPU_new$Memory_Speed, GPU_new$Pixel_Rate,
     xlab = "Memory_Speed", ylab = "Pixel_Rate",
     main = "Pixel_Rate vs Memory_Speed", col = "blue", pch = 20)
fit5 <- lm(Pixel_Rate ~ Memory_Speed, data = GPU_new)
abline(fit5, col = "red")

# Memory_Speed vs Texture_Rate
plot(GPU_new$Memory_Speed, GPU_new$Texture_Rate,
     xlab = "Memory_Speed", ylab = "Texture_Rate",
     main = "Texture_Rate vs Memory_Speed", col = "darkgreen", pch = 20)
fit6 <- lm(Texture_Rate ~ Memory_Speed, data = GPU_new)
abline(fit6, col = "red")

# ROPs vs Pixel_Rate
plot(GPU_new$ROPs, GPU_new$Pixel_Rate,
     xlab = "ROPs", ylab = "Pixel_Rate",
     main = "Pixel_Rate vs ROPs", col = "blue", pch = 20)
fit7 <- lm(Pixel_Rate ~ ROPs, data = GPU_new)
abline(fit7, col = "red")

# ROPs vs Texture_Rate
plot(GPU_new$ROPs, GPU_new$Texture_Rate,
     xlab = "ROPs", ylab = "Texture_Rate",
     main = "Texture_Rate vs ROPs", col = "darkgreen", pch = 20)
fit8 <- lm(Texture_Rate ~ ROPs, data = GPU_new)
abline(fit8, col = "red")

# TMUs vs Pixel_Rate
plot(GPU_new$TMUs, GPU_new$Pixel_Rate,
     xlab = "TMUs", ylab = "Pixel_Rate",
     main = "Pixel_Rate vs TMUs", col = "blue", pch = 20)
fit9 <- lm(Pixel_Rate ~ TMUs, data = GPU_new)
abline(fit9, col = "red")

# TMUs vs Texture_Rate
plot(GPU_new$TMUs, GPU_new$Texture_Rate,
     xlab = "TMUs", ylab = "Texture_Rate",
     main = "Texture_Rate vs TMUs", col = "darkgreen", pch = 20)
fit10 <- lm(Texture_Rate ~ TMUs, data = GPU_new)
abline(fit10, col = "red")

# Memory_Bus vs Pixel_Rate
plot(GPU_new$Memory_Bus, GPU_new$Pixel_Rate,
     xlab = "Memory_Bus", ylab = "Pixel_Rate",
     main = "Pixel_Rate vs Memory_Bus", col = "blue", pch = 20)
fit11 <- lm(Pixel_Rate ~ Memory_Bus, data = GPU_new)
abline(fit11, col = "red")

# Memory_Bus vs Texture_Rate
plot(GPU_new$Memory_Bus, GPU_new$Texture_Rate,
     xlab = "Memory_Bus", ylab = "Texture_Rate",
     main = "Texture_Rate vs Memory_Bus", col = "darkgreen", pch = 20)
fit12 <- lm(Texture_Rate ~ Memory_Bus, data = GPU_new)
abline(fit12, col = "red")

# Memory vs Pixel_Rate
plot(GPU_new$Memory, GPU_new$Pixel_Rate,
     xlab = "Memory", ylab = "Pixel_Rate",
     main = "Pixel_Rate vs Memory", col = "blue", pch = 20)
fit13 <- lm(Pixel_Rate ~ Memory, data = GPU_new)
abline(fit13, col = "red")

# Memory vs Texture_Rate
plot(GPU_new$Memory, GPU_new$Texture_Rate,
     xlab = "Memory", ylab = "Texture_Rate",
     main = "Texture_Rate vs Memory", col = "darkgreen", pch = 20)
fit14 <- lm(Texture_Rate ~ Memory, data = GPU_new)
abline(fit14, col = "red")

# Process vs Pixel_Rate
plot(GPU_new$Process, GPU_new$Pixel_Rate,
     xlab = "Process", ylab = "Pixel_Rate",
     main = "Pixel_Rate vs Process", col = "blue", pch = 20)
fit15 <- lm(Pixel_Rate ~ Process, data = GPU_new)
abline(fit15, col = "red")

# Process vs Texture_Rate
plot(GPU_new$Process, GPU_new$Texture_Rate,
     xlab = "Process", ylab = "Texture_Rate",
     main = "Texture_Rate vs Process", col = "darkgreen", pch = 20)
fit16 <- lm(Texture_Rate ~ Process, data = GPU_new)
abline(fit16, col = "red")

# Shader vs Pixel_Rate
plot(GPU_new$Shader, GPU_new$Pixel_Rate,
     xlab = "Shader", ylab = "Pixel_Rate",
     main = "Pixel_Rate vs Shader", col = "blue", pch = 20)
fit17 <- lm(Pixel_Rate ~ Shader, data = GPU_new)
abline(fit17, col = "red")

# Shader vs Texture_Rate
plot(GPU_new$Shader, GPU_new$Texture_Rate,
     xlab = "Shader", ylab = "Texture_Rate",
     main = "Texture_Rate vs Shader", col = "darkgreen", pch = 20)
fit18 <- lm(Texture_Rate ~ Shader, data = GPU_new)
abline(fit18, col = "red")

# Reset layout
par(mfrow = c(1, 1))

#--------------------------------------------------
# Thống kê suy diễn (chưa làm)

# Tạo một dataframe để lưu kết quả
correlation_results <- data.frame(
  Variable = character(),
  Pearson_Correlation = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Lặp qua từng biến trong numerical và tính hệ số tương quan Pearson
for (var1 in names(GPU_new[numerical])) {
  test_result <- cor.test(GPU_new[[var1]], GPU_new$Memory)
  
  # Thêm kết quả vào dataframe
  correlation_results <- rbind(correlation_results, 
                               data.frame(Memory = var1,
                                          Pearson_Correlation = test_result$estimate,
                                          P_Value = test_result$p.value))
}

# In ra dataframe kết quả
print(correlation_results)

pairs(GPU_new_log[numerical])

# index <- createDataPartition(GPU_new_log$Memory, p = 2/3, list = FALSE)
# train <- GPU_new_log[index,]
# test <- GPU_new_log[index,]
index <- createDataPartition(GPU_new_log$Memory, p = 2/3, list = FALSE)
train <- GPU_new_log[index,]
test <- GPU_new_log[index,]

model <- lm(Memory ~ ., data = train)

print( summary(model) )

predicted_value =  predict(model, newdata= test)

#Thêm 2 cột predicted_value và error vào tập test
test$predicted_value <- predicted_value
test$error <- predicted_value - test$Memory

# So sánh giá trị tiên đoán (red) và giá trị thực tế (blue)
test %>% 
  gather(Memory, predicted_value, key = "Y", value = "Memory") %>%
  ggplot(aes(x = Memory, fill = Y)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red")) +
  theme_bw()

# Tính toán MAE và MSE
mae_value <- mae(predicted_value, test$Memory)
mse_value <- mse(predicted_value, test$Memory)

# In ra kết quả
cat("MAE (trung bình của sai biệt tuyệt đối): ", mae_value, "\n")
cat("MSE (trung bình bình phương sai số): ", mse_value, "\n")


model_ANOVA <- aov(Memory ~ Manufacturer + Resolution_WxH + Memory_Type, data= GPU_new_log)
summary(model_ANOVA)

TukeyHSD(model_ANOVA)