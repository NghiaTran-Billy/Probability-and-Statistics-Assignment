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
GPU = read.csv("/home/nghiatran/Code_R/All_GPUs.csv",header=TRUE,na.strings=c("","\n- ","\n","\nUnknown Release Date "))

# Chọn biến
GPU_new = GPU[,c("Resolution_WxH", "Manufacturer", "Boost_Clock", "Core_Speed", "Memory_Speed", "Memory_Type", 
                    "ROPs", "TMUs", "Memory_Bus", "Memory", "Process", "Shader", "Pixel_Rate")]

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

# Loại bỏ những kí tự không phải số
to_num <- function(x) {

  cleaned <- gsub("[^0-9\\.]", "", x)
  as.numeric(cleaned)
}

# Resolution_WxH
GPU_new$Resolution_WxH[is.na(GPU_new$Resolution_WxH)] = "4096x2160"
GPU_new$Resolution_WxH <- ifelse(GPU_new$Resolution_WxH == "4096x2160", 1, 
                        ifelse(GPU_new$Resolution_WxH == "2560x1600", 2, 3)) # Gom nhóm: 4096x2160 (39%), 2560x1600 (34%), Other (26%)
GPU_new$Resolution_WxH = factor(GPU_new$Resolution_WxH)

# Manufacturer
GPU_new$Manufacturer = factor(GPU_new$Manufacturer)

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
# Giả sử GPU_new$ROPs đang là character, chứa các giá trị như "24 (x4)", "48 (x2)", v.v.
GPU_new$ROPs <- as.numeric(sub("^([0-9]+).*", "\\1", GPU_new$ROPs))
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

# Memory_Type
GPU_new <- GPU_new[complete.cases(GPU_new$Memory_Type), ]
GPU_new$Memory_Type = gsub("[^A-Za-z]+.*","",GPU_new$Memory_Type)
GPU_new$Memory_Type = factor(GPU_new$Memory_Type)

# Process
GPU_new$Process <- as.double(gsub("[^0-9\\.]", "", as.character(GPU_new$Process)))
GPU_new$Process[is.na(GPU_new$Process)] <- median(GPU_new$Process, na.rm = TRUE)

# Shader
GPU_new$Shader <- as.double(GPU_new$Shader)
GPU_new$Shader[is.na(GPU_new$Shader)] <- median(GPU_new$Shader, na.rm = TRUE)

# Pixel_Rate 
GPU_new$Pixel_Rate <- sapply(GPU_new$Pixel_Rate, helper) 
GPU_new$Pixel_Rate[is.na(GPU_new$Pixel_Rate)] <- median(GPU_new$Pixel_Rate, na.rm = TRUE)

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

categorical <- c("Manufacturer","Memory_Type","Resolution_WxH")

# Tạo dataframe log-transformed
GPU_new_log <- GPU_new
GPU_new_log[, numerical] <- log(GPU_new_log[, numerical])
GPU_new_log$Pixel_Rate <- log(GPU_new_log$Pixel_Rate)

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

# Chia layout thành 3 hàng và 3 cột
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

# Vẽ histogram cho từng biến numerical trong GPU_new_log
for (i in 1:length(numerical)) {
  hist_data <- GPU_new_log[[numerical[i]]]
  hist(hist_data, 
       xlab = names(GPU_new_log)[which(names(GPU_new_log) == numerical[i])], 
       main = paste("Histogram of log(", names(GPU_new_log)[which(names(GPU_new_log) == numerical[i])], ")"), 
       labels = TRUE, 
       col = "red")
}

# Chia layout thành 1 hàng và 2 cột
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

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
     col    = "lightpink")

# Chia layout thành 1 hàng và 3 cột
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))

# Vẽ bảng barplot thể hiện phân phối biến định luọng
barplot(table(GPU_new$Manufacturer), xlab="Manufacturer", ylab="Frequency", main="Barplot of Manufacturer", col=c("red","green","blue"))
barplot(table(GPU_new$Memory_Type), xlab="Memory_Type", ylab="Frequency", main="Barplot of Memory_Type", col=c("red","green","blue"))
barplot(table(GPU_new$Resolution_WxH), xlab="Resolution_WxH", ylab="Frequency", main="Barplot of Resolution_WxH", col=c("red","green","blue"))

# Vẽ boxplot diễn tả 5 vị trí phân bố dữ liệu của biến định lượng
par(mfrow=c(1,2))
boxplot(Pixel_Rate~Manufacturer, data=GPU_new, main="boxplot of Pixel_Rate for Manufacturer", col="blue")
boxplot(Pixel_Rate~Manufacturer, data=GPU_new_log, main="boxplot of log(Pixel_Rate) for Manufacturer", col="red")

par(mfrow=c(1,2))
boxplot(Pixel_Rate~Memory_Type, data=GPU_new, main="boxplot of Pixel_Rate for Memory_Type", col="blue")
boxplot(Pixel_Rate~Memory_Type, data=GPU_new_log, main="boxplot of log(Pixel_Rate) for Memory_Type", col="red")

par(mfrow=c(1,2))
boxplot(Pixel_Rate~Resolution_WxH, data=GPU_new, main="boxplot of Pixel_Rate for Resolution_WxH", col="blue")
boxplot(Pixel_Rate~Resolution_WxH, data=GPU_new_log, main="boxplot of log(Pixel_Rate) for Resolution_WxH", col="red")

# Chia layout thành 3 hàng và 2 cột
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))

# Vẽ scatter plot
# Core_Speed vs Pixel_Rate
plot(GPU_new$Core_Speed, GPU_new$Pixel_Rate,
     xlab = "Core_Speed", ylab = "Pixel_Rate",
     main = "Pixel_Rate and Core_Speed", col = "blue", pch = 20)
fit1 <- lm(Pixel_Rate ~ Core_Speed, data = GPU_new)
abline(fit1, col = "red")

# log(Core_Speed) vs log(Pixel_Rate)
plot(GPU_new_log$Core_Speed, GPU_new_log$Pixel_Rate, 
     xlab = "log(Core_Speed)", ylab = "log(Pixel_Rate)", 
     main = "log(Pixel_Rate) and log(Core_Speed)", col = "red", pch = 20)
fit2 <- lm(Pixel_Rate ~ Core_Speed, data = GPU_new_log)
abline(fit2, col = "blue")

# ROPs vs Pixel_Rate
plot(GPU_new$ROPs, GPU_new$Pixel_Rate,
     xlab = "ROPs", ylab = "Pixel_Rate",
     main = "Pixel_Rate and ROPs", col = "blue", pch = 20)
fit3 <- lm(Pixel_Rate ~ ROPs, data = GPU_new)
abline(fit3, col = "red")

# log(ROPs) vs log(Pixel_Rate)
plot(GPU_new_log$ROPs, GPU_new_log$Pixel_Rate, 
     xlab = "log(ROPs)", ylab = "log(Pixel_Rate)", 
     main = "log(Pixel_Rate) and log(ROPs)", col = "red", pch = 20)
fit4 <- lm(Pixel_Rate ~ ROPs, data = GPU_new_log)
abline(fit4, col = "blue")

# Shader vs Pixel_Rate
plot(GPU_new$Shader, GPU_new$Pixel_Rate,
     xlab = "Shader", ylab = "Pixel_Rate",
     main = "Pixel_Rate and Shader", col = "blue", pch = 20)
fit5 <- lm(Pixel_Rate ~ Shader, data = GPU_new)
abline(fit5, col = "red")

# log(Core_Speed) vs log(Pixel_Rate)
plot(GPU_new_log$Core_Speed, GPU_new_log$Pixel_Rate, 
     xlab = "log(Shader)", ylab = "log(Pixel_Rate)", 
     main = "log(Pixel_Rate) and log(Shader)", col = "red", pch = 20)
fit6 <- lm(Pixel_Rate ~ Shader, data = GPU_new_log)
abline(fit6, col = "blue")

# Reset layout
par(mfrow = c(1, 1))

#--------------------------------------------------
# Thống kê suy diễn và mô hình hóa dữ liệu GPU

# Tính tương quan Pearson giữa mỗi biến định lượng và Pixel_Rate
# Khởi tạo một dataframe rỗng để lưu kết quả tương quan
correlation_results <- data.frame(
  Variable = character(),
  Pearson_Correlation = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Duyệt qua từng biến định lượng để tính hệ số tương quan Pearson
for (var1 in names(GPU_new[numerical])) {
     test_result <- cor.test(GPU_new[[var1]], GPU_new$Pixel_Rate)

     # Lưu kết quả vào bảng kết quả
     correlation_results <- rbind(
       correlation_results, 
       data.frame(
         Variable = var1,
         Pearson_Correlation = test_result$estimate,
         P_Value = test_result$p.value
       )
     )
}

# Hiển thị bảng kết quả hệ số tương quan và p-value
print(correlation_results)

# Kiểm tra phân bố và mối quan hệ giữa các biến định lượng (sau khi log)
# Sử dụng biểu đồ scatter matrix để xem nhanh mối liên hệ giữa các biến đã log-transform
pairs(GPU_new_log[numerical])

# Chia dữ liệu thành tập huấn luyện và kiểm tra (2/3 - 1/3)
# Dữ liệu đã log-transform trước đó
index <- createDataPartition(GPU_new_log$Pixel_Rate, p = 2/3, list = FALSE)
train <- GPU_new_log[index, ]
test  <- GPU_new_log[-index, ]

# Huấn luyện mô hình hồi quy tuyến tính đa biến
model <- lm(Pixel_Rate ~ ., data = train)

# Hiển thị summary để đánh giá hệ số, R-squared, p-value v.v.
print(summary(model))

# Dự đoán và đánh giá mô hình trên tập kiểm tra
predicted_value <- predict(model, newdata = test)

# Thêm cột giá trị dự đoán và sai số vào tập test
test$predicted_value <- predicted_value
test$error <- predicted_value - test$Pixel_Rate

# Trực quan hóa phân phối thực tế vs dự đoán
# Chuẩn hóa dữ liệu theo định dạng long để dễ vẽ biểu đồ
test_long <- test %>%
     select(Pixel_Rate, predicted_value) %>%
     gather(key = "Type", value = "Value")

# Vẽ biểu đồ mật độ (density plot)
x11()
ggplot(test_long, aes(x = Value, fill = Type)) +
     geom_density(alpha = 0.5) +
     scale_fill_manual(values = c("blue", "red")) +
     theme_bw() +
     labs(title = "Thực tế (blue) vs Dự đoán (red)", x = "Pixel_Rate")

# Tính toán các chỉ số sai số dự đoán
mae_value <- mae(predicted_value, test$Pixel_Rate)
mse_value <- mse(predicted_value, test$Pixel_Rate)

# In ra kết quả sai số
cat("MAE (Sai số tuyệt đối trung bình): ", mae_value, "\n")
cat("MSE (Sai số bình phương trung bình): ", mse_value, "\n")

# Phân tích phương sai (ANOVA) với các biến phân loại
# Kiểm định xem Manufacturer, Resolution_WxH, Memory_Type có ảnh hưởng đến Pixel_Rate không
model_ANOVA <- aov(Pixel_Rate ~ Manufacturer + Resolution_WxH + Memory_Type, data = GPU_new_log)
summary(model_ANOVA)

# Kiểm định hậu nghiệm để xem nhóm nào khác biệt (nếu ANOVA có ý nghĩa)
TukeyHSD(model_ANOVA)
