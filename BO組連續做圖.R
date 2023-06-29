###########################
#
#         計算PNL然後畫圖
#         2023/5/3 更新
#
#         1) BO查詢的時候一次過查全部帳號
#         2) 下載的excel要轉成csv utf-8
#         3) rawdata的綠色字改成要輸入的檔案
#
############################
rm(list=ls())

# 选择文件并返回文件路径
file_path <- file.choose()

# 检查是否选择了文件
if (!is.null(file_path)) {
  # 打开文件
  file <- file(file_path, "r")
  # 读取文件内容
  file_contents <- readLines(file)
  # 输出文件内容
  cat(file_contents, sep = "\n")
  # 关闭文件
  close(file)
} else {
  # 用户取消了文件选择
  cat("未选择文件。\n")
}

library(readxl)
data1 <- readxl::read_excel(file_path)

columnnames <- c("ID", "CFD", "DEAL PRICE", "DEAL LOT", "DEAL TIME", "TAKE PROFIT", "STOP LOSS", "COMMISION", "INTEREST", "PROFIT", "OPEN PRICE",
                 "OPEN VOLUME", "OPEN TIME")

rawdata <- data1
n <- nrow(rawdata)-1 ##刪掉最後一列的total
data <- rawdata[2:n,]


##抓重要欄位
data1 <- as.data.frame(data[,c(2,5,17,18,21,23,24,27,28,29,34,35,36)]) #BO報表更新的話欄位可能會變動
'上面數字對應的BO欄位名稱:
2:"統一帳號master a/c"
5:"產品合約symbol"
17:"成交價格deal price"
18:"成交手數deal lot"	
21:"成交時間deal time"
23:"止盈take profit"	
24:"止損stop loss"	
27:"傭金commission"	
28:"利息swaps"	
29:"盈虧p/l"	
34:"開倉價格open price"	
35:"開倉手數open volume"	
36"開倉時間open time"
'
colnames(data1) <- columnnames


## count sum
profit <- NULL
for (i in 1:nrow(data1)) {
  tem <- as.numeric(data1[i,8])+as.numeric(data1[i,9])+as.numeric(data1[i,10])
  profit <- rbind(profit, tem)
}

SUM <- profit
rm(profit)

data2 <- cbind(data1[,c(1:10)],SUM,data1[,c(11:13)])
data2 <- data2[nrow(data2):1,]
ids <- data2[,1]
ids <- sort(ids[!duplicated(ids)])


data_list <- split(data2, f = data2$ID)
setwd("Y:\\RMT\\ProfitImageCreator\\Image")


for (i in 1:length(data_list)) {
  tem <- data_list[[i]]
  #save the plot
  last_profit <- cumsum(tem$SUM)
  last_profit <- last_profit[length(last_profit)]
  png(filename = paste0(ids[i], ".jpg"),width = 2400,height = 1800,res = 200)
  plot(1:nrow(tem), cumsum(tem$SUM), type = "l", xlab = paste(ids[i],"三個月交易次數"), ylab = "盈利", lwd = 4)
  abline(h=0, col = "grey", lty =1)
  legend("topright", legend = paste("盈利:", last_profit))
  dev.off()
}

##############################################

