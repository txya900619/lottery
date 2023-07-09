library(ggplot2)
library(magrittr)

lottery_history <- read.csv("../lottery_history.csv")

hit <- 0 
total <- 0
start <- nrow(lottery_history) - 200
end <- nrow(lottery_history)
for(i in start:(end-12)){
  #i <- start
  
  data <- lottery_history[i:(i+10),]
  data <- data.frame(n1 = data$n1, n2 = data$n2, n3 = data$n3, n4 = data$n4,
                     n5 = data$n5, n6 = data$n6, sp = data$sp)
  target <- lottery_history[(i+11),]
  target <- data.frame(n1 = target$n1, n2 = target$n2, n3 = target$n3, 
                       n4 = target$n4, n5 = target$n5, n6 = target$n6,
                       sp = target$sp)
  
  target_nums <- c(target$n1, target$n2, target$n3, target$n4, target$n5, 
                   target$n6, target$sp)
  
  nums <- c(data$n1, data$n2, data$n3, data$n4, data$n5, data$n6, data$sp)
  
  
  factor_levels <- factor(1:49)
  counts <- table(nums)
  counts_df <- data.frame(number = factor_levels,
                          count = ifelse(factor_levels %in% names(counts), as.numeric(counts), 0))
  counts_df$isTarget <- ifelse(counts_df$number %in% target_nums, 1, 0) %>% as.factor()
  
  # bar_plot <- ggplot(counts_df, aes(x = number, y = count, fill = isTarget)) +
  #   scale_x_discrete(breaks = c(1:49)) +
  #   ylim(0, 6) +
  #   geom_bar(stat = "identity") +
  #   geom_hline(yintercept = 3, col = "red", size = 2) +
  #   theme_minimal() +
  #   scale_fill_manual(values = c("gray", "blue"))
  
  # # 繪製圖形
  #ggsave(paste0("./pic10/", i, ".jpg"), bar_plot, width = 16, height = 9, dpi = 300)
  
  total <- total + counts_df[counts_df$count > 3,] %>% nrow() + counts_df[counts_df$count == 0,] %>% nrow()
  hit <- hit + 7 - counts_df[counts_df$count > 3 & counts_df$isTarget == 1,] %>% nrow() - counts_df[counts_df$count == 0 & counts_df$isTarget == 1,] %>% nrow()
}

hit_rate <- hit / start:(end-12) %>% length()
miss_rate <- (total - hit) / start:(end-12) %>% length()
#count3_rate <- total / start:(end-12) %>% length()

cat("撇除過去累積超過 4 次以上、0 次，平均有 ", hit_rate, " 個號碼，在 1,2,3 次內 \n")
cat("若刪除超過 4 次以上、0 次可替除 ", miss_rate, " 個號碼\n")
#cat("平均每一期超過 3 次的累計號碼次數為  ", count3_rate, " \n")
# 結論： 3 個號碼只有 14% 的機會有中

# 命題，誰的機率大:
# 49 個號碼，我任意選 7 個號碼，可以猜中對方選的 7 個號碼中至少中 3 個 
# 42 個號碼，任意選 7 個號碼，可以猜中對方選的 5 個號碼中至少中 3 個 
# 結論，第一種機率比較大，為了刪除 7 個號碼而使 2 個中獎號碼刪除並不合適
