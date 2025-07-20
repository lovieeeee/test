#加载包、读取数据
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(tidyverse)
getwd() 
setwd("/Users/humeiyu/Desktop/test")#文件夹地址
df<-read.csv(file="test7.csv")#文件名
head(df)
p2 <- ggplot(data = df,
  aes(x = Log2.FC, y = X.Log10.p.value))+
  geom_point(aes(color = Expression ))+
  scale_color_manual(values = c("Up" = "red",
                              "Down" = "blue",
                              "No" = "grey"))+
  geom_vline(xintercept = c(-1, 1), lty = 3, color = "black", lwd = 0.5)+
  geom_hline(yintercept = -log10(0.05), lty = 3, color = "black", lwd = 0.5)+
  labs(x = "Log2 FC", y = "-Log10 Pvalue" )
library(dplyr)
lab <- bind_rows(
  df %>%
    filter(Expression == "Up") %>%
    arrange(10**(-X.Log10.p.value), desc(abs(Log2.FC))) %>%
    head(20),
  df %>%
    filter(Expression == "Down") %>%
    arrange(10**(-X.Log10.p.value), desc(abs(Log2.FC))) %>%
    head(20)
)
print(lab)
p2 + 
  geom_label_repel(data = lab,
                      aes(Log2.FC, X.Log10.p.value, label = Gene),
                      max.overlaps = 40,
                      size = 2)
                      