# date：2020-10-08
# author：suke
# task：火山图尝试
# detial：用ggplot定义函数绘制火山图
# 参考文章：https://mp.weixin.qq.com/s/5v6nlTgvL6p2MpAO11tXJg> 

# 说明： 火山图是用来描述RNA差异表达分析的结果，例如miRNA（人 2600多个），mRNA（人2w）等
# 导入ggplot包
library(ggplot2)

# 自定义火山图函数
gdcVolcanoPlot2<-function (deg.all, fc = 2, pval = 0.01, dotsize=0.8) 
{
  geneList <- deg.all
  geneList$threshold <- c()
  geneList$threshold[geneList$logFC > log(fc, 2) & geneList$FDR < 
                       pval] <- 1
  geneList$threshold[geneList$logFC >= -log(fc, 2) & geneList$logFC <= 
                       log(fc, 2) | geneList$FDR >= pval] <- 2
  geneList$threshold[geneList$logFC < -log(fc, 2) & geneList$FDR < 
                       pval] <- 3
  geneList$threshold <- as.factor(geneList$threshold)
  lim <- max(max(geneList$logFC), abs(min(geneList$logFC))) + 
    0.5
  volcano <- ggplot(data = geneList, aes(x = logFC, 
                                         y = -log10(FDR)))
  volcano + geom_point(aes(color = threshold), alpha = 1, 
                       size = dotsize) + xlab("log2(Fold Change)") + ylab("-log10(FDR)") + 
    scale_colour_manual(values = c("red", "black", "green3")) + xlim(c(-lim, lim)) + 
    geom_vline(xintercept = c(-log(fc, 2), log(fc, 2)), color = "darkgreen", 
               linetype = 3) + geom_hline(yintercept = -log(pval, 
                                                            10), color = "darkgreen", linetype = 3) + theme_bw() + 
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.border = element_rect(colour = "black"), 
          panel.background = element_blank()) + theme(legend.position = "none") + 
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16))
}

# 缺少数据


load("DEGAll.rda")

volcano + geom_point(aes(color = threshold), alpha = 1, 
                     size = 0.8) + xlab("log2(Fold Change)") + ylab("-log10(FDR)")

#不指定dotsize，就用默认值0.8来绘图
gdcVolcanoPlot2(DEGMIR)
#指定了dotsize，就用指定值2来绘图
gdcVolcanoPlot2(DEGMIR,dotsize=2)
