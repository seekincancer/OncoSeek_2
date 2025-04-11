library(pROC)
library(dplyr)
library(RColorBrewer)
library(ggplot2)

load(file="./data/sample_data.rds")

windowsFonts(A=windowsFont("Times New Roman"))
outfile_name = "fig2.jpg"
jpeg(filename=outfile_name, width=6, height=6, units="in",bg="white", res=300,family="A")

temp_col = brewer.pal(4, "Set1")
temp_roc_df = as.data.frame(roc_df)
roc0 = roc(as.factor(ifelse(temp_roc_df$Type=="Cancer","1","0")),
           as.numeric(as.vector(temp_roc_df$pvalue)))
auc_list = c(paste0("All cohorts: ",round(roc0$auc,3)))

temp_roc_df = as.data.frame(roc_df) %>% filter((Source == "HNCH"))
roc1 = roc(as.factor(ifelse(temp_roc_df$Type=="Cancer","1","0")),
           as.numeric(as.vector(temp_roc_df$pvalue)))
auc_list = c(auc_list,paste0("HNCH: ",round(roc1$auc,3)))

temp_roc_df = as.data.frame(roc_df) %>% filter((Source == "BZ"))
roc2 = roc(as.factor(ifelse(temp_roc_df$Type=="Cancer","1","0")),
           as.numeric(as.vector(temp_roc_df$pvalue)))
auc_list = c(auc_list, paste0("FSD: ",round(roc2$auc,3)))

temp_roc_df = as.data.frame(roc_df) %>% filter((Source == "BGI"))
roc3 = roc(as.factor(ifelse(temp_roc_df$Type=="Cancer","1","0")),
           as.numeric(as.vector(temp_roc_df$pvalue)))
auc_list = c(auc_list, paste0("BGI: ",sprintf("%0.3f", round(roc3$auc,3))))

temp_roc_df = as.data.frame(roc_df) %>% filter((Source == "PUSH"))
roc4 = roc(as.factor(ifelse(temp_roc_df$Type=="Cancer","1","0")),
           as.numeric(as.vector(temp_roc_df$pvalue)))
auc_list = c(auc_list, paste0("PUSH: ",sprintf("%0.3f", round(roc4$auc,3))))

ggroc(list(roc0=roc0,roc1=roc1,roc2=roc2,roc3=roc3,roc4=roc4),aes=c("size","color"))+
  scale_color_manual(values=c("black",temp_col[c(1,2,4,3)]),labels=auc_list)+
  scale_size_manual(values=c(2,1.5,1.5,1.5,1.5),labels=auc_list)+
  geom_vline(xintercept = 0.9, color="black", lty=2)+
  geom_abline(slope = 1, intercept = 1, color="black", lty=2)+
  xlab("Specificity")+
  ylab("Sensitivity")+
  scale_x_reverse(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  theme_classic()+
  theme(
    text=element_text(size=25, family = "serif"),
    legend.position = c(0.7,0.2),
    legend.title = element_text(size = 0,  face = "bold"),
    legend.text = element_text(size = 25,  face = "bold"),
    axis.title = element_text(size = 25,  face = "bold"),
    axis.text = element_text(size = 25,  face = "bold",colour="black"),
    axis.text.x = element_text(angle = 0, vjust=0.25))

dev.off()


