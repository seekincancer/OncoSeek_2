library(ggplot2)
library(cowplot)

load(file="./data/sample_data.rds")

## 
show_epi_tests_stage <- function(temp_df, stage_value, cutoff){
  tp = sum(temp_df$Stage==stage_value&temp_df$pvalue>=cutoff)
  fp = sum(temp_df$Type=="Healthy"&temp_df$pvalue>=cutoff)
  fn = sum(temp_df$Stage==stage_value&temp_df$pvalue<(cutoff))
  tn = sum(temp_df$Type=="Healthy"&temp_df$pvalue<(cutoff))
  return(c(epiR::epi.tests(c(tp,fp,fn,tn), digits = 3)$detail[3,2:4], sum(temp_df$Stage==stage_value)))
}
show_epi_tests_cancertype <- function(temp_df, cancertype_value, cutoff){
  tp = sum(temp_df$CancerType==cancertype_value&temp_df$pvalue>=cutoff)
  fp = sum(temp_df$Type=="Healthy"&temp_df$pvalue>=cutoff)
  fn = sum(temp_df$CancerType==cancertype_value&temp_df$pvalue<(cutoff))
  tn = sum(temp_df$Type=="Healthy"&temp_df$pvalue<(cutoff))
  # print(epiR::epi.tests(c(tp,fp,fn,tn), digits = 3))
  return(c(epiR::epi.tests(c(tp,fp,fn,tn), digits = 3)$detail[3,2:4], sum(temp_df$CancerType==cancertype_value)))
}

# get plot df
get_stage_and_cancertype_df <- function(cohort_df, stage_list, cancertype_list, cutoff){
  total_stage_df = data.frame(matrix(,nrow=0,ncol=5))
  colnames(total_stage_df) = c("Stage","est","lower","upper","total")
  row_count = 1
  for(i in stage_list){
    temp_stage_pvalue = show_epi_tests_stage(cohort_df, i, cutoff)
    total_stage_df[row_count,] = c(i, temp_stage_pvalue)
    row_count = row_count + 1
  }
  # Stage
  library(dplyr)
  total_stage_df$total_label = ""
  total_stage_df$percent = ""
  for(i in 1:nrow(total_stage_df)){
    total_stage_df[i,"total_label"] = paste0(total_stage_df[i,]$Stage," (") %>% paste0(total_stage_df[i,]$total) %>% paste0(")")
    total_stage_df[i,"percent"] = paste0(sprintf("%.1f",total_stage_df[i,]$est*100),"%")
  }
  total_stage_df$total_label = factor(total_stage_df$total_label, levels=total_stage_df$total_label)
  
  # CancerType
  total_cancertype_df = data.frame(matrix(,nrow=0,ncol=5))
  colnames(total_cancertype_df) = c("CancerType","est","lower","upper","total")
  row_count = 1
  for(i in cancertype_list){
    total_cancertype_pvalue = show_epi_tests_cancertype(cohort_df, i, cutoff)
    total_cancertype_df[row_count,] = c(i, total_cancertype_pvalue)
    row_count = row_count + 1
  }
  #
  library(dplyr)
  total_cancertype_df$total_label = ""
  total_cancertype_df$percent = ""
  for(i in 1:nrow(total_cancertype_df)){
    total_cancertype_df[i,"total_label"] = paste0(total_cancertype_df[i,]$CancerType," (") %>% paste0(total_cancertype_df[i,]$total) %>% paste0(")")
    total_cancertype_df[i,"percent"] = paste0(sprintf("%.1f",total_cancertype_df[i,]$est*100),"%")
  }
  total_cancertype_df = total_cancertype_df[order(total_cancertype_df$est,decreasing = T),]
  total_cancertype_df$total_label = factor(total_cancertype_df$total_label, levels=total_cancertype_df$total_label)
  
  return(list(total_stage_df, total_cancertype_df))
}


total_cohort_list = get_stage_and_cancertype_df(roc_df, c("I","II","III","IV","LS","ES"), 
                                                c("Bile Duct","Breast","Cervix","Colorectum","Endometrium","Gallbladder","Oesophagus",
                                                  "Head and Neck","Liver","Lung","Lymphoma","Ovary","Pancreas","Stomach"), 0.5)

# plot
# A
ggplot(data=total_cohort_list[[2]], aes(x=total_label, y=est))+
  geom_point(size=2.5, position=position_dodge(1))+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=position_dodge(1),linewidth=1)+
  geom_text(aes(y=upper+0.05,label=percent), size=5, family="serif", fontface="bold")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1.05))+
  xlab("")+
  ylab("Sensitivity")+
  theme_classic()+
  theme(
    plot.margin = margin(t = 30,
                         r = 5,
                         b = 5,
                         l = 5),
    text=element_text(size=20, family = "serif"),
    legend.text = element_text(size = 15,  face = "bold"),
    axis.title = element_text(size = 25,  face = "bold"),
    axis.text = element_text(size = 20,  face = "bold",colour="black"),
    axis.text.x = element_text(angle = 90, vjust=0.25))
#stage#
# B
ggplot(data=total_cohort_list[[1]][1:4,], aes(x=total_label, y=est))+
  geom_point(size=2.5, position=position_dodge(1))+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=position_dodge(1),linewidth=1)+
  geom_text(aes(y=upper+0.05,label=percent), size=5, family="serif", fontface="bold")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1))+
  xlab("")+
  ylab("Sensitivity")+
  theme_classic()+
  theme(
    plot.margin = margin(t = 30,
                         r = 5,
                         b = 5,
                         l = 5),
    text=element_text(size=20, family = "serif"),
    legend.text = element_text(size = 15,  face = "bold"),
    axis.title = element_text(size = 25,  face = "bold"),
    axis.text = element_text(size = 20,  face = "bold",colour="black"),
    axis.text.x = element_text(angle = 90, vjust=0.25))
# C
cancertype_group = c("Breast","Colorectum","Liver","Lung","Lymphoma","Oesophagus","Ovary","Pancreas","Stomach")
for(cancertype in cancertype_group){
  temp_df = roc_df[which(roc_df$CancerType==cancertype),]
  total_stage_df = data.frame(matrix(,nrow=0,ncol=5))
  colnames(total_stage_df) = c("Stage","est","lower","upper","total")
  row_count = 1
  for(i in c("I","II","III","IV")){
    temp_stage_pvalue = show_epi_tests_stage(temp_df, i, 0.5)
    total_stage_df[row_count,] = c(i, temp_stage_pvalue)
    row_count = row_count + 1
  }
  #
  library(dplyr)
  total_stage_df$total_label = ""
  total_stage_df$percent = ""
  for(i in 1:nrow(total_stage_df)){
    total_stage_df[i,"total_label"] = paste0(total_stage_df[i,]$Stage," (") %>% paste0(total_stage_df[i,]$total) %>% paste0(")")
    total_stage_df[i,"percent"] = paste0(sprintf("%.1f",total_stage_df[i,]$est*100),"%")
  }
  total_stage_df$CancerType = cancertype
  
  if(exists("each_cancertype_stage")==FALSE){
    each_cancertype_stage = total_stage_df
  }else{
    each_cancertype_stage = rbind(each_cancertype_stage, total_stage_df)
  }
}
for(j in 1:9){
  cancertype = cancertype_group[j]
  temp_plot_df = each_cancertype_stage[which(each_cancertype_stage$CancerType==cancertype),]
  for(i in 1:nrow(temp_plot_df)){
    temp_plot_df[i,"total_label"] = paste0(temp_plot_df[i,]$Stage,"\n(") %>% paste0(temp_plot_df[i,]$total) %>% paste0(")")
  }
  temp_plot_df$total_label = factor(temp_plot_df$total_label, levels=temp_plot_df$total_label)
  
  temp_plot = ggplot(data=temp_plot_df, aes(x=total_label, y=est))+
    geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=position_dodge(1),size=1)+
    geom_point(size=3.5, position=position_dodge(1))+
    geom_text(aes(y=upper*1.15,label=percent), size=3, family="serif",  fontface = "bold")+
    annotate("text",label=cancertype,family="serif",x=1.5,y=1.3,color="black",size=4.5,fontface="bold")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1.3),breaks=c(0,0.25,0.5,0.75,1))+
    xlab("")+
    ylab("Sensitivity")+
    theme_classic()+
    theme(
      plot.margin = margin(t = 5,
                           r = 5,
                           b = 5,
                           l = 5),
      # legend.position = "none",
      text=element_text(size=15, family = "serif",  face = "bold"),
      legend.text = element_text(size = 10,  face = "bold"),
      axis.title = element_text(size = 15,  face = "bold"),
      axis.text = element_text(size = 10,colour="black",  face = "bold"),
      axis.text.x = element_text(angle = 0, vjust=0.25,  face = "bold"))
  assign(paste0("plot_",j),temp_plot)
}
plot_grid(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6,
                   plot_7,plot_8,plot_9,nrow=3)
