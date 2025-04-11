library(ggpubr)
load(file="./data/repeatability_data.rds")

windowsFonts(A=windowsFont("Times New Roman"))
aaa = cor.test(lab_df2$SeekIn, lab_df2$SY, method="pearson")
aaa = sprintf("%0.2f", round(aaa$estimate,2))
outfile_name = "fig1_A.jpg"
jpeg(filename=outfile_name, width=6, height=6, units="in",bg="white", res=300,family="A")
ggscatter(data=lab_df2, x="SeekIn", y="SY",color="PTMs",group="SampleName")+
  geom_abline(slope = 1,intercept = 0,color="black",lty=2)+
  annotate("text",label=paste0("Pearson: ",aaa),family="serif",x=5,y=30,color="black",size=7)+
  xlim(0,30)+
  ylim(0,30)+
  xlab("SeekIn")+
  ylab("Shenyou")+
  theme_classic()+
  theme(
    text=element_text(size=35, family = "serif"),
    # legend.position = "none",
    legend.position = c(0.8,0.3),
    legend.title = element_blank(),
    legend.text = element_text(size = 15,  face = "bold"),
    axis.title = element_text(size = 25,  face = "bold"),
    axis.text = element_text(size = 20,  face = "bold",colour="black"),
    axis.text.x = element_text(angle = 0, vjust=0.25))
dev.off()

# B
aaa = cor.test(lab_df$SeekIn, lab_df$SYX, method="pearson")
aaa = sprintf("%0.2f", round(aaa$estimate,2))
outfile_name = "fig1_B.jpg"
jpeg(filename=outfile_name, width=6, height=6, units="in",bg="white", res=300,family="A")
ggscatter(data=lab_df, x="SeekIn", y="SYX",color="PTMs",group="SampleName")+
  geom_abline(slope = 1,intercept = 0,color="black",lty=2)+
  annotate("text",label=paste0("Pearson: ",aaa),family="serif",x=100,y=400,color="black",size=7)+
  xlim(0,450)+
  ylim(0,450)+
  xlab("SeekIn")+
  ylab("SYSMH")+
  theme_classic()+
  theme(
    text=element_text(size=35, family = "serif"),
    # legend.position = "none",
    legend.title = element_blank(),
    legend.position = c(0.8,0.3),
    legend.text = element_text(size = 15,  face = "bold"),
    axis.title = element_text(size = 25,  face = "bold"),
    axis.text = element_text(size = 20,  face = "bold",colour="black"),
    axis.text.x = element_text(angle = 0, vjust=0.25))
dev.off()