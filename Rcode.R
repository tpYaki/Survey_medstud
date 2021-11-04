load(file="Rsurveyresultdb.Rda")

#demongraphic
library(reshape2)
library(ggalluvial)
library(tidyverse)

db$Q1[which(db$Q1==1)]<-"8yrs MD pre-med"
db$Q1[which(db$Q1==2)]<-"8yrs MD med basics"
db$Q1[which(db$Q1==3)]<-"8yrs MD intern"
db$Q1[which(db$Q1==4)]<-"nursing 1st"
db$Q1[which(db$Q1==5)]<-"nursing 2nd"
db$Q1[which(db$Q1==6)]<-"nursing 3rd"
db$Q1[which(db$Q1==7)]<-"nursing 4th"
db$Q1[which(db$Q1==8)]<-"MM"

db$Q3[which(db$Q3==1)]<-"male"
db$Q3[which(db$Q3==2)]<-"female"
db$Q3[which(db$Q3==3)]<-"others"
table(db$Q3)


db$Q6[which(db$Q6==1)]<-"1st tier cities"
db$Q6[which(db$Q6==2)]<-"2nd tier cities"
db$Q6[which(db$Q6==3)]<-"3rd tier cities"
db$Q6[which(db$Q6==4)]<-"villages & towns"
db$Q6[which(db$Q6==5)]<-"rural area"



ggplot(data = db,aes(axis1=Q5,axis2 = Q3, axis3 = Q1,axis4 =Q6))  +
  geom_alluvium(aes(fill=Q6)) +
  scale_x_discrete(limits = c("Age","Biological Sex","Education","Hometown"), expand = c(.1, .1)) +
  geom_stratum(alpha=0.6) +
  theme_minimal()

#knowledge scoring
dbknow<-db[c(1,2,9,10,114,115,116,117)]

library(ggplot2)
library(ggfortify)
library(devtools)
dbknow$edu<-dbknow$Q1
dbknow$edu<-gsub("nursing 1st","nursing",dbknow$edu)
dbknow$edu<-gsub("nursing 2nd","nursing",dbknow$edu)
dbknow$edu<-gsub("nursing 3rd","nursing",dbknow$edu)
dbknow$edu<-gsub("nursing 4th","nursing",dbknow$edu)
dbknow$edu<-gsub("8yrs MD intern","8yrs MD",dbknow$edu)
dbknow$edu<-gsub("8yrs MD med basics","8yrs MD",dbknow$edu)
dbknow$edu<-gsub("8yrs MD pre-med","8yrs MD",dbknow$edu)


autoplot(prcomp( dbknow[,5:(ncol(dbknow)-1)]), data=dbknow,colour = 'edu',label=FALSE,label.size=2,frame=TRUE,frame.type="norm")+theme_bw()

library(stringr)
library(pheatmap)
library(RColorBrewer)
colnames(db)
dbknow<-db[c(1,2,9,10,114,115,116,117,48,57,58,65)]
dbknow2<-as.data.frame(t(dbknow[c(5,6,7,8,9,10,12)]))
M=cor(dbknow2,method="spearman")
annotation =dbknow[c(2,1,4)]
ann_colors = list(
  Q3 = c(female = "tomato", male = "dodgerblue",others="lightgrey"),
  Q1=c('8yrs MD pre-med'=brewer.pal(8,"Oranges")[2],'8yrs MD med basics'=brewer.pal(8,"Oranges")[4],'8yrs MD intern'=brewer.pal(8,"Oranges")[6],'MM'="firebrick",'nursing 1st'=brewer.pal(8,"Blues")[2],'nursing 2nd'=brewer.pal(8,"Blues")[4],'nursing 3rd'=brewer.pal(8,"Blues")[6],'nursing 4th'=brewer.pal(8,"Blues")[8]),
  Q6=c('1st tier cities'=brewer.pal(5,"Set1")[1],'2nd tier cities'=brewer.pal(5,"Set1")[2],'3rd tier cities'=brewer.pal(5,"Set1")[3],'rural area'=brewer.pal(5,"Set1")[4],'villages & towns'=brewer.pal(5,"Set1")[5])
  )
pheatmap(M,cluster_col=T,show_colnames=F,show_rownames=F,annotation_colors = ann_colors,cluster_row=T,color = colorRampPalette(c("dodgerblue", "white","tomato"))(100),cellwidth=1,cellheight=1,display_numbers = F,annotation_col = annotation,annotation_row=annotation) 
pheatmap(dbknow2,cluster_col=T,scale="row",show_colnames=F,show_rownames=T,annotation_colors = ann_colors,cluster_row=T,color = colorRampPalette(c("dodgerblue", "white","tomato"))(100),cellwidth=1,cellheight=10,display_numbers = F,annotation_col = annotation) 


colnames(db)
colnames(db)
dbedu<-db[c(1,2,9,10,59,60,61,62,63,64,82,83,84,85,86,87,88)]
dbedu2<-as.data.frame(t(dbedu[c(5,6,7,8,9,10,11,12,13,14,15,16,17)]))
annotation =dbedu[c(2,1,4)]
ann_colors = list(
  Q3 = c(female = "tomato", male = "dodgerblue",others="lightgrey"),
  Q1=c('8yrs MD pre-med'=brewer.pal(8,"Oranges")[2],'8yrs MD med basics'=brewer.pal(8,"Oranges")[4],'8yrs MD intern'=brewer.pal(8,"Oranges")[6],'MM'="firebrick",'nursing 1st'=brewer.pal(8,"Blues")[2],'nursing 2nd'=brewer.pal(8,"Blues")[4],'nursing 3rd'=brewer.pal(8,"Blues")[6],'nursing 4th'=brewer.pal(8,"Blues")[8]),
  Q6=c('1st tier cities'=brewer.pal(5,"Set1")[1],'2nd tier cities'=brewer.pal(5,"Set1")[2],'3rd tier cities'=brewer.pal(5,"Set1")[3],'rural area'=brewer.pal(5,"Set1")[4],'villages & towns'=brewer.pal(5,"Set1")[5])
)
pheatmap(dbedu2,cluster_col=T,scale="none",show_colnames=F,show_rownames=T,annotation_colors = ann_colors,cluster_row=F,color = colorRampPalette(c("tan", "white","black"))(100),cellwidth=1,cellheight=10,display_numbers = F,annotation_col = annotation) 



#Correlation
dbknow3<-dbknow[c(5,6,7,8)]
M=cor(dbknow3,method = "spearman")
pheatmap(M,cluster_col=T,cluster_row=T,color = colorRampPalette(c("dodgerblue", "white", "tomato"))(100),cellwidth=20,cellheight=20,display_numbers = T) 
dbknow4<-db[-c(1,2,9,10)]
M=cor(dbknow4,method = "spearman")
pheatmap(M,cluster_col=T,cluster_row=T,color = colorRampPalette(c("dodgerblue", "white", "tomato"))(100),cellwidth=5,cellheight=5,display_numbers = F,fontsize = 5,border=F) 

library(psych)
res<-corr.test(dbknow4, dbknow4, use = "pairwise",method="spearman",adjust="bonferroni", alpha=.05)
annotation2<-read.csv("KAPgrouping.csv",header=F)
colnames(annotation2)<-c("KAP_group","Question")
row.names(annotation2)<-annotation2$Question
annotation2$Question<-NULL
table(annotation2$KAP_group)
ann_colors2 = list(
  KAP_group=c('belief'=brewer.pal(5,"Set1")[1],'demographic'=brewer.pal(5,"Set1")[2],'knowledge'=brewer.pal(5,"Set1")[3],'medicine'=brewer.pal(5,"Set1")[4],'practice'=brewer.pal(5,"Set1")[5])
)
pheatmap(res$r,cluster_rows = T,cluster_col=T,display_numbers = matrix(ifelse(res$p <= 0.01, "**", ifelse(res$p <= 0.05 ,"*"," ")), nrow(res$p)), color = colorRampPalette(c("dodgerblue", "white", "tomato"))(100),cellwidth=5,cellheight=5,fontsize = 5,border=F,annotation_colors = ann_colors2,annotation_col = annotation2,annotation_row=annotation2)
write.csv(res$r,"correlation.csv")
write.csv(res$p,"pvalue.csv")


library(psych)
dbknow4<-db[-c(1,2,9,10)]
dbknow4$Q50<-dbknow4$Q45+dbknow4$Q46
dbknow4$Q12<-(4-dbknow4$Q12)
dbknow4$Q13<-(4-dbknow4$Q13)
dbknow4$Q48<-(6-dbknow4$Q48)
dbknow4$Q47<-(6-dbknow4$Q47)
dbknow4$Q44<-(4-dbknow4$Q44)
dbknow4new<-dbknow4[,c("Q4","Q8","Q9","Q10","Q12","Q13","Q15","Q17","Q18","Q21","Q24","Q25","Q27","Q35","Q41","Q42","Q43","Q44","Q47","Q48","Q37","Q39","Q36","Q40","Q50","Q30","Q31")]
res2<-corr.test(dbknow4new, dbknow4new, use = "pairwise",method="spearman",adjust="bonferroni", alpha=.05)
annotation2<-read.csv("KAPgrouping2.csv",header=F)
colnames(annotation2)<-c("KAP_group","Question")
row.names(annotation2)<-annotation2$Question
annotation2$Question<-NULL
table(annotation2$KAP_group)
ann_colors2 = list(
  KAP_group=c('belief'=brewer.pal(5,"Set1")[1],'education'=brewer.pal(5,"Set1")[2],'knowledge'=brewer.pal(5,"Set1")[3],'medicine'=brewer.pal(5,"Set1")[4],'practice'=brewer.pal(5,"Set1")[5])
)
bk <- c(seq(-0.4,-0.01,by=0.001),seq(0,1,by=0.001))

pheatmap(res2$r,breaks=bk,cluster_rows = T,cluster_col=T,display_numbers = matrix(ifelse(res2$p <= 0.01, "**", ifelse(res2$p <= 0.05 ,"*"," ")), nrow(res2$p)), color = c(colorRampPalette(colors = c("dodgerblue","white"))(392),colorRampPalette(colors = c("white","tomato"))(1000)) ,cellwidth=13,cellheight=13,fontsize =13,border=F,annotation_colors = ann_colors2,annotation_col = annotation2,annotation_row=annotation2)


#Statistical testing
library(ggpubr)
library(RColorBrewer)
df0<-db[,-c(2,9,10)]
comp<-unique(df0$Q1)
my_comparisons <- list(c(comp[1],comp[2]),c(comp[1],comp[3]),c(comp[1],comp[4]),c(comp[1],comp[5]),c(comp[1],comp[6]),c(comp[1],comp[7]),c(comp[1],comp[8]),c(comp[2],comp[3]),c(comp[2],comp[4]),c(comp[2],comp[5]),c(comp[2],comp[6]),c(comp[2],comp[7]),c(comp[2],comp[8]),c(comp[3],comp[4]),c(comp[3],comp[5]),c(comp[3],comp[6]),c(comp[3],comp[7]),c(comp[3],comp[8]),c(comp[4],comp[5]),c(comp[4],comp[6]),c(comp[4],comp[7]),c(comp[4],comp[8]),c(comp[5],comp[6]),c(comp[5],comp[7]),c(comp[5],comp[8]),c(comp[6],comp[7]),c(comp[6],comp[8]),c(comp[7],comp[8]))
for(i in 2:ncol(df0)){
  df1<-df0[,c(1,i)]
  name<-colnames(df1)[2]
  p<-ggviolin(df1, x="Q1", y=name, fill = "Q1", 
           palette = c(brewer.pal(8,"Set3")[1],brewer.pal(8,"Set3")[2],brewer.pal(8,"Set3")[3],brewer.pal(8,"Set3")[4],brewer.pal(8,"Set3")[5],brewer.pal(8,"Set3")[6],brewer.pal(8,"Set3")[7],brewer.pal(8,"Set3")[1],brewer.pal(8,"Set3")[8]))+
    stat_compare_means(comparisons = my_comparisons, label = "p.signif")+#label这里表示选择显著性标记（星号） 
    stat_compare_means(label.y = 50)
  p
  myfilename <- paste("Q1_",name,"_violin.pdf",sep="")
  ggsave(filename = myfilename,width = 10, height = 10)
}

df0<-db[,-c(1,9,10)]
comp<-unique(df0$Q3)
my_comparisons <- list(c(comp[1],comp[2]),c(comp[1],comp[3]),c(comp[2],comp[3]))
for(i in 2:ncol(df0)){
  df1<-df0[,c(1,i)]
  name<-colnames(df1)[2]
  p<-ggviolin(df1, x="Q3", y=name, fill = "Q3",
              palette = c("tomato","dodgerblue","lightgrey"))+
    stat_compare_means(comparisons = my_comparisons, label = "p.signif")+#label这里表示选择显著性标记（星号） 
    stat_compare_means(label.y = 50)
  p
  myfilename <- paste("Q3_",name,"_violin.pdf",sep="")
  ggsave(filename = myfilename,width = 10, height = 10)
}

df0<-db[,-c(1,2,9)]
comp<-unique(df0$Q6)
df0q6<-as.data.frame(df0$Q6)
colnames(df0q6)<-"Q6"
df0$Q6<-NULL
df0<-cbind(df0q6,df0)
my_comparisons <- list(c(comp[1],comp[2]),c(comp[1],comp[3]),c(comp[1],comp[4]),c(comp[1],comp[5]),c(comp[2],comp[3]),c(comp[2],comp[4]),c(comp[2],comp[5]),c(comp[3],comp[4]),c(comp[3],comp[5]),c(comp[4],comp[5]))
for(i in 2:ncol(df0)){
  df1<-df0[,c(1,i)]
  name<-colnames(df1)[2]
  p<-ggviolin(df1, x="Q6", y=name, fill = "Q6",
              palette = c(brewer.pal(5,"Set1")[1],brewer.pal(5,"Set1")[2],brewer.pal(5,"Set1")[3],brewer.pal(5,"Set1")[4],brewer.pal(5,"Set1")[5]))+
    stat_compare_means(comparisons = my_comparisons, label = "p.signif")+#label这里表示选择显著性标记（星号） 
    stat_compare_means(label.y = 50)
  p
  myfilename <- paste("Q6_",name,"_violin.pdf",sep="")
  ggsave(filename = myfilename,width = 10, height = 10)
}

df0<-db[,-c(2,9,10)]
df0$Q1<-gsub("nursing 1st","nursing",df0$Q1)
df0$Q1<-gsub("nursing 2nd","nursing",df0$Q1)
df0$Q1<-gsub("nursing 3rd","nursing",df0$Q1)
df0$Q1<-gsub("nursing 4th","nursing",df0$Q1)
df0$Q1<-gsub("8yrs MD intern","8yrs MD",df0$Q1)
df0$Q1<-gsub("8yrs MD med basics","8yrs MD",df0$Q1)
df0$Q1<-gsub("8yrs MD pre-med","8yrs MD",df0$Q1)
comp<-unique(df0$Q1)
my_comparisons <- list(c(comp[1],comp[2]),c(comp[1],comp[3]),c(comp[2],comp[3]))
for(i in 2:ncol(df0)){
  df1<-df0[,c(1,i)]
  name<-colnames(df1)[2]
  p<-ggviolin(df1, x="Q1", y=name, fill = "Q1", 
              palette = c(brewer.pal(3,"Set2")[1],brewer.pal(3,"Set2")[2],brewer.pal(3,"Set2")[3]))+
    stat_compare_means(comparisons = my_comparisons, label = "p.signif")+#label这里表示选择显著性标记（星号） 
    stat_compare_means(label.y = 50)
  p
  myfilename <- paste("Q1_",name,"_violin.pdf",sep="")
  ggsave(filename = myfilename,width = 10, height = 10)
}

