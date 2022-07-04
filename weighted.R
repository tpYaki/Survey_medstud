install.packages('dbplyr')

load(file="Rsurveyresultdb.Rda")

#demongraphic
library(reshape2)
library(ggalluvial)
library(tidyverse)


db$Q1[which(db$Q1=='8yrs MD pre-med')]<-"8yrs MD"
db$Q1[which(db$Q1=='8yrs MD med basics')]<-"8yrs MD"
db$Q1[which(db$Q1=='8yrs MD intern')]<-"8yrs MD"
db$Q1[which(db$Q1=='nursing 1st')]<-"nursing"
db$Q1[which(db$Q1=='nursing 2nd')]<-"nursing"
db$Q1[which(db$Q1=='nursing 3rd')]<-"nursing"
db$Q1[which(db$Q1=='nursing 4th')]<-"nursing"
db$Q1[which(db$Q1=='MM')]<-"MM"

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
#dbknow3<-dbknow[c(5,6,7,8)]
#M=cor(dbknow3,method = "spearman")
#pheatmap(M,cluster_col=T,cluster_row=T,color = colorRampPalette(c("dodgerblue", "white", "tomato"))(100),cellwidth=20,cellheight=20,display_numbers = T) 
#dbknow4<-db[-c(1,2,9,10)]
#M=cor(dbknow4,method = "spearman")
#pheatmap(M,cluster_col=T,cluster_row=T,color = colorRampPalette(c("dodgerblue", "white", "tomato"))(100),cellwidth=5,cellheight=5,display_numbers = F,fontsize = 5,border=F) 

#library(psych)
#res<-corr.test(dbknow4, dbknow4, use = "pairwise",method="spearman",adjust="bonferroni", alpha=.05)
#annotation2<-read.csv("KAPgrouping.csv",header=F)
#colnames(annotation2)<-c("KAP_group","Question")
#row.names(annotation2)<-annotation2$Question
#annotation2$Question<-NULL
#table(annotation2$KAP_group)
#ann_colors2 = list(
#  KAP_group=c('belief'=brewer.pal(5,"Set1")[1],'demographic'=brewer.pal(5,"Set1")[2],'knowledge'=brewer.pal(5,"Set1")[3],'medicine'=brewer.pal(5,"Set1")[4],'practice'=brewer.pal(5,"Set1")[5])
#)
#pheatmap(res$r,cluster_rows = T,cluster_col=T,display_numbers = matrix(ifelse(res$p <= 0.01, "**", ifelse(res$p <= 0.05 ,"*"," ")), nrow(res$p)), color = colorRampPalette(c("dodgerblue", "white", "tomato"))(100),cellwidth=5,cellheight=5,fontsize = 5,border=F,annotation_colors = ann_colors2,annotation_col = annotation2,annotation_row=annotation2)
#write.csv(res$r,"correlation.csv")
#write.csv(res$p,"pvalue.csv")

#install.packages('wCorr')
library(wCorr)
library(psych)
dbknow4<-db[-c(2,9,10)]
dbknow4$Q50<-dbknow4$Q45+dbknow4$Q46
dbknow4$Q12<-(4-dbknow4$Q12)
dbknow4$Q13<-(4-dbknow4$Q13)
dbknow4$Q48<-(6-dbknow4$Q48)
dbknow4$Q47<-(6-dbknow4$Q47)
dbknow4$Q44<-(4-dbknow4$Q44)

dbknow4$Q1[which(db$Q1=='8yrs MD')]<-1.132686084
dbknow4$Q1[which(db$Q1=="nursing")]<-0.650334076
dbknow4$Q1[which(db$Q1=="MM")]<-1.483471074
weight=  as.numeric(unlist(dbknow4$Q1)) ##转化list为numeric
##for (i in range(27)){weight2[i] = weight}

spearmentt <- function(r,n){r*sqrt((n-2)/(1-r^2))} ##定义r到p的算法
res_r = data.frame()
res_test = data.frame()
dbknow4new<-dbknow4[,c("Q4","Q8","Q9","Q10","Q12","Q13","Q15","Q17","Q18","Q21","Q24","Q25","Q27","Q35","Q41","Q42","Q43","Q44","Q47","Q48","Q37","Q39","Q36","Q40","Q50","Q30","Q31")]
column27 = c("Q4","Q8","Q9","Q10","Q12","Q13","Q15","Q17","Q18","Q21","Q24","Q25","Q27","Q35","Q41","Q42","Q43","Q44","Q47","Q48","Q37","Q39","Q36","Q40","Q50","Q30","Q31")
for (k in column27){for (j in column27){
      print(j)
      #if (k!=j){
      res_temp = weightedCorr(x=as.numeric(unlist(dbknow4new[k])),y = as.numeric(unlist(dbknow4new[j])), w=weight,method = "spearman")
      res_r[k,j] = res_temp
      pt = pt(spearmentt(res_temp,472),470)
      res_test[k,j] =p.adjust((1-abs(pt))/2,method ='bonferroni',n = 702)
    #}
  }
}


##res2<-corr.test(dbknow4new, dbknow4new, use = "pairwise",method="spearman",adjust="bonferroni", alpha=.05)

annotation2<-read.csv("KAPgrouping2.csv",header=F)
colnames(annotation2)<-c("KAP_group","Question")
row.names(annotation2)<-annotation2$Question
annotation2$Question<-NULL
table(annotation2$KAP_group)
ann_colors2 = list(
  KAP_group=c('belief'=brewer.pal(5,"Set1")[1],'education'=brewer.pal(5,"Set1")[2],'knowledge'=brewer.pal(5,"Set1")[3],'medicine'=brewer.pal(5,"Set1")[4],'practice'=brewer.pal(5,"Set1")[5])
)
bk <- c(seq(-0.3,-0.01,by=0.001),seq(0,0.3,by=0.001))
#matrixdisplay = data.frame()
#for (m in column27){for (n in column27){if(res_test[m,n] <= 0.05){matrixdisplay[m,n]="*"}
#  {if(res_test[m,n] <= 0.05) {matrixdisplay[m,n]="*"}}}}
#matrixdisplay = as.matrix(matrixdisplay)
#pheatmap(res2$r,breaks=bk,cluster_rows = T,cluster_col=T,display_numbers = matrix(ifelse(res2$p <= 0.01, "**", ifelse(res2$p <= 0.05 ,"*"," ")), nrow(res2$p)), color = c(colorRampPalette(colors = c("dodgerblue","white"))(392),colorRampPalette(colors = c("white","tomato"))(1000)) ,cellwidth=13,cellheight=13,fontsize =13,border=F,annotation_colors = ann_colors2,annotation_col = annotation2,annotation_row=annotation2)
heatmap = pheatmap(res_r,breaks=bk,cluster_rows = T,cluster_col=T,display_numbers =matrix(ifelse(res_test <= 0.0001, "△", ifelse(res_test <= 0.001 ,"***",ifelse(res_test<= 0.01 ,"**",ifelse(res_test<= 0.05 ,"*"," ")))), nrow(res_test)), color = c(colorRampPalette(colors = c("dodgerblue","white"))(392),colorRampPalette(colors = c("white","tomato"))(200)) ,cellwidth=13,cellheight=13,fontsize =13,annotation_colors = ann_colors2,annotation_col = annotation2,annotation_row=annotation2)

save_pheatmap_png <- function(x, filename, width=1400, height=1200, res = 150) {
  png(filename, width = width, height = height, res = res)
  grid::grid.newpage()
  grid::grid.draw(x$gtable)
  dev.off()
}

save_pheatmap_png(heatmap,'weighted_result.png')