library(ggplot2)
library(tidyr)
library(dplyr)

######################
#redcap2<-read.csv("../OI/REDCAP/_DATA_2021-03-29_0247.csv",encoding="UTF-8",sep="\t")
#redcap2<-readr::read_tsv("../OI/REDCAP/_DATA_2021-04-20_0851.csv")
redcap2<-readr::read_tsv("../OI/REDCAP/_DATA_2021-04-21_0030.csv")
redcap2<-read.csv("../OI/REDCAP/_DATA_2021-04-21_0030.csv",encoding="UTF-8",sep="\t")
colnames(redcap2)[1]<-"record_id"


colnames(redcap2)[1]<-gsub(".*\\.","",colnames(redcap2)[1])

CODECONV<-read.csv("../OI/code-conversion.txt",encoding="UTF-8",header=F,sep="\t")
	rownames(CODECONV)<-CODECONV[,1]
setdiff(redcap2[["gene_test_time_result"]],CODECONV[,1])
redcap2[["newgene"]]<-CODECONV[redcap2[["gene_test_time_result"]],2]
###########################
patientName<-as.matrix(redcap2[,grep("redcap_event_name|email|pa_na|^name$",colnames(redcap2))])
LName<-sapply(unique(redcap2$record_id),function(id){
	setdiff(unique(as.vector(patientName[redcap2$record_id==id,-1])),NA)
})
names(LName)<-unique(redcap2$record_id)
cbind(redcap2$record_id,patientName)[redcap2$record_id%in%names(LName[sapply(LName,length)>2]),]
###########################
source("../OI/step1.help.funcs.R")
REDCAP<-GETDAT(as.data.frame(redcap2),colnames(as.data.frame(redcap2))[colSums(!is.na(as.data.frame(redcap2)))>0])
rownames(REDCAP)<-REDCAP$ids
PROMIS<-GETDAT(redcap2[redcap2[,2]%in%names(LNum[1:4]),],colnames(redcap2)[colSums(!is.na(redcap2))>0])
REDCAP_prom<-REDCAP[match(PROMIS$record_id,REDCAP$record_id),]

PATNAMES<-as.matrix(PROMIS[,grep("pa_na|^name$",colnames(PROMIS))])
UNQname<-apply(PATNAMES,1,function(x)paste0(unique(gsub(" ","",sort(x[!is.na(x)]))),collapse=";"))

sapply(colnames(PATNAMES),function(x){
	sapply(sapply(LNum,names),function(y)x%in%y)
})
###########################


PROMISids<-redcap2$record_id[grep(".*_promis_arm_1",redcap2$redcap_event_name)]
REDCAP_promis<-REDCAP[REDCAP$ids%in%PROMISids,]

as.matrix(REDCAP_promis[is.na(REDCAP_promis$gender),grep("name|record_id",colnames(REDCAP_promis))])
REDCAP_promis$gender[match(c(445,461,463,471),REDCAP_promis$ids)]<-c(2,2,1,1)

#save(REDCAP_promis,file="REDCAP_promis.RData")
as.matrix(REDCAP_promis[is.na(REDCAP_promis$hospital_id),grep("record_id|name",colnames(REDCAP))])
###########################
promis1<-redcap2[redcap2$redcap_event_name=="1st_promis_arm_1",]
promis2<-redcap2[redcap2$redcap_event_name=="2nd_promis_arm_1",]
promis3<-redcap2[redcap2$redcap_event_name=="3rd_promis_arm_1",]
promis4<-redcap2[redcap2$redcap_event_name=="4th_promis_arm_1",]

age<-redcap2$age[redcap2$record_id%in%promis1$record_id&redcap2$redcap_event_name=="nurse_record_basel_arm_1"]
gender<-redcap2$gender[redcap2$record_id%in%promis1$record_id&redcap2$redcap_event_name=="nurse_record_basel_arm_1"]

summary(age, na.rm=T)

table(age<18)
###########################
REDCAP2022<-readr::read_delim("../REDCAP/data/_DATA_2022-01-19_0759.csv", delim="^")
REDCAP2022<-readr::read_delim("../REDCAP/data/_DATA_2022-03-15_0313.csv", delim="^")

names2022<-REDCAP2022[,grepl("record_id|pa_name", colnames(REDCAP2022))]

as.matrix(redcap2[which(redcap2$record_id=="461"), grep("pa_n", colnames(redcap2))])

promis1$record_id[promis1$record_id==461]<-103

LName2022<-sapply(promis1$record_id, function(ID){
	setdiff(as.vector(as.matrix(names2022[which(names2022$record_id%in%ID),-1])), NA)
})
table(sapply(LName2022, length))

promis1$record_id[sapply(LName2022, length)==0]


unique(unlist(LName2022))


SEQ<-readxl::read_excel("O:/B002_OI_Seq_reports/OI-seq-summary-mar16-2022.xlsx", sheet = "SZH-OI")

promisName90<-sapply(LName2022, function(x)x[1])
promisName90[is.na(promisName90)]<-"n.a."
table(promisName90%in% SEQ$ChiNameOnReport)
setdiff(promisName90,  SEQ$ChiNameOnReport)

SEQ90<-SEQ[match(promisName90,as.matrix(SEQ)[,2]),]
SEQ69<-SEQ[which(as.matrix(SEQ)[,2]%in%promisName90),]

#xlsx::write.xlsx(SEQ69, "genetics/SEQ72-Mar-16-2022.xlsx", sheetName = "SEQ62", 
#  col.names = TRUE, row.names = TRUE, append = TRUE)


SEQ188<-readxl::read_excel("O:/B002_OI_Seq_reports/Sillence_subtyping/Summary.of.Sillence.and.Geno.Dec15.2021.xlsx", sheet = "188 patients")
table(promisName90%in%SEQ188$ChiNameOnReport)

intersect(promisName90, setdiff(as.matrix(SEQ)[,2],SEQ188$ChiNameOnReport))

GENES<-gsub("COL1A1, COL1A2, BMP1","BMP1",SEQ69$AffectedGenes)
tab72<-sort(table(GENES))
tabGene72<-as.matrix(tab72)
round(as.integer(tabGene)/sum(tabGene)*100, 1)
####
MatName<-cbind(ID=unique(REDCAP2022$record_id),
	Chi=sapply(unique(REDCAP2022$record_id), function(ID){
	setdiff(as.vector(as.matrix(names2022[which(names2022$record_id%in%ID),-1])), NA)
}))

OISEQ<-readxl::read_excel("O:/B002_OI_Seq_reports/OI-seq-summary-mar15-2022.xlsx", sheet="SZH-OI")[189:239,]
OISEQ$RedCap_ID[is.na( OISEQ$RedCap_ID)]<-"n.a."
REDCAPextramural<-REDCAP2022[match(OISEQ$RedCap_ID,REDCAP2022$record_id),]
as.matrix(REDCAPextramural[,c("record_id","pa_name1","hospital_id","dob")])

Sillence<-readxl::read_excel("O:/B002_OI_Seq_reports/need.sillence-2022-feb10-11.xlsx", sheet = "Sheet1")
as.matrix(Sillence[match(OISEQ$ChiNameOnReport,Sillence$proband),1:2])

Sillence90<-Sillence[match(promisName90,Sillence$proband),1:2]
Sillence90[["id"]]<-promis1$record_id
Sillence90[["promisName90"]]<-promisName90

setdiff(names(PROMGROUP), Sillence90$id)
setdiff(Sillence90$id, names(PROMGROUP))

Sillence70<-Sillence90[match(names(PROMGROUP), Sillence90$id), ]

SEQ70<-SEQ[match(names(PROMGROUP),SEQ$RedCap_ID),]
SEQ70[["PROMGROUP"]]<-PROMGROUP
table(is.na(Sillence70$SillenceType), is.na(SEQ70$SillenceType))
table(Sillence70$SillenceType, SEQ70$SillenceType)

indna<-which(is.na(Sillence70$SillenceType)&!is.na(SEQ70$SillenceType))
Sillence70$SillenceType[indna]<-SEQ70$SillenceType[indna]
SEQ70[["Sillence"]]<-Sillence70$SillenceType
SEQ70$AffectedGenes[6]<-"BMP1"
SEQ63<-SEQ70[SEQ70$PROMGROUP!="ambiguous",]

tabGeneClust<-table(SEQ63$AffectedGenes, SEQ63$PROMGROUP)
tabSillenceClust<-table(SEQ63$Sillence, SEQ63$PROMGROUP)
tabInheritanceClust<-table(SEQ63$Inheritance, SEQ63$PROMGROUP)

SEQ63[["missense"]]<-grepl("missense",SEQ63$Mutation_effect)

SEQ29<-SEQ63[grepl("COL1",SEQ63$AffectedGenes),]
table(SEQ29$missense, SEQ29$PROMGROUP)

chisq.test(tabInheritanceClust[-1,])
###########################

indDoc<-seq(which(colnames(REDCAP)=="doc_name"),which(colnames(REDCAP)=="ddcd_263414_complete"))

indChild<-seq(which(colnames(promis1)=="name"),which(colnames(promis1)=="promis_818_complete"))
indParent<-seq(which(colnames(promis1)=="pa_name3"),which(colnames(promis1)=="promis_518_complete"))
indAdult<-seq(which(colnames(promis1)=="pa_name4"),which(colnames(promis1)=="promis_19_complete"))

table(rowSums(!is.na(promis1[,indChild])),
	rowSums(!is.na(promis1[,indParent])))

table(rowSums(!is.na(promis1[,indAdult])),
	rowSums(!is.na(promis1[,indParent])))

barplot(colSums(!is.na(promis1[,indChild])))
barplot(colSums(!is.na(promis1[,indParent])))
barplot(colSums(!is.na(promis1[,indAdult])))

indChild1<-which(rowSums(!is.na(promis1[,indChild]))>60)
indChild2<-which(rowSums(!is.na(promis2[,indChild]))>60)
indChild3<-which(rowSums(!is.na(promis3[,indChild]))>60)
indChild4<-which(rowSums(!is.na(promis4[,indChild]))>60)

Child1<-as.matrix(promis1[indChild1,indChild][,-c(1:4,62)])
Child2<-as.matrix(promis2[indChild2,indChild][,-c(1:4,62)])
Child3<-as.matrix(promis3[indChild3,indChild][,-c(1:4,62)])
Child4<-as.matrix(promis4[indChild4,indChild][,-c(1:4,62)])

barplot(colSums(!is.na(Child1)))

sum(is.na(Child1))
sum(is.na(Child2))
sum(is.na(Child3))
sum(is.na(Child4))


indRC.Child1<-match(promis1$record_id[indChild1],REDCAP$ids)
indRC.Child2<-match(promis2$record_id[indChild2],REDCAP$ids)
indRC.Child3<-match(promis3$record_id[indChild3],REDCAP$ids)
indRC.Child4<-match(promis4$record_id[indChild4],REDCAP$ids)

indRC.Child1234<-c(indRC.Child1,indRC.Child2,indRC.Child3,indRC.Child4)

indParent1<-which(rowSums(!is.na(promis1[,indParent]))>20)
indParent2<-which(rowSums(!is.na(promis2[,indParent]))>20)
indParent3<-which(rowSums(!is.na(promis3[,indParent]))>20)
indParent4<-which(rowSums(!is.na(promis4[,indParent]))>20)


indRC.Parent1<-match(promis1$record_id[indParent1],REDCAP$ids)
indRC.Parent2<-match(promis2$record_id[indParent2],REDCAP$ids)
indRC.Parent3<-match(promis3$record_id[indParent3],REDCAP$ids)
indRC.Parent4<-match(promis4$record_id[indParent4],REDCAP$ids)

indRC.Parent1234<-c(indRC.Parent1,indRC.Parent2,indRC.Parent3,indRC.Parent4)

Parent1<-as.matrix(promis1[indParent1,indParent][,-c(1:4,54)])
Parent2<-as.matrix(promis2[indParent2,indParent][,-c(1:4,54)])
Parent3<-as.matrix(promis3[indParent3,indParent][,-c(1:4,54)])
Parent4<-as.matrix(promis4[indParent4,indParent][,-c(1:4,54)])

indAdult1<-which((rowSums(!is.na(promis1[,indAdult])))>15)

Adult1<-as.matrix(promis1[indAdult1,indAdult][,-c(1:4,20)])
Adult2<-as.matrix(promis2[(rowSums(!is.na(promis2[,indAdult])))>15,indAdult][,-c(1:4,20)])
Adult3<-as.matrix(promis3[(rowSums(!is.na(promis3[,indAdult])))>15,indAdult][,-c(1:4,20)])
Adult4<-as.matrix(promis4[(rowSums(!is.na(promis4[,indAdult])))>15,indAdult][,-c(1:4,20)])

indRC.Adult1<-match(promis1$record_id[indAdult1],REDCAP$ids)


NC1<-paste0("HS_",REDCAP$ids[indRC.Child1])
NC2<-paste0("HS_",REDCAP$ids[indRC.Child2])
NC3<-paste0("HS_",REDCAP$ids[indRC.Child3])
NC4<-paste0("HS_",REDCAP$ids[indRC.Child4])

NP1<-paste0("HS_",REDCAP$ids[indRC.Parent1])
NP2<-paste0("HS_",REDCAP$ids[indRC.Parent2])
NP3<-paste0("HS_",REDCAP$ids[indRC.Parent3])
NP4<-paste0("HS_",REDCAP$ids[indRC.Parent4])

NA1<-paste0("HS_",REDCAP$ids[indRC.Adult1])

rownames(Child1)<-NC1
rownames(Child2)<-NC2
rownames(Child3)<-NC3
rownames(Child4)<-NC4

rownames(Parent1)<-NP1
rownames(Parent2)<-NP2
rownames(Parent3)<-NP3
rownames(Parent4)<-NP4

rownames(Adult1)<-NA1

#####################################
PCA.Child1<-prcomp((Child1))
PCA.Child1234<-prcomp(rbind(Child1,Child2,Child3,Child4))

PCA.Child234<-prcomp(rbind(Child2,Child3,Child4))


PCA.Parent1<-prcomp(Parent1)
PCA.Parent1234<-prcomp(rbind(Parent1,Parent2,Parent3,Parent4))

PCA.Adult1<-prcomp(Adult1)

#####################################

image(cor((Child1%*%PCA.Child1$rotation)[,], PCA.Child1$x[,]))

varexpCh.l<-PCA.Child1$sdev^2/sum(PCA.Child1$sdev^2)
PCT.Child1<-paste0(colnames(PCA.Child1$x)," (",percent(varexpCh.l,.1),")")
ChildGene1<-REDCAP[indRC.Child1,]$newgene
ChildGene1[ChildGene1%in%c("noResult","Unknown",NA)]<-"Unknown"


varexpCh.l234<-PCA.Child1234$sdev^2/sum(PCA.Child1234$sdev^2)
PCT.Child1234<-paste0(colnames(PCA.Child1234$x)," (",percent(varexpCh.l234,.1),")")

#####################################
Projection.Child1.1234<-rbind(Child1,Child2,Child3,Child4)%*%PCA.Child1$rotation
rownames(Projection.Child1.1234)<-c(paste0(NC1,"_a"),paste0(NC2,"_b"),paste0(NC3,"_c"),paste0(NC4,"_d"))

Projection.Parent1.1234<-rbind(Parent1,Parent2,Parent3,Parent4)%*%PCA.Parent1$rotation
rownames(Projection.Parent1.1234)<-c(paste0(NP1,"_a"),paste0(NP2,"_b"),paste0(NP3,"_c"),paste0(NP4,"_d"))


######################################
PROCH1<-promis1[indChild1,c(1,indChild)]
PROCH2<-promis2[indChild2,c(1,indChild)]
PROCH3<-promis3[indChild3,c(1,indChild)]
PROCH4<-promis4[indChild4,c(1,indChild)]

DateCh12.1<-as.Date(PROCH1$fill_date[match(intersect(PROCH1$record_id,PROCH2$record_id),PROCH1$record_id)])
DateCh12.2<-as.Date(PROCH2$fill_date[match(intersect(PROCH1$record_id,PROCH2$record_id),PROCH2$record_id)])
DateCh23.1<-as.Date(PROCH2$fill_date[match(intersect(PROCH2$record_id,PROCH3$record_id),PROCH2$record_id)])
DateCh23.2<-as.Date(PROCH3$fill_date[match(intersect(PROCH2$record_id,PROCH3$record_id),PROCH3$record_id)])
DateCh34.1<-as.Date(PROCH3$fill_date[match(intersect(PROCH3$record_id,PROCH4$record_id),PROCH3$record_id)])
DateCh34.2<-as.Date(PROCH4$fill_date[match(intersect(PROCH4$record_id,PROCH4$record_id),PROCH4$record_id)])

IntCh1<-as.numeric(difftime(DateCh12.2, DateCh12.1, units = "days"))
IntCh2<-as.numeric(difftime(DateCh23.2, DateCh23.1, units = "days"))
IntCh3<-as.numeric(difftime(DateCh34.2, DateCh34.1, units = "days"))

summary(c(IntCh1,IntCh2,IntCh3))
boxplot(list(IntCh1,IntCh2,IntCh3))


PROPA1<-promis1[indParent1,c(1,indParent)]
PROPA2<-promis2[indParent2,c(1,indParent)]
PROPA3<-promis3[indParent3,c(1,indParent)]
PROPA4<-promis4[indParent4,c(1,indParent)]

DatePA12.1<-as.Date(PROPA1$reg_date2[match(intersect(PROPA1$record_id,PROPA2$record_id),PROPA1$record_id)])
DatePA12.2<-as.Date(PROPA2$reg_date2[match(intersect(PROPA1$record_id,PROPA2$record_id),PROPA2$record_id)])
DatePA23.1<-as.Date(PROPA2$reg_date2[match(intersect(PROPA2$record_id,PROPA3$record_id),PROPA2$record_id)])
DatePA23.2<-as.Date(PROPA3$reg_date2[match(intersect(PROPA2$record_id,PROPA3$record_id),PROPA3$record_id)])
DatePA34.1<-as.Date(PROPA3$reg_date2[match(intersect(PROPA3$record_id,PROPA4$record_id),PROPA3$record_id)])
DatePA34.2<-as.Date(PROPA4$reg_date2[match(intersect(PROPA4$record_id,PROPA4$record_id),PROPA4$record_id)])

IntPA1<-as.numeric(difftime(DatePA12.2, DatePA12.1, units = "days"))
IntPA2<-as.numeric(difftime(DatePA23.2, DatePA23.1, units = "days"))
IntPA3<-as.numeric(difftime(DatePA34.2, DatePA34.1, units = "days"))

boxplot(list(IntPA1,IntPA2,IntPA3))
summary(c(IntPA1,IntPA2,IntPA3))


