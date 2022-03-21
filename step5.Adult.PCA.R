PCA.Adult1<-prcomp(Adult1)
svd.Adult1<-svd(apply(Adult1,2,function(x)x-mean(x)))
plot(data.frame(PCA.Adult1$rotation[,1:4],svd.Adult1$v[,1:4]))

plot(PCA.Adult1$sdev,svd.Adult1$d/sqrt(15))
abline(0,1,col=2)

varExpAd_1<-PCA.Adult1$sdev^2/sum(PCA.Adult1$sdev^2)
PCT.Adult1<-paste0(colnames(PCA.Adult1$x)," (",percent(PCA.Adult1$sdev^2/sum(PCA.Adult1$sdev^2),.1),")")
AdultGene1<-REDCAP[indRC.Adult1,]$newgene
AdultGene1[AdultGene1%in%c("noResult","Unknown","noMut",NA)]<-"Unknown"
####################################


pdf("Adult.PCA.pdf",width=8.5)
	ggplot(data.frame(PCA.Adult1$x,REDCAP[indRC.Adult1,]),
		aes(PC1,PC2,label=rownames(Adult1))) +
		geom_point(size=4,aes(color=newgene,shape=factor(gender))) +
		geom_density_2d() +
		geom_text() +xlab(PCT.Adult1[1]) +ylab(PCT.Adult1[2]) +
		ggtitle("PCA of Adult self-assessments")

	par(mar=c(9,4,4,2))
	par(mfrow=c(2,1))
	barplot(sort(PCA.Adult1$rotation[,1]),las=2,ylab="PC1",cex.axis=1/2)
	barplot(sort(PCA.Adult1$rotation[,2]),las=2,ylab="PC2")
dev.off()

d3<-d3heatmap(Adult1,colors="Purples")
purples_fun <- colorRampPalette(brewer.pal(9,"Purples"))
barplot(rep(1,5),col=purples_fun(5))


pdf("d3heat.Adult1.row.ridges.pdf",height=12,width=6)
	d3<-d3heatmap(Adult1,colors="Greens")

	ggplot(pivot_longer(data.frame(t(Adult1)),everything(),"patients"),
		aes(x =value ,y = factor(patients,levels=rev(d3$x$matrix$rows)))) + geom_density_ridges2() +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(pivot_longer(data.frame(t(Adult1)),everything(),"patients"),
		aes(x =value ,y = 1)) + geom_density_ridges2() +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
		ggtitle("aggregate")

	ggplot(pivot_longer(data.frame(Adult1),everything(),"items"),
		aes(x =value ,y = factor(items,levels=rev(d3$x$matrix$cols)))) + geom_density_ridges2() +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dev.off()

###########################################
DocAd<-REDCAP[indRC.Adult1,indDoc]
DocAd$perinatal_2___3<- 2- DocAd$perinatal_2___3
rownames(DocAd)<-REDCAP$ids[indRC.Adult1]

AdP<-apply(DocAd,2,function(x){
	tryCatch(t.test(PCA.Adult1$x[,1]~x)$p.value, error = function(e) e, finally = {})
	
})
PVECAd<-as.numeric(unlist(sapply(AdP,function(x)x[1]),length))

d3heatmap(DocAd[,which(PVECAd<0.06)][-c(11),])
d3heatmap(DocAd[,which(!is.na(PVECAd)&!colnames(DocAd)%in%c("doc_name","pa_name2_1","ddcd_263414_complete" ))][-11,])

table(colSums(!is.na(DocAd))>10,apply(DocAd,2,function(x)length(unique(setdiff(x,NA))))>1)

DocAd2<-DocAd[-c(7,11),colSums(!is.na(DocAd))>10&apply(DocAd,2,function(x)length(unique(setdiff(x,NA))))>1][,-c(1,2,30)]

d3AdCli<-d3heatmap((DocAd2[,-grep("given_birth|^pa|^ma|mother|father|gene_test",colnames(DocAd2))]))
d3AdCli$x$matrix$rows
d3AdCli$x$matrix$cols
as.matrix(paste0("HS",d3AdCli$x$matrix$rows))

table(readr::read_csv("promis90-province.txt"))
tabProvince<-sort(table(readr::read_csv("promis90-province.txt")))
cbind(names(tabProvince),tabProvince,percent(as.numeric(tabProvince/sum(tabProvince)),0.1))

ggplot(data_frame(prov=names(tabProvince),tabProvince,percent=percent(as.numeric(tabProvince/sum(tabProvince)),0.1))%>%
		mutate(PROV=reorder(prov,-tabProvince),LAB=paste0(" (",tabProvince,",",percent,")")), aes(PROV,tabProvince, label=LAB)) +
	geom_col() + geom_text()

####################################
ggplot(data.frame(PVECAd,VAR=colnames(DocAd),NumVal=colSums(!is.na(DocAd)),
		NumNotEmpt=colSums(DocAd!="",na.rm=T))%>%arrange(PVECAd)%>%drop_na(PVECAd)%>%
		filter(!VAR%in%c("doc_name","pa_name2_1","gene_test_time_result","ddcd_263414_complete")&NumVal>10&NumNotEmpt>10)%>%
		mutate(logP=-log10(PVECAd)),
	aes(x=reorder(VAR,-logP),y=logP,label=VAR)) + geom_bar(stat="identity") +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
	ggtitle("Clinical features associated with adults") + xlab("clinical features")




