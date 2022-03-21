library(gg3D)
library(plotly)
library(d3heatmap)
library(RColorBrewer)
library(gplots)

##################################################


venn(list(rownames(Child1),rownames(Parent1)))

d3<-d3heatmap(Child1,colors="Blues")
barplot(rep(1,8),col=)

ggplot(pivot_longer(data.frame(t(Child1)),everything(),"patients"),
	aes(patients,value)) + geom_violin(width=2) +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##################################################

plot(PCA.Child1$x[,1:2])


pdf("PCA.Child1.pdf",width=9)
	ggplot(data.frame(PCs=colnames(PCA.Child1$x),varexpl,PCT=PCT.Child1) %>%
			mutate(lab=rev(PCT),category=c(PCs[1:5],rep("others",45)),
			CAT=factor(category,levels=c(paste0("PC",seq(5)),"others")),
			ypos=cumsum(rev(varexpl))-rev(varexpl)/2),
		aes(x=1,y=varexpl,label=lab,fill=CAT)) + 
		geom_col(width=1) + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
		scale_fill_brewer(palette="Greens",direction = -1) +
		coord_polar("y") + ggtitle("Percentages explained in Child1")

	fig3d<-plot_ly(data=data.frame(PCA.Child1$x),x=~PC1,y=~PC2,z=~PC3,
		color=~ChildGene1,colors="Set3",
		type="scatter3d",mode="markers")
	orca(fig3d, "child-plotly-3d.svg")


	ggplot(data.frame(PCA.Child1$x,REDCAP[indRC.Child1,]),
		aes(x=PC1,y=PC2,z=PC3,size=4,color=newgene)) +
		axes_3D() + stat_3D() + theme_void() +
		ggtitle("PCA of Children self-assessments")

	ggplot(data.frame(PCA.Child1$x,REDCAP[indRC.Child1,]),
		aes(PC1,PC2,label=rownames(Child1))) +
		geom_point(size=4,aes(color=newgene)) +
		geom_density_2d() +
		geom_text() +xlab(PCT.Child1[1]) +ylab(PCT.Child1[2]) +
		ggtitle("PCA of Children self-assessments")

	par(mfrow=c(2,1))
	barplot(sort(PCA.Child1$rotation[,1]),las=2)
	barplot(sort(PCA.Child1$rotation[,2]),las=2)

	par(mfrow=c(1,1))

	ggplot(data.frame(PCA.Child1$x,REDCAP[indRC.Child1,]),
		aes(PC2,PC3,label=rownames(Child1))) +
		geom_point(size=4,aes(color=newgene)) +
		geom_density_2d() +
		geom_text() +xlab(PCT.Child1[2]) +ylab(PCT.Child1[3]) +
		ggtitle("PCA of Children self-assessments")


	ggplot(data.frame(PCA.Child1$x,REDCAP[indRC.Child1,]),
		aes(PC1,PC2,label=rownames(Child1))) +
		geom_point(size=4,aes(color=newgene)) +
		geom_density_2d() +
		geom_text() +xlab(PCT.Child1[1]) +ylab(PCT.Child1[2]) +
		ggtitle("PCA of Children self-assessments")

	ggplot(data.frame(PCA.Child1$x,REDCAP[indRC.Child1,])%>%mutate(twogroups=PC1>PC2),
		aes(PC1,PC2,label=rownames(Child1))) +
		geom_point(size=4,aes(color=twogroups)) +
		geom_density_2d() +
		geom_text() +xlab(PCT.Child1[1]) +ylab(PCT.Child1[2]) +
		ggtitle("PCA of Children self-assessments")

dev.off()
plot(PCA.Child1$x[,1:2])
abline(0,1)
GroupChild1<-split(rownames(Child1),PCA.Child1$x[,1]>PCA.Child1$x[,2])

ggplot(data.frame(PCA.Child1$x,REDCAP[indRC.Child1,]),
	aes(PC1,PC2,label=paste0(newgene))) +
	geom_point(size=4,aes(color=factor(joint))) +
	geom_density_2d() +
	geom_text() +xlab(PCT.Child1[1]) +ylab(PCT.Child1[2]) +
	ggtitle("PCA of Children self-assessments")


indRC.Parent1<-match(promis1$record_id[indParent1],REDCAP$ids)
ggplot(data.frame(PCA.Parent1$x,REDCAP[indRC.Parent1,]),
	aes(PC1,PC2,label=paste0(newgene))) +
	geom_point(size=4,aes(color=factor(joint))) +
	geom_density_2d() +
	geom_text() +xlab(PCT[1]) +ylab(PCT[2]) +
	ggtitle("Parental assessments on their children")

##################################################
pdf("d3heat.Child1.row.ridges.pdf",height=12,width=6)
	d3<-d3heatmap(Child1,colors="Blues")

	ggplot(pivot_longer(data.frame(t(Child1)),everything(),"patients"),
		aes(x =value ,y = factor(patients,levels=rev(d3$x$matrix$rows)))) + geom_density_ridges2() +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(pivot_longer(data.frame(t(Child1)),everything(),"patients"),
		aes(x =value ,y = 1)) + geom_density_ridges2() +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
		ggtitle("aggregate")

	ggplot(pivot_longer(data.frame(Child1),everything(),"items"),
		aes(x =value ,y = factor(items,levels=rev(d3$x$matrix$cols)))) + geom_density_ridges2() +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(pivot_longer(data.frame(t(Child1)),everything(),"patients"),
		aes(x =value ,y = 1)) + geom_density_ridges2() +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
		ggtitle("aggregate")
dev.off()

blues_fun <- colorRampPalette(brewer.pal(9,"Blues"))
barplot(rep(1,11),col=blues_fun(11))

greens_fun <- colorRampPalette(brewer.pal(9,"Greens"))
barplot(rep(1,11),col=greens_fun(11))
##################################################
#			Parent 1
##################################################
varExpPa_1<-PCA.Parent1$sdev^2/sum(PCA.Parent1$sdev^2)
PCT.Parent1<-paste0(colnames(PCA.Parent1$x)," (",percent(PCA.Parent1$sdev^2/sum(PCA.Parent1$sdev^2),.1),")")
ParentGene1<-REDCAP[indRC.Parent1,]$newgene
ParentGene1[ParentGene1%in%c("noResult","Unknown","noMut",NA)]<-"Unknown"

pdf("PCA.Parent1.pdf",width=9)
	ggplot(data.frame(PCs=colnames(PCA.Parent1$x),varExpPa_1,PCT=PCT.Parent1) %>%
			mutate(lab=rev(PCT),category=c(PCs[1:5],rep("others",44)),
			CAT=factor(category,levels=c(paste0("PC",seq(5)),"others")),
			ypos=cumsum(rev(varExpPa_1))-rev(varExpPa_1)/2),
		aes(x=1,y=varExpPa_1,label=lab,fill=CAT)) + 
		geom_col(width=1) + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
		scale_fill_brewer(palette="Greens",direction = -1) +
		coord_polar("y") + ggtitle("Percentages explained in Parent1")

	fig3d<-plot_ly(data=data.frame(PCA.Parent1$x),x=~PC1,y=~PC2,z=~PC3,
		color=~ChildGene1,colors="Set3",
		type="scatter3d",mode="markers")
	orca(fig3d, "child-plotly-3d.svg")


	ggplot(data.frame(PCA.Parent1$x,REDCAP[indRC.Parent1,]),
		aes(x=PC1,y=PC2,z=PC3,size=4,color=newgene)) +
		axes_3D() + stat_3D() + theme_void() +
		ggtitle("PCA of Children self-assessments")

	ggplot(data.frame(PCA.Parent1$x,REDCAP[indRC.Parent1,]),
		aes(PC1,PC2,label=rownames(Parent1))) +
		geom_point(size=4,aes(color=ParentGene1)) +
		scale_color_brewer(palette="Set3") +
		geom_density_2d() +
		geom_text() +xlab(PCT.Parent1[1]) +ylab(PCT.Parent1[2]) +
		ggtitle("PCA of Children self-assessments")

	par(mfrow=c(2,1))
	barplot(sort(PCA.Parent1$rotation[,1]),las=2,ylab="PC1")
	barplot(sort(PCA.Parent1$rotation[,2]),las=2,ylab="PC2")

	par(mfrow=c(1,1))

	ggplot(data.frame(PCA.Parent1$x,REDCAP[indRC.Parent1,]),
		aes(PC2,PC3,label=rownames(Parent1))) +
		geom_point(size=4,aes(color=newgene)) +
		geom_density_2d() +
		geom_text() +xlab(PCT.Parent1[2]) +ylab(PCT.Parent1[3]) +
		ggtitle("PCA of Children self-assessments")


	ggplot(data.frame(PCA.Parent1$x,REDCAP[indRC.Parent1,]),
		aes(PC1,PC2,label=rownames(Parent1))) +
		geom_point(size=4,aes(color=ParentGene1,shape=factor(gender))) +
		scale_color_brewer(palette="Set3") +
		geom_density_2d() +
		geom_text() +xlab(PCT.Parent1[1]) +ylab(PCT.Parent1[2]) +
		ggtitle("PCA of Children self-assessments")


	ggplot(data.frame(PCA.Parent1$x,REDCAP[indRC.Parent1,])%>%mutate(GROUP=PC1>PC2),
		aes(PC1,PC2,label=rownames(Parent1))) +
		geom_point(size=4,aes(color=GROUP,shape=factor(gender))) +
		geom_density_2d() +
		geom_text() +xlab(PCT.Parent1[1]) +ylab(PCT.Parent1[2]) +
		ggtitle("PCA of Children self-assessments")

dev.off()
pdf("d3heat.Parent1.row.ridges.pdf",height=12,width=6)
	d3<-d3heatmap(Parent1,colors="Greens")

	ggplot(pivot_longer(data.frame(t(Parent1)),everything(),"patients"),
		aes(x =value ,y = factor(patients,levels=rev(d3$x$matrix$rows)))) + geom_density_ridges2() +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

	ggplot(pivot_longer(data.frame(t(Parent1)),everything(),"patients"),
		aes(x =value ,y = 1)) + geom_density_ridges2() +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
		ggtitle("aggregate")

	ggplot(pivot_longer(data.frame(Parent1),everything(),"items"),
		aes(x =value ,y = factor(items,levels=rev(d3$x$matrix$cols)))) + geom_density_ridges2() +
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dev.off()


#############################################
#	Child and Parent condordance check
#############################################
plot(PCA.Parent1$x[,1:2])
abline(0,1)
GroupParent1<-split(rownames(Parent1),PCA.Parent1$x[,1]>PCA.Parent1$x[,2])

venn(c(GroupChild1,GroupParent1))
LConcord<-attr(venn(c(GroupChild1,GroupParent1)),"intersections")
#############################################
#	Identified two groups of patients
#############################################
indChPa<-sort(union(indRC.Child1,indRC.Parent1))

PROMGROUP<-rep("Cluster1",length(indChPa))
names(PROMGROUP)<-REDCAP$ids[indChPa]
PROMGROUP[REDCAP$ids[indChPa]%in%gsub("HS_","",GroupChild1[["TRUE"]])]<-"Cluster2"
PROMGROUP[REDCAP$ids[indChPa]%in%gsub("HS_","",GroupParent1[["TRUE"]])]<-"Cluster2"
PROMGROUP[REDCAP$ids[indChPa]%in%gsub("HS_","",setdiff(unlist(LConcord[1:2]),"HS_19"))]<-"ambiguous"

DocChPA<-REDCAP[indChPa,indDoc]
indAmbi<-which(PROMGROUP!="ambiguous")
LP<-apply(DocChPA,2,function(x){
	tryCatch(chisq.test(table(x[indAmbi],PROMGROUP[indAmbi]),correct=T)$p.value, error = function(e) e, finally = {})
	
})
PVEC<-as.numeric(unlist(sapply(LP,function(x)x[1]),length))

ggplot(data.frame(PVEC,VAR=colnames(DocChPA),NumVal=colSums(!is.na(DocChPA)),NumNotEmpt=colSums(DocChPA!="",na.rm=T))%>%arrange(PVEC)%>%drop_na(PVEC)%>%
		filter(!VAR%in%c("doc_name","pa_name2_1","gene_test_time_result","ddcd_263414_complete")&NumVal>10&NumNotEmpt>10)%>%
		mutate(logP=-log10(PVEC)),
	aes(x=reorder(VAR,-logP),y=logP,label=VAR)) + geom_bar(stat="identity") +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

tabGender<-table(GROUP_for_all[GROUP_for_all!="ambiguous"],REDCAP$gender[GROUP_for_all!="ambiguous"])
tabFund<-table(GROUP_for_all[GROUP_for_all!="ambiguous"],REDCAP$fund[GROUP_for_all!="ambiguous"])
tabTreat<-table(GROUP_for_all[GROUP_for_all!="ambiguous"],paste0(REDCAP$treatment___1,"*",REDCAP$treatment___2)[GROUP_for_all!="ambiguous"])
tabIncome<-table(GROUP_for_all[GROUP_for_all!="ambiguous"],REDCAP$income[GROUP_for_all!="ambiguous"])
tabSchool<-table(GROUP_for_all[GROUP_for_all!="ambiguous"],
	paste0(REDCAP$schooling_v2___1,REDCAP$schooling_v2___2,
	REDCAP$schooling_v2___3)[GROUP_for_all!="ambiguous"]!="NANANA")

Gene70<-REDCAP$newgene[indChPa[indAmbi]]
Gene70[Gene70%in%c("noMut","Unknown","noResult")]<-"zUnknown"

pdf("Genotyps.and.pheno.in.two.clusters.pdf",width=9)
	ggplot(as.data.frame(table(Gene70,PROMGROUP[indAmbi]))%>%group_by(Var2)%>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%;n=",Freq)),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab,fill=Gene70)) + geom_col(width=1)  + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	facet_wrap(vars(Var2)) +
	scale_fill_brewer(palette="Set3") +
	coord_polar("y") + ggtitle("Genotyps distr in the two clusters")

	ggplot(as.data.frame(table(clinical_type=REDCAP$clinical_type[indChPa[indAmbi]],PROMGROUP[indAmbi]))%>%group_by(Var2)%>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%;n=",Freq)),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab,fill=clinical_type)) + geom_col(width=1)  + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	facet_wrap(vars(Var2)) +
	scale_fill_brewer(palette="Set3") +
	coord_polar("y") + ggtitle("clinical_type distr in the two clusters")


	ggplot(as.data.frame(table(deformity=REDCAP$deformity[indChPa[indAmbi]],PROMGROUP[indAmbi]))%>%group_by(Var2)%>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%;n=",Freq)),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab,fill=deformity)) + geom_col(width=1)  + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	facet_wrap(vars(Var2)) +
	scale_fill_brewer(palette="Set3") +
	coord_polar("y") + ggtitle("deformity distr in the two clusters")


	ggplot(as.data.frame(table(leg_equa=REDCAP$leg_equa[indChPa[indAmbi]],PROMGROUP[indAmbi]))%>%group_by(Var2)%>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%;n=",Freq)),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab,fill=leg_equa)) + geom_col(width=1)  + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	facet_wrap(vars(Var2)) +
	scale_fill_brewer(palette="Set3") +
	coord_polar("y") + ggtitle("leg_equa distr in the two clusters")


	ggplot(as.data.frame(table(joint=REDCAP$joint[indChPa[indAmbi]],PROMGROUP[indAmbi]))%>%group_by(Var2)%>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%;n=",Freq)),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab,fill=joint)) + geom_col(width=1)  + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	facet_wrap(vars(Var2)) +
	scale_fill_brewer(palette="Set3") +
	coord_polar("y") + ggtitle("joint distr in the two clusters")
###############
###############
	ggplot(as.data.frame(tabGender)%>%group_by(Var1)%>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%;n=",Freq)),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab,fill=Var2)) + geom_col(width=1)  + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	facet_wrap(vars(Var1)) +
	coord_polar("y") + ggtitle("Gender distr in the two clusters")

	ggplot(as.data.frame(tabFund)%>%group_by(Var1)%>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%;n=",Freq)),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab,fill=Var2)) + geom_col(width=1)  + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	facet_wrap(vars(Var1)) +
	coord_polar("y") + ggtitle("Funding distr in the two clusters")

	ggplot(as.data.frame(tabTreat)%>%group_by(Var1)%>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%;n=",Freq)),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab,fill=Var2)) + geom_col(width=1)  + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	facet_wrap(vars(Var1)) +
	coord_polar("y") + ggtitle("Treatment distr in the two clusters")

	ggplot(as.data.frame(tabIncome)%>%group_by(Var1)%>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%;n=",Freq)),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab,fill=Var2)) + geom_col(width=1)  + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	scale_fill_brewer(palette="Blues") +
	facet_wrap(vars(Var1)) +
	coord_polar("y") + ggtitle("Treatment distr in the two clusters")

	ggplot(as.data.frame(tabSchool)%>%group_by(Var1)%>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%;n=",Freq)),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab,fill=Var2)) + geom_col(width=1)  + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	scale_fill_brewer(palette="Blues") +
	facet_wrap(vars(Var1)) +
	coord_polar("y") + ggtitle("Schooling distr in the two clusters")


###############
library(ggbeeswarm)

	t.test(REDCAP[indChPa[indAmbi],"age"]~PROMGROUP[indAmbi])
	ggplot(data.frame(REDCAP[indChPa[indAmbi],],PROMGROUP=PROMGROUP[indAmbi]),
		aes(x=PROMGROUP,y=age)) + 
	geom_violin(width=1)  + 
	geom_boxplot(width=0.5)  + geom_quasirandom(aes(color=PROMGROUP),size=4) +
	ggtitle("Age distr in the two clusters")

	REDCAP.twogroups<-data.frame(REDCAP[indChPa[indAmbi],],PROMGROUP=PROMGROUP[indAmbi])
	save(REDCAP.twogroups,file="REDCAP.twogroups.RData")

dev.off()


##################################################

venn(list("once"=unique(promis1$record_id),"twice"=unique(promis2$record_id),"thrice"=unique(promis3$record_id)))
venn(list("once"=promis1$record_id,"twice"=promis2$record_id,"thrice"=promis3$record_id))
venn(list("once"=promis1$record_id,"twice"=promis2$record_id,"thrice"=promis3$record_id,"fourth"=promis4$record_id))





