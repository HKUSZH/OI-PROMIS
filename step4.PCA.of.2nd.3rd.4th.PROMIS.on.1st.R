
GROUP_for_all<-PROMGROUP[match(REDCAP[["ids"]], names(PROMGROUP))]

tabGender<-table(GROUP_for_all[GROUP_for_all!="ambiguous"],REDCAP$gender[GROUP_for_all!="ambiguous"])
tabFund<-table(GROUP_for_all[GROUP_for_all!="ambiguous"],REDCAP$fund[GROUP_for_all!="ambiguous"])
tabTreat<-table(GROUP_for_all[GROUP_for_all!="ambiguous"],paste0(REDCAP$treatment___1,"*",REDCAP$treatment___2)[GROUP_for_all!="ambiguous"])

chisq.test(table(GROUP_for_all[GROUP_for_all!="ambiguous"],REDCAP$gender[GROUP_for_all!="ambiguous"]),correct=F)
chisq.test(table(GROUP_for_all[GROUP_for_all!="ambiguous"],REDCAP$treatment___1[GROUP_for_all!="ambiguous"]),correct=F)
chisq.test(tabTreat,correct=F)
chisq.test(tabFund,correct=F)

	heatmap(cor(t(Child1),t(Child2)),col=greenred(20))
	heatmap(cor(t(Child1),t(Child3)),col=greenred(20))
	heatmap(cor(t(Child1),t(Child4)),col=greenred(20))
	heatmap(Child2[match(intersect(indRC.Child1,indRC.Child2),indRC.Child2),]-Child1[ match(intersect(indRC.Child1,indRC.Child2),indRC.Child1),],col=greenred(20))
	

	heatmap(cor(t(Child1),t(Child2)),col=greenred(20))
	heatmap(cor(t(Parent1),t(Parent2)),col=greenred(20))

pdf("PCA.projections.onto.PROMIS-1.pdf",width=8)

	ggplot(data.frame(Projection.Child1.1234,GROUP=GROUP_for_all[indRC.Child1234],REDCAP[indRC.Child1234,]),
		aes(PC1,PC2,label=rownames(Projection.Child1.1234))) +
		geom_point(size=4,aes(color=GROUP)) +
		geom_density_2d() +
		geom_path(aes(group=factor(record_id)),size=1,arrow = arrow(length = unit(0.6,"cm"))) +
		geom_text() +xlab(PCT.Child1[1]) +ylab(PCT.Child1[2]) +
		ggtitle("Projection of Children self-assessments to PROMIS-1") + theme(legend.position = "none")

	ggplot(data.frame(Projection.Child1.1234,GROUP=GROUP_for_all[indRC.Child1234],REDCAP[indRC.Child1234,]),
		aes(PC1,PC2,label=rownames(Projection.Child1.1234))) +
		geom_point(size=4,aes()) +
		geom_density_2d() +
		geom_path(aes(group=factor(record_id),color=GROUP),size=1,arrow = arrow(length = unit(0.6,"cm"))) +
		geom_text() +xlab(PCT.Child1[1]) +ylab(PCT.Child1[2]) +
		ggtitle("Projection of Children self-assessments to PROMIS-1") + theme(legend.position = "none")

	ggplot(data.frame(Projection.Parent1.1234,GROUP=GROUP_for_all[indRC.Parent1234],
			ROWNAME=rownames(Projection.Parent1.1234),REDCAP[indRC.Parent1234,]),
		aes(PC1,PC2,label=ROWNAME)) +
		geom_point(size=4,aes(color=GROUP)) +
		geom_density_2d() +
		geom_path(aes(group=factor(record_id)),
			size=1,
			arrow = arrow(length = unit(0.6,"cm"))) +
		geom_text() +xlab(PCT.Parent1[1]) +ylab(PCT.Parent1[2]) +
		ggtitle("Projection PCA of Parent self-assessments to PROMIS-1") + theme(legend.position = "none")

	ggplot(data.frame(Projection.Parent1.1234,
			GROUP=GROUP_for_all[indRC.Parent1234],
			ROWNAME=rownames(Projection.Parent1.1234),REDCAP[indRC.Parent1234,]),
		aes(PC1,PC2,label=ROWNAME)) +
		geom_point(size=4,aes()) +
		geom_density_2d() +
		geom_path(aes(group=factor(record_id),color=GROUP),
			size=1,
			arrow = arrow(length = unit(0.6,"cm"))) +
		geom_text() +xlab(PCT.Parent1[1]) +ylab(PCT.Parent1[2]) +
		ggtitle("Projection PCA of Parent self-assessments to PROMIS-1") + theme(legend.position = "none")



dev.off()
###############
###############
	ggplot(data.frame(PCA.Child1234$x,GROUP=GROUP_for_all[indRC.Child1234],REDCAP[indRC.Child1234,]),
		aes(PC1,PC2,label=rownames(PCA.Child1.2.3.4))) +
		geom_point(size=6,aes(color=GROUP,shape=GROUP)) +
		geom_density_2d() +
		geom_path(aes(group=factor(record_id),color=factor(record_id)),size=1,
			arrow = arrow(), show.legend=FALSE) +
		geom_text() +xlab(PCT.Child1234[1]) +ylab(PCT.Child1234[2]) +
		ggtitle("re-PCA of Children self-assessments (all PROMIS combined)") 

###############
	ggplot(data.frame(PCA.Child1234$x,REDCAP[c(indRC.Child1,indRC.Child2,indRC.Child3,indRC.Child4),]),
		aes(PC1,PC2,label=rownames(PCA.Child1.2.3.4))) +
		geom_point(size=4,aes(color=newgene)) +
		geom_density_2d() +
		geom_path(aes(group=factor(record_id),color=factor(record_id)),
			size=1,
			arrow = arrow(length = unit(0.6,"cm"))) +
		geom_text() +xlab(PCT.Child1234[1]) +ylab(PCT.Child1234[2]) +
		ggtitle("re-PCA of Children self-assessments")

	P1<-ggplot(data.frame(PCA.Child1234$x,REDCAP[c(indRC.Child1,indRC.Child2,indRC.Child3,indRC.Child4),])%>%
			mutate(LAB=paste0(pa_name1,":",gsub("HS_","",ids))),
		aes(PC1,PC2,label=LAB)) +
		geom_point(size=4,aes(color=newgene)) +
		geom_density_2d() +
		geom_path(aes(group=factor(record_id),color=factor(record_id)),
			size=1,
			arrow = arrow(length = unit(0.6,"cm"))) +
		geom_text() + xlab(PCT.Child1[1]) +ylab(PCT.Child1[2]) +
		ggtitle("PCA of Children self-assessments") + theme(legend.position = "none")

###############

	ggplot(data.frame(PCA.Parent1234$x,
			ROWNAME=rownames(Projection.Parent1.1234),
			GROUP=GROUP_for_all[indRC.Parent1234],REDCAP[indRC.Parent1234,]),
		aes(PC1,PC2,label=ROWNAME)) +
		geom_point(size=4,aes(color=GROUP)) +
		geom_density_2d() +
		geom_path(aes(group=factor(record_id),fill=factor(record_id)),
			size=1,
			arrow = arrow(length = unit(0.6,"cm"))) +
		geom_text() +xlab(PCT.Parent1[1]) +ylab(PCT.Parent1[2]) +
		ggtitle("re-PCA of Parent self-assessments")


	P2<-ggplot(data.frame(PCA.Parent1234$x,REDCAP[indRC.Parent1234,]),
		aes(PC1,PC2,label=paste0(pa_name1,":",gsub("HS_","",rownames(PCA.Parent1234$x))))) +
		geom_point(size=4,aes(color=newgene)) +
		geom_density_2d() +
		geom_path(aes(group=factor(record_id),color=factor(record_id)),
			size=1,
			arrow = arrow(length = unit(0.6,"cm"))) +
		geom_text() +xlab(PCT.Parent1[1]) +ylab(PCT.Parent1[2]) +
		ggtitle("PCA of Parentren self-assessments")+ theme(legend.position = "none")

###############
	library(grid)
	library(gridExtra)
	grid.arrange(P1, P2,ncol=2)

	par(mfrow=c(2,1))
	barplot(sort(PCA.Parent1234$rotation[,1]),las=2,ylab="PC1")
	barplot(sort(PCA.Parent1234$rotation[,2]),las=2,ylab="PC2")



