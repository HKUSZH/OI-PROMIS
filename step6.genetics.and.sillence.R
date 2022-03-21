library(scales)


pdf("genetics.and.sillence.pdf")
	ggplot(as.data.frame(tab72)%>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%\nn=",Freq)),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab)) + 
	geom_col(aes(fill=factor(Var1)),width=1)  +
	xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	scale_fill_brewer(palette="Set3") +
	coord_polar("y") + ggtitle("GENE distr among total 90 patients")

################
	barplot(table(SEQ69$SillenceType, GENES)[,rev(names(tab72))], las=2, col=hue_pal()(5))
	barplot(rep(1,5), col=hue_pal()(5))

dev.off()

pdf("Genotyps.and.pheno.in.two.clusters.mar16.2022.pdf",width=9)

	ggplot(as.data.frame(tabGeneClust)%>%group_by(Var2)%>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%;n=",Freq)),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab,fill=Var1)) + geom_col(width=1)  + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	facet_wrap(vars(Var2)) +
	scale_fill_brewer(palette="Set3") +
	coord_polar("y") + ggtitle("Genotyps distr in the two clusters")
######
	ggplot(as.data.frame(tabSillenceClust)%>%group_by(Var2)%>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%;n=",Freq)),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab,fill=Var1)) + geom_col(width=1)  + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	facet_wrap(vars(Var2)) +
	scale_fill_brewer(palette="Set3") +
	coord_polar("y") + ggtitle("Genotyps distr in the two clusters")

dev.off()



