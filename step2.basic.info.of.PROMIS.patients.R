pdf("piechart.number.of.PROMIS.pdf")
	FREQ<-table(redcap2$record_id[grep(".*_promis_arm_1",redcap2$redcap_event_name)])
	ggplot(as.data.frame(table(FREQ))%>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%\n",Freq)),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab,fill=FREQ)) + geom_col(width=1)  + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	scale_color_brewer(palette="Set2") +
	coord_polar("y") + ggtitle("Num of PROMIS among total 90 patients")
####
	table(names(Freq)==REDCAP_promis$ids)
	ggplot(as.data.frame(table(gender=REDCAP_promis$gender))%>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%\n",Freq)),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab,fill=gender)) + geom_col(width=1)  + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	scale_color_brewer(palette="Set2") +
	coord_polar("y") + ggtitle("gender distr among total 90 patients")

####
	ggplot(as.data.frame(table(FREQ,gender=REDCAP_promis$gender))%>%group_by(gender) %>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%\n",Freq)),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab,fill=FREQ)) + geom_col(width=1)  + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	scale_color_brewer(palette="Set2") +
	facet_wrap(vars(gender)) +
	coord_polar("y") + ggtitle("Num of PROMIS among total 90 patients") 

####
	ggplot(REDCAP_promis[,c("age","gender")]%>%drop_na(age), aes(x=age,color=factor(gender),fill=factor(gender))) + 
		geom_histogram(aes(y=..density..), bins=20,alpha=0.5, position="identity") +
		geom_density(aes(color=factor(gender)),alpha=.2)  +
		scale_color_brewer(palette="Set2") +  theme_minimal()
####
	Genotype<-REDCAP_promis$newgene
	Genotype[Genotype%in%c("noMut", "noResult", "Unknown")]<-"Unknown"
	ggplot(as.data.frame(table(GENE=Genotype))%>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%\nn=",Freq)),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab)) + 
	geom_col(aes(fill=factor(GENE)),width=1)  +
	xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	scale_fill_brewer(palette="Set3") +
	coord_polar("y") + ggtitle("GENE distr among total 90 patients")
####
	ggplot(as.data.frame(table(surgery=REDCAP_promis$treatment___1,BP=REDCAP_promis$treatment___2))%>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%; n=",Freq)),
		treat=paste0(surgery,"__" ,BP),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab,fill=c(treat))) + geom_col(width=1)  + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	scale_fill_brewer(palette="Set2") +
	coord_polar("y") + ggtitle("How the 90 patients were treated")
####
	ggplot(as.data.frame(table(income=REDCAP_promis$income,fund=REDCAP_promis$fund))%>%group_by(fund) %>%
		mutate(prop=100*Freq/sum(Freq),lab=rev(paste0(round(prop,1),"%\n",Freq)),
		ypos=cumsum(rev(prop))-rev(prop)/2),
	aes(x=1,y=prop,label=lab,fill=income)) + geom_col(width=1)  + xlim(c(0,1.5)) + geom_text(aes(y=ypos)) +
	scale_fill_brewer(palette="GnBu") +
	facet_wrap(vars(fund)) +
	coord_polar("y") + ggtitle("Fund and Income") 

dev.off()
