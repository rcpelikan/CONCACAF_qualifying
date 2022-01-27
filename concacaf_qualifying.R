library(TouRnament)
require(ggplot2)
require("ggflags")
require(reshape2)
require(matrixStats)
require(dplyr)
library(countrycode)
library(ggimage)
require("future.apply")
plan(multicore)

n.sims<-3^15   # Important to set this appropriately

##########
##  VISUAL HELPERS
##########
theme_Publication <- function(base_size=14, base_family="Helvetica") {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size, base_family=base_family)
       + theme(plot.title = element_text(face = "bold",
                                         size = rel(1.2), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.y = element_text(angle=90,vjust =2),
               axis.title.x = element_text(vjust = -0.2),
               axis.text = element_text(), 
               axis.line = element_line(colour="black"),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour="#f0f0f0"),
               panel.grid.minor = element_blank(),
               legend.key = element_rect(colour = NA),
               legend.position = "bottom",
               legend.direction = "horizontal",
               legend.key.size= unit(0.2, "cm"),
			   legend.text=element_text(size=14),
               #legend.margin = unit(0, "cm"),
               legend.title = element_text(face="italic"),
               plot.margin=unit(c(10,5,5,5),"mm"),
               strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
               strip.text = element_text(face="bold")
          ))
      
}

scale_fill_Publication <- function(...){
      library(scales)
      discrete_scale("fill","Publication",manual_pal(values = palette), ...)

}

scale_colour_Publication <- function(...){
      library(scales)
      discrete_scale("colour","Publication",manual_pal(values = palette), ...)

}

##########
##  Build table of already played matches
##########
octagonal<-roundrobin(c("USA","MEX","PAN","HON","JAM","ESA","CAN","CRC"))
octagonal$homePoints<- -1
octagonal$awayPoints<- -1

# Well, we already know some results
octagonal$homePoints[with(octagonal,which(Home=="CAN"& Away=="USA"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="CAN"& Away=="MEX"))]<-3
octagonal$homePoints[with(octagonal,which(Home=="CAN"& Away=="PAN"))]<-3
octagonal$homePoints[with(octagonal,which(Home=="CAN"& Away=="CRC"))]<-3
octagonal$homePoints[with(octagonal,which(Home=="CAN"& Away=="JAM"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="CAN"& Away=="ESA"))]<-3
octagonal$homePoints[with(octagonal,which(Home=="CAN"& Away=="HON"))]<-1

octagonal$homePoints[with(octagonal,which(Home=="USA"& Away=="CAN"))]<-1
octagonal$homePoints[with(octagonal,which(Home=="USA"& Away=="MEX"))]<-3
octagonal$homePoints[with(octagonal,which(Home=="USA"& Away=="PAN"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="USA"& Away=="CRC"))]<-3
octagonal$homePoints[with(octagonal,which(Home=="USA"& Away=="JAM"))]<-3
octagonal$homePoints[with(octagonal,which(Home=="USA"& Away=="ESA"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="USA"& Away=="HON"))]<- -1

octagonal$homePoints[with(octagonal,which(Home=="MEX"& Away=="CAN"))]<-1
octagonal$homePoints[with(octagonal,which(Home=="MEX"& Away=="USA"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="MEX"& Away=="PAN"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="MEX"& Away=="CRC"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="MEX"& Away=="JAM"))]<-3
octagonal$homePoints[with(octagonal,which(Home=="MEX"& Away=="ESA"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="MEX"& Away=="HON"))]<-3

octagonal$homePoints[with(octagonal,which(Home=="PAN"& Away=="CAN"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="PAN"& Away=="USA"))]<- 3
octagonal$homePoints[with(octagonal,which(Home=="PAN"& Away=="MEX"))]<- 1
octagonal$homePoints[with(octagonal,which(Home=="PAN"& Away=="CRC"))]<- 1
octagonal$homePoints[with(octagonal,which(Home=="PAN"& Away=="JAM"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="PAN"& Away=="ESA"))]<- 3
octagonal$homePoints[with(octagonal,which(Home=="PAN"& Away=="HON"))]<- -1

octagonal$homePoints[with(octagonal,which(Home=="CRC"& Away=="CAN"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="CRC"& Away=="USA"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="CRC"& Away=="MEX"))]<- 0
octagonal$homePoints[with(octagonal,which(Home=="CRC"& Away=="PAN"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="CRC"& Away=="JAM"))]<- 1
octagonal$homePoints[with(octagonal,which(Home=="CRC"& Away=="ESA"))]<- 3
octagonal$homePoints[with(octagonal,which(Home=="CRC"& Away=="HON"))]<- 3

octagonal$homePoints[with(octagonal,which(Home=="JAM"& Away=="CAN"))]<- 1
octagonal$homePoints[with(octagonal,which(Home=="JAM"& Away=="USA"))]<- 1
octagonal$homePoints[with(octagonal,which(Home=="JAM"& Away=="MEX"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="JAM"& Away=="PAN"))]<- 0
octagonal$homePoints[with(octagonal,which(Home=="JAM"& Away=="CRC"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="JAM"& Away=="ESA"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="JAM"& Away=="HON"))]<- -1

octagonal$homePoints[with(octagonal,which(Home=="ESA"& Away=="CAN"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="ESA"& Away=="USA"))]<- 1
octagonal$homePoints[with(octagonal,which(Home=="ESA"& Away=="MEX"))]<- 0
octagonal$homePoints[with(octagonal,which(Home=="ESA"& Away=="PAN"))]<- 3
octagonal$homePoints[with(octagonal,which(Home=="ESA"& Away=="CRC"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="ESA"& Away=="JAM"))]<- 1
octagonal$homePoints[with(octagonal,which(Home=="ESA"& Away=="HON"))]<- 1

octagonal$homePoints[with(octagonal,which(Home=="HON"& Away=="CAN"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="HON"& Away=="USA"))]<- 0
octagonal$homePoints[with(octagonal,which(Home=="HON"& Away=="MEX"))]<- -1
octagonal$homePoints[with(octagonal,which(Home=="HON"& Away=="PAN"))]<- 0
octagonal$homePoints[with(octagonal,which(Home=="HON"& Away=="CRC"))]<- 1
octagonal$homePoints[with(octagonal,which(Home=="HON"& Away=="JAM"))]<- 0
octagonal$homePoints[with(octagonal,which(Home=="HON"& Away=="ESA"))]<- -1

# Above just makes it easy for me to indicate how many points the home team got
# If the home team has a result, then the visiting team also gets one.
# If the match hasn't been played yet, then both teams have -1.


notPlayedYet<-with(octagonal,which(homePoints== -1 & awayPoints == -1))
octagonal$awayPoints[with(octagonal,which(homePoints==1))]<-1
octagonal$awayPoints[with(octagonal,which(homePoints==3))]<-0
octagonal$awayPoints[with(octagonal,which(homePoints==0))]<-3

homePoints<-octagonal[-notPlayedYet,] %>% group_by(Home) %>% summarize(points=sum(homePoints))
awayPoints<-octagonal[-notPlayedYet,] %>% group_by(Away) %>% summarize(points=sum(awayPoints))

final.table<-merge(homePoints,awayPoints,by.x="Home",by.y="Away")
final.table$total<-with(final.table,points.x+points.y)

# This function simulates results of the non-played matches.
simulate.table<-function(x,match.table){
	
	notPlayedYet<-with(match.table,which(homePoints== -1 & awayPoints == -1))
	# for these matches, randomly decide whathappens to the home teams
	results<-sample(c(0,1,3),length(notPlayedYet),replace=T)
	match.table$homePoints[notPlayedYet]<-results
	match.table$awayPoints[with(match.table,which(homePoints==1))]<-1
	match.table$awayPoints[with(match.table,which(homePoints==3))]<-0
	match.table$awayPoints[with(match.table,which(homePoints==0))]<-3
	homePoints<-match.table %>% group_by(Home) %>% summarize(points=sum(homePoints))
	awayPoints<-match.table %>% group_by(Away) %>% summarize(points=sum(awayPoints))

	final.table<-merge(homePoints,awayPoints,by.x="Home",by.y="Away")
	final.table$total<-with(final.table,points.x+points.y)
	colnames(final.table)[1]<-paste0("Sim",x)
	return(final.table[,(4)])
}




points.matrix<-do.call(cbind,future_lapply(1:n.sims,FUN=simulate.table,match.table=octagonal))
rownames(points.matrix)<-final.table$Home
team.rank<-colRanks(as.matrix(points.matrix),preserveShape=T)
rownames(team.rank)<-final.table$Home
rank.freq<-melt(team.rank)



convert.country<-c("CAN"="ca","USA"="us","CRC","cr","ELS"="sv","HON"="hn","PAN"="pa","MEX"="mx","JAM"="jm")
my.palette<-c("Fail"="#d95f02","Playoff"="#7570b3","Automatic"="#1b9e77" )

oda<-rank.freq
oda$qualified<- ifelse(oda$value>=4,"Automatic","Fail")
oda$qualified[which(oda$value==4)]<-"Playoff"

oda<-as.data.frame(oda %>% group_by(Var1, qualified) %>% summarize(prob.outcome= n() /ncol(team.rank)*100))
oda$iso2 <- tolower(countrycode(oda$Var1, "ioc", "iso2c"))
bb<-oda[which(oda$qualified=="Automatic"),]

p<-oda %>% ggplot(aes(x=factor(Var1,levels=levels(with(bb,reorder(Var1,prob.outcome)))),y=prob.outcome,fill=qualified ) )+
geom_bar(position="dodge",stat="identity")+
geom_text(aes(y=prob.outcome-0.5,label=paste0(round(prob.outcome,1),"%")),position =position_dodge(width=1),hjust = -0.2)+
ggflags::geom_flag(data=oda[!duplicated(oda$iso2),],y=-5,aes(x=Var1,country=tolower(iso2)), size=10)+
coord_flip()+
expand_limits(y= c(-5,105))+labs(y="Estimated Probability of Outcome",x="CONCACAF Team",subtitle=paste0(ncol(team.rank)," Permutations"))+scale_fill_manual(values = my.palette)+theme_Publication()

png("~/Documents/Concacaf.png",width=2000,height=2000,res=300)
print(p)
dev.off()
