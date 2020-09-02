
library(ggthemes)
library(egg)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(rworldmap)
library(Benchmarking)

fig1abc = read.csv(file="C:\\Clock\\fig1abc.csv")
fig1def = read.csv(file="C:\\Clock\\fig1def.csv")
#Code for Figure 1 - 
mapworld = fortify(spTransform(getMap(), CRS("+proj=wintri")))
#create map boundary
ll.to.wt = function (points)
  as.data.frame(spTransform(SpatialPoints(points, CRS("+proj=longlat")),
                            CRS("+proj=wintri")))

lseq     = seq(-85, 85, by=.25)
boundary = ll.to.wt(data.frame(
  long     = c(rep(-180, length(lseq)), rep(180, length(lseq)), -180),
  lat      = c(lseq, rev(lseq),lseq[1])))

fig1abc$variable = factor(fig1abc$variable, levels = c("Tech.Eff","Alloc.Eff", "Rev.Eff"))
png("C:\\Clock\\fig1.png", units="in", width=7, height=8, res=300)
p=ggplot()+ 
  geom_polygon(data=boundary, aes(x=long, y=lat), fill="azure") +
  geom_map(data=mapworld, map=mapworld, aes(map_id=id, x=long, y=lat), fill ="gray93", colour=NA)+
  geom_map(data=fig1abc, map=mapworld, aes(map_id=id, x=long, y=lat,fill=M),color = NA)+ 
  scale_fill_distiller(palette ="Spectral", direction = 1, limits=c(0,1), breaks=c(c(0,1))) + # or direction=1
  theme_map()+
  theme(legend.key = element_blank(), 
        strip.background = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size=12,face="bold"))+
  facet_wrap(~variable, scales = "free", ncol=1)
x=tag_facet(p)


q=ggplot()+ 
  geom_polygon(data=boundary, aes(x=long, y=lat), fill="azure") +
  geom_map(data=mapworld, map=mapworld, aes(map_id=id, x=long, y=lat), fill ="gray93", colour=NA)+
  geom_point(data=fig1def, aes(x=long, y=lat, size=M, color=M), alpha=0.9) +
  geom_text_repel(data=fig1def,aes(x=long, y=lat, label=un.name), size=2.5, color = 'black',
                  box.padding = unit(0.5, "lines"),
                  point.padding = unit(0.5, "lines"),
                  segment.color = 'grey50') + 
  scale_radius(range = c(1, 2)) +
  scale_colour_distiller(palette ="Spectral", direction = 1, limits=c(0,1), breaks=c(c(0,1))) + # or direction=1
  theme_map()+
  theme(legend.key = element_blank(), 
        strip.background = element_blank(), 
        legend.position = "none",
        text = element_text(size=12,face="bold"))+
  facet_wrap(~variable, scales = "free", ncol=1)+scale_shape(guide=FALSE)+scale_size(guide=FALSE)
y=tag_facet(q, tag_pool = c("d","e","f"))


ggarrange(x, y, common.legend = TRUE,ncol=2,legend = c("bottom"))
dev.off()
#Figure 2
fig2        = read.csv(file="C:\\Clock\\fig2.csv")
labs        = c("Technical Efficiency","Allocative Efficiency", "Revenue Efficiency" )
names(labs) = c("Tech.Eff","Alloc.Eff","Rev.Eff")
fig2$variable = factor(as.character(fig2$variable), c("Tech.Eff","Alloc.Eff","Rev.Eff"))

png("C:\\Clock\\fig2.png", units="in", width=7, height=8, res=300)
ggplot(subset(fig2,  variable %in% c("Tech.Eff","Alloc.Eff","Rev.Eff")), aes(x = year, y = M,group=sector, fill=sector))+
  geom_ribbon(aes(ymin = M - SD, ymax = M + SD), alpha = 0.2)+
  facet_grid(rname ~ variable,  scales = "free", labeller = labeller(variable=labs))  +
  geom_line(size=1, aes(colour = sector)) + 
  theme_bw() + coord_cartesian(ylim = c(0,1)) + 
  expand_limits(y = 0) +
  theme_cleveland() + 
  theme(strip.background =  element_blank(),
        legend.position  = "bottom",
        panel.border = element_rect(colour = "black")) +  
  scale_fill_discrete(name = "", labels = c("Artisanal", "Industrial")) + 
  guides(color = FALSE, size = FALSE) + stat_cor(aes(color = sector), size=3.2, label.x.npc = 0.3, label.y.npc = 0.28, hjust = 0,  method="spearman")


dev.off()


#Figure 3 bubbles
fig3        = read.csv(file="C:\\Clock\\fig3.csv")
fig3$eraf = factor(fig3$era, levels=c('2010','2000','1990','1980','1970','1960'))
#gdpindex is logged.
png("C:\\Clock\\fig3.png", units="in", width=14, height=9, res=300)
ggplot(fig3, aes(Tech.Eff, Alloc.Eff, size = score, fill = gdpIndex)) +
  geom_jitter(shape = 21, colour = "gray80") +
  scale_fill_distiller(name = "GDP index", palette = "RdYlBu", direction = 1, labels = c("Low","High"), limits = c(5,12), breaks = c(5,12)) +
  scale_radius(range = c(1, 8))+
  facet_grid(eraf ~ rname)+ expand_limits(y = 0)+ 
  theme_bw()+
  theme(plot.title = element_text( size=10), 
        axis.title.x = element_text( size=10),
        axis.title.y = element_text(size=10, angle=90),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position="bottom", 
        strip.background = element_blank(),
        strip.text.y = element_text(size = 10),
        legend.text  = element_text(size=10),
        legend.title = element_text(size=10),
        strip.text.x = element_text(size = 10))+
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))+
  labs(size="Vulnerability") + ylab("Allocative Efficiency") + xlab("Technical Efficiency")
dev.off()

#figure4

png("C:\\Clock\\fig5.png", units="in", width=8, height=5, res=300)
fig5=read.csv("C:\\Clock\\1st_paper\\New folder\\fig5.csv",sep=",")
dea.plot(fig5$Total_EffEffort_GearSpecies/1000,fig5$Total_Catch_GearSpecies/1000,RTS="vrs",ORIENTATION="in-out",xlab="Effort (x 1000)",ylab="Catch (x 1000)",pch=16)
dea.plot(fig5$Total_EffEffort_GearSpecies/1000,fig5$Total_Catch_GearSpecies/1000,RTS="crs",ORIENTATION="in-out",add=T,lty="dotted",col="blue", lwd=4)

dev.off()
#figure 5
fig6        = read.csv("C:\\Clock\\1st_paper\\New folder\\fig6.csv")
png("C:\\Clock\\fig6.png", units="in", width=7, height=8, res=300)
#write.csv(fig5,"C:\\Clock\\1st_paper\\New folder\\fig5.csv")
ggplot(subset(fig6, !(variable %in% c("TEvrs","TEcrs"))), aes(year, value, group=variable, colour=variable))+geom_line(size=1.5)+theme_bw()+
  theme(plot.title = element_text( size=14), 
        axis.title.x = element_text( size=14),
        axis.title.y = element_text(size=14, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position="bottom", 
        strip.background = element_blank(),
        strip.text.y = element_text(size = 14),
        legend.text  = element_text(size=14),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 14)) + 
  ylab("Technical efficiency Peru Industrial")
dev.off()