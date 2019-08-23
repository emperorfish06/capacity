
library(ggthemes)
library(egg)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(rworldmap)

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

#tiff("C:\\Clock\\fig1.tif", units="in", width=7, height=8, res=300)
ggarrange(x, y, ncol=2)

#Figure 2
fig2        = read.csv(file="C:\\Clock\\fig2.csv")
labs        = c("Allocative Efficiency", "Revenue Efficiency", "Technical Efficiency")
names(labs) = c("Alloc.Eff","Rev.Eff","Tech.Eff")

ggplot(subset(fig2,  variable %in% c("Tech.Eff","Alloc.Eff","Rev.Eff")), aes(x = year, y = M,group=sector, fill=sector))+
  geom_ribbon(aes(ymin = M - SD, ymax = M + SD), alpha = 0.2)+
  facet_grid(rname ~ variable,  scales = "free", labeller = labeller(variable=labs))  +
  geom_line(size=1, aes(colour = sector)) + 
  theme_bw() + coord_cartesian(ylim = c(0.2,1)) + 
  expand_limits(y = 0.2) +
  theme_cleveland() + 
  theme(strip.background =  element_blank(),
        legend.position  = "bottom",
        panel.border = element_rect(colour = "black")) +  
  scale_fill_discrete(name = "", labels = c("Artisanal", "Industrial")) + 
  guides(color = FALSE, size = FALSE) 




#Figure 3 bubbles
fig3        = read.csv(file="C:\\Clock\\fig3.csv")
#gdpindex is logged.

ggplot(fig3, aes(Tech.Eff, Alloc.Eff, size = score, fill = gdpIndex)) +
  geom_jitter(shape = 21, colour = "gray80") +
  scale_fill_distiller(name = "GDP index", palette = "RdYlBu", direction = 1, labels = c("Low","High"), limits = c(5,12), breaks = c(5,12)) +
  scale_radius(range = c(1, 8))+
  facet_grid(era ~ rname)+ expand_limits(y = 0)+ 
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

#figure4
fig4a = read.csv(file="C:\\Clock\\fig4a.csv")
fig4b = read.csv(file="C:\\Clock\\fig4b.csv")
cbbPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
a=ggplot(subset(fig4a, !(rname=="High Seas")), aes(iyear,mnshn, colour=rname))+
  #geom_point(size=3)+
  geom_line(size=2)+
  theme_bw()+
  theme( text = element_text(size=16),legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_colour_manual(values=cbbPalette)+
  xlab("year")+
  ylab("Shannon Wiener diversity index")
c=tag_facet(a)

b=ggplot(fig4b, aes(year,log(totrev), colour=rname))+
  #geom_point(size=3)+
  geom_line(size=2)+
  scale_colour_manual(values=cbbPalette)+
  theme_bw()+
  theme( text = element_text(size=16),legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylab("log(total revenue $US)")

d=tag_facet(b,tag_pool=c("b"))
ggpubr::ggarrange(c,d,common.legend = TRUE)



#figure 5
fig5        = read.csv(file="C:\\Clock\\fig5.csv")
p=ggplot(subset(fig5, era==2010),aes(pctae, pctkw, size=score,colour = score)) +
  
  geom_jitter( stroke = 4) +
  geom_hline(yintercept=0,linetype="dotted")+
  geom_vline(xintercept=0,linetype="dotted")+
  
  #geom_smooth(aes(group=1),method='lm',formula=y~x, se=FALSE, size=0.5,colour="black",show.legend = FALSE) +
  scale_colour_distiller(name = "vulnerability", palette ="RdYlBu", direction = 1,  limits=c(0.5,1), breaks=c(c(0.5,1))) +
  #scale_y_continuous(breaks=seq(0,12, by=2)) +
  geom_text_repel(aes(label=un.name), size=3, color = 'black',
                  box.padding = unit(0.3, "lines"),
                  point.padding = unit(0.3, "lines"),
                  segment.color = 'transparent') + 
  facet_wrap(~rname, ncol=1, scales="free_y")+
  scale_radius(range = c(1, 7)) +
  theme_bw()+
  theme(plot.title = element_text( size=14), # use theme_get() to see available options
        axis.title.x = element_text( size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        #panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position="bottom", 
        strip.background = element_blank(),
        strip.text.y = element_text(size = 14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.box = "vertical",
        strip.text.x = element_text(size = 14))+
  guides(size=FALSE)+
  labs( y="$US per kW % change", x="Allocative Efficiency % change")
tag_facet(p)