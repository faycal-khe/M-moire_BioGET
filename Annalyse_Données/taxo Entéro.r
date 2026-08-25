#------
#package
#------
library(openxlsx)
library(ggplot2)
library(ggtree)
library(ape)
library(aplot)
#------
#fonction
#------
get_tips=function(tree=NULL){
  d = fortify(tree)
  d = subset(d, isTip)
  tips=rev(with(d, label[order(y, decreasing=T)]))
  return(tips)
}

#------
#code
#------
ani=read.table("/Users/faycalkhetim/Desktop/STAGE/Annalyse Données/tabs/ANI.tab")
head(ani)

colnames(ani)=c("from","to","ANI","hits","comparisons")
ani$group=cut(ani$ANI,c(0,90,95,98,100))
ani$from=gsub(".fasta","",ani$from)
ani$to=gsub(".fasta","",ani$to)

md=read.xlsx("/Users/faycalkhetim/Desktop/STAGE/Annalyse Données/tabs/metadata.xlsx")

ani$from2=md$name[match(ani$from,md$accession)]
ani$to2=md$name[match(ani$to,md$accession)]

dist=as.dist(xtabs(100-ANI ~ from2+to2,ani))
hc=hclust(dist)
ani$from2=factor(ani$from2,hc$labels[hc$order])
ani$to2=factor(ani$to2,hc$labels[hc$order])

phy=as.phylo(hc)
phy=hc
tr=ggtree(phy)
#tr2<-tr + geom_tiplab(,align=T,hjust =1)+xlim(-15, 20)
#tr2

#ani2<-ggplot(ani)+
  #subset(ani, as.numeric(from2) <= as.numeric(to2))+
  #geom_tile(aes(x=from2,y=to2,fill=group),colour="grey50")+
  #coord_equal()+
  #scale_fill_manual(values=c("grey90","lightyellow","lightblue","green"),labels=c("<90%","90-95%","95-98%",">98%"),name="Average Nucleotide Identity")+
  #theme(axis.text.x=element_text(angle=-45,hjust=0,vjust=0.5))
#tr2
#ani2



# Ordre des tips et table de correspondance numéro <-> nom
tip_order <- get_tips(phy)
id_df <- data.frame(label = tip_order, id = seq_along(tip_order))
id_lookup <- setNames(id_df$id, id_df$label)
tr2 <- tr %<+% id_df +
  geom_tiplab(aes(label = paste0(label,":",id)), size = 2.7,align = TRUE, linetype = "dotted",hjust=0) +
  hexpand(1)
tr2

#tr + ggtree::geom_tiplab(align=TRUE) +xlim(0,20)
ani$from2 <- factor(ani$from2, levels = get_tips(phy))# met les axes dans le bon ordre
ani$to2   <- factor(ani$to2,   levels = get_tips(phy))#

# 4. Heatmap : pas de texte en y (fourni par tr2), numéros en x en haut
ani2 <- ggplot(ani) +
  subset(ani, as.numeric(from2) <= as.numeric(to2))+ #avoir que la moitier haute
  geom_tile(aes(x = from2, y = to2, fill = group), colour = "grey50") +
  coord_equal() +
  scale_fill_manual(
    values = c("grey90", "lightyellow", "lightblue", "green"),
    labels = c("<90%", "90-95%", "95-98%", ">98%"),
    name = "Average Nucleotide Identity"
  ) +
  scale_x_discrete(position = "top", labels = function(x) id_lookup[x]) +
  theme(
    axis.text.x = element_text(size = 7),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA) #couleur du fond hahahaa
  )
tr2+ani2


