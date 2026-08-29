## Phylogeny for FAYCAL

## Choose your libraries

library(ggtree)
library(phangorn)
library(ggplot2)
library(ape)
library(openxlsx)
library(cowplot)

## Read consensus tree output from IQTREE-2
## This contains bootstrap values for each node in the tree.

# for E.coli
tr <- read.tree("~/faycal/phylo/ecoli_achtman_4/ecoli2.fasta.contree")
# for Klebsiella
tr <- read.tree("~/faycal/phylo/klebsiella/klebsiella2.fasta.contree")
# for Enterobacter
tr <- read.tree("~/faycal/phylo/ecloacae/enterobacter2.fasta.contree")

## Reda metadata file

md <- read.xlsx("~/faycal/phylo/reunion_isolates_table.xlsx")
head(md)

## Check that the names in the metadata file, and the tree match
## number of isolates in the tree
length(tr$tip.label)
length(intersect(tr$tip.label,md$ID)) 

## If these values are not the same, then the labels do not match up between the tree and the table
## find the missing values and inspect the table to see if this is just a naming issue.

if(length(tr$tip.label) != length(intersect(tr$tip.label,md$ID))){
  setdiff(tr$tip.label,md$ID)
}

# View(md)


## Once your metadata is sorted, plot your tree.

## First - we'll perform a "midpoint rerooting".
## This is typically visually more pleasing, and easier to interpret than the unrooted output from IQTREE-2

tr=midpoint(tr)
tr_fig=ggtree(tr)

## naked tree
tr_fig

## tree + tip label
tr_fig + geom_tiplab()

## find tree axes scale
mx=max(tr_fig@data$x)

## Adjust tree labels
tr_fig + geom_tiplab(size=3,align=T) +
  xlim(0,mx*2)

## Attach metadata to tree and add colour points per host
tr_fig %<+% md +
  geom_tiplab(aes(label=paste0(host," :: ST", ST)),size=3,align=T)+
  geom_tippoint(aes(colour=host)) +
  xlim(0,mx*2)

## Change the colours, add a title
tr_fig %<+% md +
  geom_tiplab(aes(label=paste0(host," :: ", ST,study)),size=3,align=T)+
  geom_tippoint(aes(colour=host)) +
  xlim(0,mx*2)+
  scale_colour_brewer(palette="Paired")+
  ggtitle("ESBL E.coli isolated in La Réunion")

## Inspect bootstrap support
tr_fig %<+% md +
  geom_tiplab(aes(label=paste0(host," :: ", ST,study)),size=3,align=T)+
  geom_tippoint(aes(colour=host)) +
  geom_nodelab()+
  xlim(0,mx*2)+
  scale_colour_brewer(palette="Paired")+
  ggtitle("ESBL E.coli isolated in La Réunion")

## Show bootstrap support
tr_fig %<+% md +
  geom_tiplab(aes(label=paste0(host," :: ", ST,study)),size=3,align=T)+
  geom_tippoint(aes(colour=host)) +
  geom_nodepoint(aes(shape=as.numeric(label)>90),size=2)+
  xlim(0,mx*2)+
  scale_colour_brewer(palette="Paired")+
  ggtitle("ESBL E.coli isolated in La Réunion")+
  scale_shape_manual(values = c(NA,20))


## Once you have the figure the way you want it, write it to a variable
a = tr_fig %<+% md +
  geom_tiplab(aes(label=paste0(host," :: ST", ST," ",study)),size=3,align=T)+
  geom_tiplab(aes(label=paste0("(",y,")")),size=3,align=T,offset=0.003,linetype=NA)+
  geom_tippoint(aes(colour=host)) +
  xlim(0,mx*1.3)+
  scale_colour_brewer(palette="Paired")+
  theme(legend.position='top', legend.justification='left', legend.direction='horizontal')+
  geom_nodepoint(aes(shape=as.numeric(label)>90),size=2)+
  scale_shape_manual(values = c(NA,20))
a
#### Plot SNP distances

## In terminal, use "snp-dists" to create a distance matrix from your alignment:
#  snp-dists -b alignment.fasta > dists.tab

# read in distance table
# For E.coli
d1=read.table("~/faycal/phylo/ecoli_achtman_4/dists.tab", as.is=T, check.names = F)
# For Klebsiella
d1=read.table("~/faycal/phylo/klebsiella/dists.tab", as.is=T, check.names = F)
# For Enterobacter
d1=read.table("~/faycal/phylo/ecloacae/dists.tab", as.is=T, check.names = F)

# convert to long form
ds1=as.data.frame(as.table(as.matrix(d1)))
colnames(ds1)=c("from","to","SNPs")
ds1$from <- as.character(ds1$from)
ds1$to <- as.character(ds1$to)

# Define a function that extracts tip labels from a tree in the order they appear in the tree
get_tips=function(tree=NULL){
  d = fortify(tree)
  d = subset(d, isTip)
  tips=rev(with(d, label[order(y, decreasing=T)]))
  return(tips)}

# Use this functioon to establish the order of the tiles, so that it follows that of the tree
ds1$from=factor(ds1$from,levels=get_tips(tr))
ds1$to=factor(ds1$to,levels=get_tips(tr))

# Make SNP distances categorical (between X and Y SNPs)
ds1$dist=cut(ds1$SNPs,c(-1,0,10,50,100,1000,10000,100000),include.lowest = T,)

# Keep only the bottom half of the distance matrix
swap_idx <- as.integer(ds1$from) > as.integer(ds1$to)
temp_from <- ds1$from[swap_idx]
ds1$from[swap_idx] <- ds1$to[swap_idx]
ds1$to[swap_idx] <- temp_from
ds1 <- ds1[!duplicated(ds1[, c("from", "to")]), ]

ds1$from=factor(ds1$from,levels=get_tips(tr))
ds1$to=factor(ds1$to,levels=get_tips(tr))

d=ggplot(ds1)+
  geom_tile(aes(x=from,y=to,fill=dist),colour="black")+
  #scale_fill_manual(values = c("red","orange","yellow","lightyellow","grey90","grey70","grey50","grey20","black"),
  #                  labels=c("0 (identical)","1-10","10-20","20-30","30-40","40-50","50-100","100-1000",">10000"),name="SNPs")+
  scale_fill_manual(values = c("red","orange","yellow","lightyellow","grey90","grey70","grey50","grey20","black"),
                    labels=c("0 (identical)","1-10","10-50","50-100","100-1000","1000-10000","10000-100000",">100000"),name="SNPs")+
  #geom_text(aes(x=from,y=to,label=SNPs),size=1)+
  coord_equal()+
  theme_void()+
  scale_x_discrete(position = "top", 
                   labels = function(x) paste0(as.integer(factor(x, levels = levels(ds1$from))),"  ")) +
  scale_y_discrete(position = "left", 
                   labels = function(x) paste0(as.integer(factor(x, levels = rev(levels(ds1$from)))),"  ")) +
  theme(axis.text.y=element_text(size=6,hjust=1,vjust=0.5),
        axis.text.x.top=element_text(size=6,angle=0,hjust=1,vjust=0.5),
        axis.title = element_blank(),
        legend.position = "right")
d


cowplot::plot_grid(a,d,ncol=2,align="hv")

