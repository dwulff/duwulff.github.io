######### Psychonet demonstration
#
#

PASSWORD = ''

# ---- load packages
if(!require(igraph)) install.packages('igraph') ; require(devtools)
if(!require(devtools)) install.packages('devtools') ; require(igraph)
if(!require(cryptR)) install_git('https://github.com/dwulff/cryptR') ; require(cryptR)

package?igraph

# ---- read and decryp data
load(url('https://raw.githubusercontent.com/dwulff/dwulff.github.io/master/_Networks/Downloads/psychonet.RData'))
data = decrypt(data,PASSWORD)

# ---- create graph

g = graph_from_data_frame(data$edg,directed=F)
plot(g)


# ---- plot function

network_plot = function(edges,col,cnt,nodesize=.7,edgesize=2,category=1){
  g = graph_from_data_frame(edges,directed=F)
  v = get.vertex.attribute(g)$name
  #l = layout_with_fr(g)
  l = layout_with_dh(g)
  plot.new();plot.window(c(min(l[,1]),max(l[,1])),c(min(l[,2]),max(l[,2])))
  sapply(1:nrow(edges),function(x){
    is = c(which(v==edges[x,1]),which(v==edges[x,2]))
    lines(c(l[is[1],1],l[is[2],1]),c(l[is[1],2],l[is[2],2]),lwd=edges[x,3]**edgesize-.3,lty=1,col='grey50')
    })
  for(i in 1:length(v)){
    points(l[i,1]+.04,l[i,2]-.04,cex=cnt[v[i]]**nodesize,pch=16,col='grey50')
    points(l[i,1],l[i,2],cex=cnt[v[i]]**nodesize,pch=16,col=col[category[i]])
    }
  for(i in 1:length(v)){
    text(l[i,1],l[i,2],labels=v[i],cex=cnt[v[i]]**.08-.4)
    }
  }

col1 = rgb(224,238,212,maxColorValue = 255)
col2 = rgb(220,207,232,maxColorValue = 255)

network_plot(data$edg,col=c(col1,col2),data$cnt,1.1,category = (data$gen=='w')+1)


# ----- small-world-ness

l = average.path.length(g)
c = transitivity(g,type = 'localaverage')

l_c = log(length(V(g))) / log(mean(degree(g)))
c_c = mean(degree(g)) / length(V(g))

(c / c_c) / (l / l_c)


# ----- centrality

g = graph_from_data_frame(data$edg,directed=F)

deg = centr_degree(g) ; V(g)[deg$res == max(deg$res)] ; hist(deg$res,breaks=20,main='Degree distribution',xlab='Degree')
clo = centr_clo(g) ; V(g)[clo$res == max(clo$res)] ; hist(clo$res,breaks=20,main='Degree distribution',xlab='Degree')
bet = centr_betw(g) ; V(g)[bet$res == max(bet$res)]

g = graph_from_data_frame(data$edg,directed=F)
E(g)$weight = data$edg$frq
deg_w = sapply(V(g),degree,graph=g) ; deg_w[deg_w == max(deg_w)]
clo_w = sapply(V(g),closeness,graph=g) ; clo_w[clo_w == max(clo_w)]
bet_w = sapply(V(g),betweenness,graph=g) ; bet_w[bet_w == max(bet_w)]


# ----- modularity


g = graph_from_data_frame(data$edg,directed=F)

cl = cluster_louvain(g)$membership

col = c("#69D2E7", "#A7DBD8", "#E0E4CC", "#E9B57E", "#F38630", "#FA6900")
network_plot(data$edg,col=col,data$cnt,1.1,category = cl)







