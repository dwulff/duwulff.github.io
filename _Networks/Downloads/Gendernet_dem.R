# --------------------------------------------------------------------
#      READ AND DECRYPT
# --------------------------------------------------------------------

PASSWORD = 'networksarecool' # networks are what

# ---- load packages
if(!require(devtools)) install.packages('devtools') ; require(devtools)
if(!require(sodium)) install.packages('sodium') ; require(sodium)
if(!require(igraph)) install.packages('igraph') ; require(igraph)
if(!require(cryptR)) install_git('https://github.com/dwulff/cryptR') ; require(cryptR)

# ---- read and decryp data
load(url('https://raw.githubusercontent.com/dwulff/dwulff.github.io/master/_Networks/Downloads/Genderasso.RData'),verbose=T)
data = decrypt(data,PASSWORD)
str(data)


# --------------------------------------------------------------------
#      CREATE EDGE LIST
# --------------------------------------------------------------------

# create edges
edg_m = c()
for(i in 1:nrow(d)){
  e1 = cbind('mann',unlist(d[i,paste('v',1:5,sep='_')]))
  e2 = cbind(d[i,paste('v',1,sep='_')],unlist(d[i,paste('v',6:10,sep='_')]))
  e3 = cbind(d[i,paste('v',2,sep='_')],unlist(d[i,paste('v',11:15,sep='_')]))
  e4 = cbind(d[i,paste('v',3,sep='_')],unlist(d[i,paste('v',16:20,sep='_')]))
  e5 = cbind(d[i,paste('v',4,sep='_')],unlist(d[i,paste('v',21:25,sep='_')]))
  e6 = cbind(d[i,paste('v',5,sep='_')],unlist(d[i,paste('v',26:30,sep='_')]))
  e  = rbind(e1,e2,e3,e4,e5,e6)
  edg_m = rbind(edg_m,data.frame(e))
}

tab_m = table(c(rep('mann',5 * nrow(d)),edg_m[,2]))
edg_m$cnt = tab_m[edg_m[,2]]
m = list(edg_m,tab_m)

# create edges
edg_f = c()
for(i in 1:nrow(d)){
  e1 = cbind('frau',unlist(d[i,paste('v',(1:5)+30,sep='_')]))
  e2 = cbind(d[i,paste('v',1+30,sep='_')],unlist(d[i,paste('v',(6:10)+30,sep='_')]))
  e3 = cbind(d[i,paste('v',2+30,sep='_')],unlist(d[i,paste('v',(11:15)+30,sep='_')]))
  e4 = cbind(d[i,paste('v',3+30,sep='_')],unlist(d[i,paste('v',(16:20)+30,sep='_')]))
  e5 = cbind(d[i,paste('v',4+30,sep='_')],unlist(d[i,paste('v',(21:25)+30,sep='_')]))
  e6 = cbind(d[i,paste('v',5+30,sep='_')],unlist(d[i,paste('v',(26:30)+30,sep='_')]))
  e  = rbind(e1,e2,e3,e4,e5,e6)
  edg_f = rbind(edg_f,data.frame(e))
}

tab_f = table(c(rep('frau',5 * nrow(d)),edg_f[,2]))
edg_f$cnt = tab_f[edg_f[,2]]
f = list(edg_f,tab_f)


# --------------------------------------------------------------------
#      PLOT
# --------------------------------------------------------------------

require(igraph)

network_plot = function(edges,min,col,nodesize=.4,edgesize=.3,category=1){
  edg = edges[[1]]
  cnt = edges[[2]]
  g = graph_from_data_frame(subset(edg,cnt>=min),directed=F)
  v = get.vertex.attribute(g)$name
  #l = layout_with_fr(g)
  l = layout_with_dh(g)
  #l = layout_with_graphopt(g)
  plot.new();plot.window(c(min(l[,1]),max(l[,1])),c(min(l[,2]),max(l[,2])))
  sapply(1:nrow(edg),function(x){
    is = c(which(v==edg[x,1]),which(v==edg[x,2]))
    lines(c(l[is[1],1],l[is[2],1]),c(l[is[1],2],l[is[2],2]),lwd=edg[x,3]**edgesize-.3,lty=1,col='grey70')
  })
  for(i in 1:length(v)){
    points(l[i,1],l[i,2],cex=cnt[v[i]]**nodesize,pch=16,col='grey70')
    #points(l[i,1],l[i,2],cex=cnt[v[i]]**nodesize,pch=16,col=col)
  }
  for(i in 1:length(v)){
    text(l[i,1],l[i,2],labels=v[i],cex=cnt[v[i]]**.2-.4)
  }
}

network_plot(m,2,col)
network_plot(f,2,col)


# --------------------------------------------------------------------
#      STATS
# --------------------------------------------------------------------

g_m = graph_from_data_frame(subset(edg_m,cnt>=2),directed=F)
g_f = graph_from_data_frame(subset(edg_f,cnt>=2),directed=F)

transitivity(g_m,type = 'localaverage') 
transitivity(g_f,type = 'localaverage') 

length(E(g_m)) / length(V(g_m))
length(E(g_f)) / length(V(g_f))

length(unique(edg_m[,2]))
length(unique(edg_f[,2]))


### degree distribution

col1 = rgb(224,238,212,maxColorValue = 255)
col2 = rgb(220,207,232,maxColorValue = 255)

deg_m = degree_distribution(g_m)
deg_m_p = which(deg_m > 0) - 1
plot(deg_m_p,deg_m[deg_m!=0],type='b',xlim=c(0,60),ylab='Relative frequency',xlab='Degree',ylim=c(0,.5),col=col1,lwd=3,las=1)

deg_f = degree_distribution(g_f)
deg_f_p = which(deg_f > 0) - 1
lines(deg_f_p,deg_f[deg_f!=0],type='b',col=col2,lwd=3)


# --------------------------------------------------------------------
#      CONTENT
# --------------------------------------------------------------------


#### hubs

tab_m_m = table(subset(edg_m,edg_m[,1]=='mann')[,2])
tab_m_m = names(tab_m_m)[order(tab_m_m,decreasing=T)]

tab_f_f = table(subset(edg_f,edg_f[,1]=='frau')[,2])
tab_f_f = names(tab_f_f)[order(tab_f_f,decreasing=T)]

cat('male hubs: ',paste0(tab_m_m[1:5],collapse=' '),'\n',sep='')
cat('female hubs: ',paste0(tab_f_f[1:5],collapse=' '),'\n',sep='')

#### page rank

V(g_m)[which(rank(-page_rank(g_m)[[1]]) %in% 2:6)]
V(g_f)[which(rank(-page_rank(g_m)[[1]]) %in% 2:6)]


### clustering

cl_m = cluster_walktrap(g_m)$membership
cl_f = cluster_walktrap(g_f)$membership

for(i in 1:max(cl_m)) cat('cluster ',i,': ',paste0(names(V(g_m)[cl_m == i]),collapse=' '),'\n',sep='')
for(i in 1:max(cl_f)) cat('cluster ',i,': ',paste0(names(V(g_f)[cl_f == i]),collapse=' '),'\n',sep='')

cl_m = cluster_louvain(g_m)$membership
cl_f = cluster_louvain(g_f)$membership

for(i in 1:max(cl_m)) cat('cluster ',i,': ',paste0(names(V(g_m)[cl_m == i]),collapse=' '),'\n',sep='')
for(i in 1:max(cl_f)) cat('cluster ',i,': ',paste0(names(V(g_f)[cl_f == i]),collapse=' '),'\n',sep='')






