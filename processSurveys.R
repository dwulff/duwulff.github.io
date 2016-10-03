options(stringsAsFactors = F)
require(googlesheets)

require(devtools);install_github('dwulff/translatoR');require(translatoR)
setwd('~/Dropbox (2.0)/Work/Software/dwulff.github.io/')


## Intro survey




goodchoices = read.table('_Goodchoices/GoodchoicesSurveys.txt',header=F,sep='\n')[,1]
networks    = read.table('_Networks/NetworksSurveys.txt',header=F,sep='\n')[,1]

tab = paste0(networks[1],' (Antworten)')



gs = gs_title(tab)
d = gs_read(gs)
d = as.data.frame(d)

# networks
names = d[,3]
  
comp = unlist(d[,4:10])
crit = unlist(d[,11:13])
  
for(com in comp) cat(com,'\n\n')
for(cri in crit) cat(cri,'\n\n')
