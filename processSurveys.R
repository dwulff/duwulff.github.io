options(stringsAsFactors = F)
require(googlesheets)

setwd('~/Dropbox (2.0)/Work/Software/dwulff.github.io/')

goodchoices = read.table('_Goodchoices/GoodchoicesSurveys.txt',header=F,sep='\n')[,1]
networks    = read.table('_Networks/NetworksSurveys.txt',header=F,sep='\n')[,1]

tab = paste0(networks[6],' (Antworten)')
tab = paste0(goodchoices[6],' (Antworten)')


gs = gs_title(tab)
d = gs_read(gs)
d = as.data.frame(d)

# networks
names = d[,3]

comp = unlist(d[,4:6])
crit = unlist(d[,7:9])

for(com in comp) cat(com,'\n\n')
for(cri in crit) cat(cri,'\n\n')

comp[grepl('die Wörter gelernt',comp)]
crit[grepl('die Wörter gelernt',crit)]
