options(stringsAsFactors = F)
require(googlesheets)
require(tm)
require(lsa)
require(translate)
setwd('~/Dropbox (2.0)/Work/Software/dwulff.github.io/')

goodchoices = read.table('_Goodchoices/GoodchoicesSurveys.txt',header=F,sep='\n')[,1]
networks    = read.table('_Networks/NetworksSurveys.txt',header=F,sep='\n')[,1]

translate('hello world','en','de')

tab = paste0(networks[1],' (Antworten)')

# networks
for(tab in networks){
  
  gs = gs_title(tab)
  d = gs_read(gs)
  d = as.data.frame(d)
 
  names = d[,3]
  
  comp = unlist(d[,4:10])
  crit = unlist(d[,11:13])
   
  corpus = Corpus(VectorSource(comp))
  corpus = tm_map(corpus, tolower)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, function(x) removeWords(x, c('1\t')))
  corpus = tm_map(corpus, function(x) removeWords(x, stopwords('german')))
  corpus = tm_map(corpus, stemDocument, language = 'german')

  
  }
