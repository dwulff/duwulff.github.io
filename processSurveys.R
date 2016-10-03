options(stringsAsFactors = F)
require(googlesheets)

require(devtools);install_github('dwulff/translatoR');require(translatoR)
setwd('~/Dropbox (2.0)/Work/Software/dwulff.github.io/')

stops1 = read.table('~/Desktop/EnglishStopWord1.txt',sep='\n')[,1]
stops1 = unlist(sapply(stops1,strsplit,'\n'))
con = file('~/Desktop/EnglishStopWord2.txt')
stops2 = readLines(con);close(con)
stops2 = unlist(sapply(stops2,strsplit,'\t'))
stops3 = read.table('~/Desktop/GermanStopWord1.txt',sep='\t')[,2]
stops4 = read.table('~/Desktop/GermanStopWord2.txt',sep='\t')[,1]

stops = c(stops1,stops2,stops3,stops4)


getsyn = function(word){
  filter <- getTermFilter("ExactMatchFilter", word, TRUE)      
  terms <- getIndexTerms("NOUN", 1, filter)
  if(!is.null(terms)) getSynonyms(terms[[1]])
  }

goodchoices = read.table('_Goodchoices/GoodchoicesSurveys.txt',header=F,sep='\n')[,1]
networks    = read.table('_Networks/NetworksSurveys.txt',header=F,sep='\n')[,1]

tab = paste0(networks[1],' (Antworten)')

# networks
for(tab in networks){
  
  gs = gs_title(tab)
  d = gs_read(gs)
  d = as.data.frame(d)
  
  names = d[,3]
  
  comp = unlist(d[,4:10])
  crit = unlist(d[,11:13])
  
  comp.processed = c()
  for(iq in 1:length(comp)){
    print(iq)
    
    # process
    q = comp[iq]
    q = gsub('[[:punct:]]','',q)
    q = gsub('[[:digit:]]','',q)
    q = tolower(q)
    q = strsplit(q,' ')[[1]]
    q = q[!q %in% stops]
    
    # translate
    q = sapply(q,translatoR,'de','en')
    q = tolower(q)
    
    # get syns
    q = c(q,unlist(lapply(q,getsyn)))
    
    # bind
    q = paste(q,collapse=' ')
    
    # store
    comp.processed[iq] = q
    }
  
  
  
  }




