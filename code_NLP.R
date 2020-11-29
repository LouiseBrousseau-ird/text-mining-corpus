#L. Brousseau, 2020-11-20

# ----- CONFIG

# paths and files
dir="" #path to working diectory
corpus_dir="./corpus author" # path to author directory containing author's articles (within working dir)
list.files(path = corpus_dir)

file_number=3 # Manual selection of file to process

# parameters
occurence_threshold=5 # number of occurence of a word in the text
cooccurence_threshold=3 # number of occurence of keywords in the text

# set wd and source functions
setwd(dir)
source("code_NLP.function")

# ----- LIBRARIES

library(NLP)
library(tm)
library(openNLP)
library(graph)
library(RKEA)

library(igraph)
library(networkD3)

# ----- MAIN CODE

file=paste0(corpus_dir,"/",list.files(path = corpus_dir)[file_number])

## Text processing

article = scan(file,what="raw",sep="\n")

title=article[grep("\\TITLE",article)+1]
abstract=article[grep("\\ABSTRACT",article)+1]
keywords=article[grep("\\KEYWORDS",article)+1]

title=gsub("\\."," ",title)
title=paste0(title,".")

text=article[seq((grep("\\TEXT",article)+1),length(article),1)]
text=paste(text,sep="",collapse=" ")
text=gsub( " *\\(.*?\\) *", "", text) # remove content within parentheses
text=gsub("\\.",". ",text)
text=gsub("  "," ",text)
text=gsub("  "," ",text)
text=tolower(text)

keywords=tolower(keywords)
keywords=removeWords(keywords,stopwords("english"))
keywords=stripWhitespace(keywords)
if(is.na(keywords)){keywords=""}

title_abstract_text=paste(title,abstract,text, sep=" ",collapse=" ")
title_abstract_text=gsub( " *\\(.*?\\) *", "", title_abstract_text) # remove content within parentheses
doc=title_abstract_text
doc=gsub("  "," ",doc)
doc=gsub("  "," ",doc)

## Keywords extraction (based on words occurence)

corp = Corpus(VectorSource(doc))
corp = tm_map(corp, stripWhitespace)
corp = tm_map(corp, tolower)
corp = tm_map(corp, removeWords, stopwords("english"))
corp = tm_map(corp, removePunctuation)
corp = tm_map(corp, stripWhitespace)

words = SplitText(as.character(corp[[1]]))
tagged_text = tagPOS(corp[[1]])

tagged_words = SplitText(as.character(tagged_text))
tagged_words = c(SelectTaggedWords(tagged_words,"/NN"),SelectTaggedWords(tagged_words,"/JJ"))  # keep only NN & JJ tagged words 

tagged_words = RemoveTags(tagged_words)                                                        # remove un-used tag POS

table = table(tagged_words)

table = table[-which(table<occurence_threshold)] 

keywords.2 = unlist(strsplit(keywords,split=", "))
keywords.2 = unlist(strsplit(keywords.2,split=" "))
keywords.2 = tolower(keywords.2)

selected_keywords=c(names(table),keywords.2)
selected_words=selected_keywords[which(nchar(selected_keywords)>2)]
selected_words=unique(selected_words)
selected_words

## Keyexpressions extraction (based on keywords cooccurence)

text_to_explore = corp$content
text_to_explore = unlist(strsplit(text_to_explore,split=" "))

text_to_explore.2 = vector()
for (i in 1:length(text_to_explore)-1){
  text_to_explore.2[i]=paste0(text_to_explore[i]," ",text_to_explore[i+1])
}

keyword.1_2 = vector()
count = vector()
x = 0
for (i in 1:length(selected_words)){
  for (j in 1:length(selected_words)){
    x = x+1
    keyword.1_2[x] = paste0(selected_words[i]," ",selected_words[j])
    tmp = grep(keyword.1_2[x],text_to_explore.2)
    count[x]=length(tmp)
  }
}
count = as.numeric(as.vector(count))

cooccurences = cbind(keyword.1_2,count)
cooccurences = data.frame(cooccurences)
cooccurences[,2] = as.numeric(as.vector(cooccurences[,2]))
cooccurences = cooccurences[which(cooccurences[,2]>0),]

cooccurences_above_threshold=cooccurences[which(cooccurences[,2]>cooccurence_threshold),]

#as.vector(cooccurences_above_threshold[,1])


## visual network of keyexpression building


from_to=matrix(unlist(strsplit(as.character(cooccurences_above_threshold[,1]),split=" ")),ncol=2,byrow=T)

weight=cooccurences_above_threshold[,2]

network = data.frame(from_to)

tmp=(max(weight)-weight)/max(weight)*10
p = simpleNetwork(network, height="100px", width="100px",fontSize = 20,opacity = 1, zoom=T)
p


selected_words
as.character(cooccurences_above_threshold[,1])


## Export

outfile=paste0(corpus_dir,"/","output_article",file_number,".out")

tmp="\\keywords"
write.table(tmp,outfile,sep="\t",row.names=F,col.names=F,quote=F)
tmp=paste(selected_words,sep="",collapse=",")
write.table(tmp,outfile,sep="\t",row.names=F,col.names=F,quote=F,append=T)
tmp="\\keyexpressions"
write.table(tmp,outfile,sep="\t",row.names=F,col.names=F,quote=F,append=T)
tmp=as.character(cooccurences_above_threshold[,1])
tmp=paste(tmp,sep="",collapse=",")
write.table(tmp,outfile,sep="\t",row.names=F,col.names=F,quote=F,append=T)

outfile=paste0("output_article",file_number,"_network.html")

dir=paste0(dir,corpus_dir)
setwd(dir)

saveNetwork(p, outfile, selfcontained = TRUE)
