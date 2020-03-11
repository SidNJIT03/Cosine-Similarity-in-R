#Write a function in R to compute the cosine similarity between a document and a query

#Use:
library(tm)
create_matrix <- function(textColumns, language="english", minDocFreq=1, maxDocFreq=Inf, minWordLength=3, maxWordLength=Inf, ngramLength=1, originalMatrix=NULL, removeNumbers=FALSE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=TRUE,  stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE, weighting=weightTf) {
  
  stem_words <- function(x) {
    split <- strsplit(x," ")
    return(wordStem(unlist(split),language=language))
  }
  
  tokenize_ngrams <- function(x, n=ngramLength) return(rownames(as.data.frame(unclass(textcnt(x,method="string",n=n)))))
  
  control <- list(bounds=list(local=c(minDocFreq,maxDocFreq)),language=language,tolower=toLower,removeNumbers=removeNumbers,removePunctuation=removePunctuation,stopwords=removeStopwords,stripWhitespace=stripWhitespace,wordLengths=c(minWordLength,maxWordLength),weighting=weighting)
  
  if (ngramLength > 1) { 
    control <- append(control,list(tokenize=tokenize_ngrams),after=7)
  } else {
    control <- append(control,list(tokenize=scan_tokenizer),after=4)
  }
  
  if (stemWords == TRUE && ngramLength == 1) control <- append(control,list(stemming=stem_words),after=7)
  
  trainingColumn <- apply(as.matrix(textColumns),1,paste,collapse=" ")
  trainingColumn <- sapply(as.vector(trainingColumn,mode="character"),iconv,to="UTF8",sub="byte")
  
  corpus <- Corpus(VectorSource(trainingColumn),readerControl=list(language=language))
  matrix <- DocumentTermMatrix(corpus,control=control);
  if (removeSparseTerms > 0) matrix <- removeSparseTerms(matrix,removeSparseTerms)
  
  if (!is.null(originalMatrix)) {
    terms <- colnames(originalMatrix)[which(!colnames(originalMatrix) %in% 
                                              colnames(matrix))]
    
    weight <- 0
    if (attr(weighting,"acronym")=="tf-idf") weight <- 0.000000001
    amat <- matrix(weight,nrow=nrow(matrix),ncol=length(terms))
    colnames(amat) <- terms
    rownames(amat) <- rownames(matrix)
    
    fixed <- as.DocumentTermMatrix(cbind(matrix[,which(colnames(matrix) %in% colnames(originalMatrix))],amat),weighting=weighting)
    matrix <- fixed
  }
  
  matrix <- matrix[,sort(colnames(matrix))]
  
  gc()
  return(matrix)
}


documents=c("the grey cat is nice","how to feed cats","dogs make great pets")
corpus<-create_matrix(documents,removeStopwords = T,stemWords = T)
corpus<- as.matrix(corpus)

q1="feed cats"
#colnames(corpus, do.NULL = TRUE, prefix = "col")


vec<-corpus[2,]
vec1<-as.vector(vec)


q1matrix<-create_matrix(q1,originalMatrix = corpus,removeStopwords = T,stemWords = T)
q1matrix<-as.matrix(q1matrix)

cosval <- sum(vec1*q1matrix)

vec1_sq<- sqrt(sum(vec1^2))
q1_sq<- sqrt(sum(q1matrix^2))

result <- cosval / (vec1_sq * q1_sq)
result

#Question 1: Show your results without stemming and stopword removal

#Question 2: Show your results with stemming and stop word removal


#The format of the function should be as follows:


cosinesim<-function(query,document){
  #Your code here

  dimensions<-dimnames(corpus)
  colnames = c(unlist[dimensions[2], use.names =  FALSE])
  rownames = c("doc_freq")
  n_col<-ncol(corpus)
  n_row<-nrow(corpus)
  doc_freq_vec<-c()
  
  for (r in 1:n_row){
    doc_freq_words = 0
 
    for (c in 1:n_col)  
      if(corpus[i,j]>0){
        doc_freq_words = doc_freq_words + 1
      }
  doc_freq_vec[j]<-doc_freq_words
  }   
  
  df<-matrix(n_row/doc_freq_vec, nrow = 1, byrow = TRUE, dinames = list(rownames, colnames))
  idf<-log10(df)
  return (idf)
}

  getWeight<-function(tf,idf){
    dimensions<-dinames(tf)
    w_vector<-c()
    n_col<-ncol(tf)
    n_row<-nrow(tf)
      for(j in 1:n_col){
        for(i in 1:n_row){
          w_vector<-append(w_vector,(tf[i,j]*idf[j]))
        }
      }
  }
  result <- cosval / (vec1_sq * q1_sq)
  result 
  
}