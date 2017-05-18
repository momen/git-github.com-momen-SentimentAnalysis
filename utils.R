arabic.lexicon <- read.xlsx("data/sentimentLex_modified.xlsx",sheetIndex = 1,encoding = "UTF-8")
arabic.lexicon.neg <- filter(arabic.lexicon, Polarity == "neg")
arabic.lexicon.pos <- filter(arabic.lexicon, Polarity == "pos")

replaceNegWords <- function(text){
      l <- apply(arabic.lexicon.neg, 1, function(x){
            if(grepl(x[1], text)){
                  # print(x[1])
                  text <<- paste(text, "كلمهسالب"  ,  sep = " ")
            }
      })
      return(text)
}

replacePosWords <- function(text){
      l <- apply(arabic.lexicon.pos, 1, function(x){
            if(grepl(x[1], text)){
                  # print(x[1])
                  text <<- paste(text, "كلمهموجب"  ,  sep = " ")
            }
      })
      return(text)
}

removeEnglishWords <- function(x){
      return(gsub("[a-z]|[A-Z]","",x))
}

removeElongation <- function(x){
      text_temp <- x
      
      # remove Alef elongation  
      text_temp <- gsub("\u0627(\u0627*)", "\u0627", text_temp)
      
      # remove Baa' elongation  
      text_temp <- gsub("\u0628(\u0628*)", "\u0628", text_temp)
      
      # remove Haa' elongation  
      text_temp <- gsub("\u0647(\u0647*)", "\u0647", text_temp)
      
      # remove Khaa' elongation  
      text_temp <- gsub("\u062E(\u062E*)", "\u062E", text_temp)
      
      # remove Waoo' elongation  
      text_temp <- gsub("\u0648(\u0648*)", "\u0648", text_temp)

      # remove Yaa' elongation  
      text_temp <- gsub("\u064A(\u064A*)", "\u064A", text_temp)
      
      # remove Yaa'wza  elongation  
      text_temp <- gsub("\u0649(\u0649*)", "\u0649", text_temp)

      # remove 3yen  elongation  
      text_temp <- gsub("\u0639(\u0639*)", "\u0639", text_temp)
      
      # remove Raa'  elongation  
      text_temp <- gsub("\u0631(\u0631*)", "\u0631", text_temp)
      
      # remove Rayz  elongation  
      text_temp <- gsub("\u0632(\u0632*)", "\u0632", text_temp)
      
      return(text_temp)     
}

reomveSingleLetters <- function(x){
      text_temp <- x
      
      # remove Single Alef   
      text_temp <- gsub("^\u0627{1}$", "", text_temp)
      
      # remove Single Baa'   
      text_temp <- gsub("^\u0628{1}$", "", text_temp)
      
      # remove Single Haa'   
      text_temp <- gsub("^\u0647{1}$", "", text_temp)
      
      # remove Single Khaa'   
      text_temp <- gsub("^\u062E{1}$", "", text_temp)
      
      # remove Single Waoo'   
      text_temp <- gsub("^\u0648{1}$", "", text_temp)
      
      # remove Single Yaa'  
      text_temp <- gsub("^\u064A{1}$", "", text_temp)
      
      # remove Single Yaa'wza   
      text_temp <- gsub("^\u0649{1}$", "", text_temp)
      
      # remove Single 3yen    
      text_temp <- gsub("^\u0639{1}$", "", text_temp)
      
      # remove Single Raa'    
      text_temp <- gsub("^\u0631{1}$", "", text_temp)
      
      # remove Single Rayz  
      text_temp <- gsub("^\u0632{1}$", "", text_temp)
      
      return(text_temp)     
}

convertCounts <- function(x) {
      x <- ifelse(x > 0, 1, 0)
      x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

removeMention  <- function(x){
  return(gsub("@[[:alnum:]]*", "منشن", x))
}

removeHashtag <- function(x){
  return(gsub("#[[:alnum:]]*","هاشتاج", x))
}

removeHTTP <- function(x){
      return(gsub("http[[:alnum:][:punct:]]*", "كلمهلينك", x))
} 

arabicStemming <- function(x){
      x <- as.character(x)
      stem(x, returnStemList = F, transliteration = F)
}

countLinks <- function(dataset){
      dataset$numLinks <- rep(0 , times = nrow(dataset))
      lens <- as.vector(sapply(dataset$tweet,function(row){
        str_count(row,"كلمهلينك")
      }))
      dataset$numLinks <- as.numeric(lens)
      dataset
}

countNegWords <- function(dataset){
  dataset$numNegWords <- rep(0 , times = nrow(dataset))
  lens <- as.vector(sapply(dataset$tweet,function(row){
    str_count(row,"كلمهسالب")
  }))
  dataset$numNegWords <- as.numeric(lens)
  dataset
}

countPosWords <- function(dataset){
  dataset$numPosWords <- rep(0 , times = nrow(dataset))
  lens <- as.vector(sapply(dataset$tweet,function(row){
    str_count(row,"كلمهموجب")
  }))
  dataset$numPosWords <- as.numeric(lens)
  dataset
}

tweet.length <- function(dataset){
  dataset$length <- rep(0 , times = nrow(dataset))
  lens <- as.vector(sapply(dataset$tweet,function(row){
    l <- 0
   # print(nchar(row) )
    if(nchar(row) < 60)
      l <- 0
    else if(nchar(row) < 100 )
      l <- 1
    else
      l <- 2
    
    l
  }))
  dataset$length <- as.numeric(lens)
  dataset
}
