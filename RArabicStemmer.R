library(rJava)

getArabicStemmer <- function(){
      .jinit('.')
      .jaddClassPath("ArabicStemmer.jar")
      Stemmer <- .jnew("elbeltagyRafea/SimpleArStemmer")
      return(Stemmer)
}

stemWord <- function(word){
      stemmer <-  getArabicStemmer()
      #print(word)
      jword <- .jnew('java/lang/String', word)
      out   <- .jcall(stemmer, 'Ljava/lang/String;', 'simpleStem',jword)
      return(out)
}

stemWordWithLog <- function(word, outFile){
      stemmer <-  getArabicStemmer()
      jword <- .jnew('java/lang/String', word)
      joutFile <- .jnew('java/lang/String', outFile)
      out   <- .jcall(stemmer, 'S', 'stemWordAndLog',jword,joutFile,evalString = F, simplify = T)
      return(out)
}

stemFile <- function(inFile, outFile){
      stemmer <-  getArabicStemmer()
       f1 <- .jnew('java/lang/String',inFile)
       f2 <- .jnew('java/lang/String',outFile)
       .jcall(stemmer, 'V', 'stemFile',f1,f2)
}

#stemmer <- getArabicStemmer()
#stemWord("كلمة")
# stemWordWithLog(stemmer,"كلمة","out.txt")
# Sys.setlocale("LC_ALL", "Arabic")

