removeEnglishWords <- function(x){
      return(gsub('[a-z]|[A-Z]','',as.character(x)))
}

reomveElongation <- function(x){
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


normalizeArabic <- function(x) {
      text_temp <- x
      text_temp <- gsub("\\p{P}", " ", text_temp, perl = TRUE) # Remove punctuation
      # Remove leading whitespace, remove extra spaces, remove non-letter, non-space characters
      text_temp <- gsub('^ ', '', stripWhitespace(gsub('[^\\p{L}\\p{Zs}]', '', text_temp, perl = TRUE)))
      text_temp <- stripWhitespace(gsub('\\x{0623}|\\x{0622}|\\x{0625}|\\x{0671}|\\x{0672}|\\x{0673}', 'ا', text_temp)) 
      # Normalize alefs with hamzas
      text_temp <- gsub('\\p{Zs}\\x{0648}*\\x{0627}\\x{0644}(?=\\p{L})', ' ', text_temp, perl = TRUE) 
      # Remove leading alef lam with optional leading waw
      text_temp <- gsub('^\\x{0627}\\x{0644}(?=\\p{L})', '', text_temp, perl = TRUE) 
      # Remove leading alef lam at start of string
      text_temp <- gsub('\\p{Zs}\\x{0648}*\\x{0644}{2,}(?=\\p{L})', ' ', text_temp, perl = TRUE) 
      # Remove leading double lam at start of string
      text_temp <- gsub('\\p{Zs}\\x{0648}*\\x{0643}\\x{0627}\\x{0644}(?=\\p{L})', ' ', text_temp, perl = TRUE) 
      # Remove leading kaf alef lam with optional waw
      text_temp <- gsub('\\p{Zs}\\x{0648}*\\x{0628}\\x{0627}\\x{0644}(?=\\p{L})', ' ', text_temp, perl = TRUE) 
      # Remove leading baa alef lam with optional waw
      text_temp <- gsub('\\p{Zs}\\x{0648}*\\x{0641}\\x{0627}\\x{0644}(?=\\p{L})', ' ', text_temp, perl = TRUE) 
      # Remove leading faa alef lam with optional waw
      text_temp <- gsub('\\p{Zs}\\x{0648}*\\x{0627}{2,}\\x{0644}*(?=\\p{L})', ' ', text_temp, perl = TRUE) 
      # Remove leading double alef with optional lam with optional leading waw
      text_temp <- gsub('(?<=\\p{L})\\x{0647}(?=\\p{Zs})', ' ', text_temp, perl = TRUE) 
      # Remove trailing haa
      text_temp <- gsub('(?<=\\p{L})\\x{0649}(?=\\p{Zs})', 'ي', text_temp, perl = TRUE) 
      # Normalize ending yeh
      text_temp <- gsub('(?<=\\p{L})\\x{064A}{2,}\\x{0646}(?=\\p{Zs})', '', text_temp, perl = TRUE) 
      # Remove trailing yeh yeh noon
      text_temp <- gsub('(?<=\\p{L})\\x{064A}\\x{0648}\\x{0646}(?=\\p{Zs})', '', text_temp, perl = TRUE) 
      # Remove trailing yeh waw noon
      text_temp <- gsub('(?<=\\p{L})\\x{0647}\\x{0647}*(?=\\p{Zs})', '', text_temp, perl = TRUE) 
      # Remove trailing haa or haa alef
      text_temp <- gsub('(?<=\\p{L})\\x{0647}\\x{0645}\\x{0627}*(?=\\p{Zs})', '', text_temp, perl = TRUE) 
      # Remove trailing haa meem and haa meem alef
      text_temp <- gsub('(?<=\\p{Zs})\\p{L}(?=\\p{Zs})', '', text_temp, perl = TRUE) 
      # Remove single letters such as waw and those produced by above normalization
      text_temp <- stripWhitespace(gsub('(\\p{Zs}$)|(^\\p{Zs})', '', text_temp, perl = TRUE)) 
      # Remove added, leading, trailing whitespace
      return(text_temp)
}

convertCounts <- function(x) {
      x <- ifelse(x > 0, 1, 0)
      x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

stemBlArabi <- function(txt){
      txt <- as.character(txt)
      stem(txt, returnStemList = F, transliteration = F)
}
