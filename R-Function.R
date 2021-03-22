palindrome5 = function(x){
  b=stringi::stri_reverse(x)
  
  if(nchar(x) == 5 & x==b){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

palindrome5("level")
palindrome5("apple")
palindrome5("radar")


