rm(list=ls())
setwd('/Users/KK/STAT850/R-EXAM/Scan/Output')
list.files('../Data')

mm.file = ('../Data/data.txt')
fileIn=file(mm.file,open='r')
work=scan(fileIn,nlines=10,what=character(),quiet=TRUE, sep=',')
work
mat = matrix(work, ncol = 10, byrow = TRUE)
work1=t(mat)
write.csv(work1, 'new obs1.csv')
write.csv(work1, 'new obs2.csv')
write.csv(work1, 'new obs3.csv')

tr <- list.files('../Output')

for(i in 1:length(tr))
{  
  TT <- read.csv(paste('../Output/',tr[i],sep=''),stringsAsFactors=FALSE)
  form=sprintf('obs%d.csv', i)
  write.csv(TT,file=form)
}

close(fileIn)
