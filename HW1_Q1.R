

# make up some data
set.seed(13)

df100<-data.frame(
  f1=sample(paste('f1',1:2,sep=''),100,rep=T),
  f2=sample(paste('f2',1:3,sep=''),100,rep=T),
  label=sample(c('C','D'),100,replace=  T))
r2
dfs14<-df100[sample(1:100,14,rep=F),]

# computing conditionally independent probabilities in R
getCondProb<-function(df2=dfs14,labelcol='label',label)
{
  tdf<-df2[df2[labelcol]==label,1:ncol(df2)-1];
  apply(tdf,2,FUN=function(x){table(x)/length(x)})
}

r1<-c(label='C',prob=getCondProb(dfs14,'label','C'))
r2<-c(label='D',prob=getCondProb(dfs14,'label','D'))



#print("class prior probabilities:");print(round(table(dfs14$label)/nrow(dfs14),6))

getProbStrings<-function(o=r1)
{
  nam2<-o$prob.f1
  nam3<-names(nam2)
  s<-unlist(lapply(1:length(nam3),FUN=function(x)
  {
    paste('p(',substring(nam3[[x]],1,2),"=",nam3[[x]],'|',o$label,')=',round(nam2[[x]],6),sep='')
  }))
  nam2<-o$prob.f2
  nam3<-names(nam2)
  s<-c(s,unlist(lapply(1:length(nam3),FUN=function(x)
  {
    paste('p(',substring(nam3[[x]],1,2),"=",nam3[[x]],'|',o$label,')=',round(nam2[[x]],6),sep='')
  })))
  s
}

#getProbStrings(r1)
print("class prior probabilities:");print(round(table(dfs14$label)/nrow(dfs14),6))
l1<-data.frame(L=getProbStrings(r1),stringsAsFactors=F)
l2<-data.frame(L=getProbStrings(r2),stringsAsFactors=F)
l12<-rbind(l1,l2)

l12