source("api_ibge.R")


lista<-c(7,59)

dados[lista,3]

as.character(dados[lista,1])

ib<-api_ibge(lista)
ib$ts


nomes<-colnames(ib$ts)

colnames(ib$ts)

a=nomes[1]
b=nomes[2]

b

ts<-as.xts(ib$ts[,1])#,b=as.xts(ib$ts[,2]))

for(i in 2:dim(ib$ts)[2]){
  b=nomes[i]
  ts<-cbind(ts,as.xts(ib$ts[,i]))}

# ts<-na.omit(ts)
colnames(ts)<-nomes

dygraph(ts, main = "")
ts


ts
