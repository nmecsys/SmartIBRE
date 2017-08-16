api_ibge<-function(linhas){
  
  p1="http://seriesestatisticas.ibge.gov.br/exportador.aspx?arquivo="
  #linhas<-input$tabela_IBGE_rows_selected
  p2<-as.character(dados[linhas,1])
  #p2=as.character(dados[masc,1])
  p3="_BR_PERC.csv"
  p4="_BR_ABS.csv"
  
  frame_periodo=data.frame("Periodo"=c("Anual","Ano_mes","Mensal","Trimestral"),"Valor"=c(1,1,12,4))
  
  b=0
  
  for (i in 1:length(p2)){
    
    x=subset(frame_periodo,frame_periodo[,1]==dados[linhas[i],3])
    t=x[,2]
    
    if (b==t || i<2){
      erro=0
      url<-paste0(p1,p2[i],p3)
      y<-try(download.file(url,paste0(getwd(),"/","data/","elegante.csv")))
      
      if (y!=0){
        url=paste0(p1,p2[i],p4)
        y<-try(download.file(url,paste0(getwd(),"/","data/","elegante.csv")))
        
        if (y!=0){
          masc_falt<-subset(dados_faltante,dados_faltante[,1]==p2[i])
          url<-masc_falt[,6]
          y<-try(download.file(url,paste0(getwd(),"/","data/","elegante.csv")))
        }
        
      } 
      
      cat(p2[i])
      api<-read.csv(paste0(getwd(),"/","data/","elegante.csv"),sep="\t",dec=",") # serie do IBGE com cabeçalho   
      api_t<-read.csv(paste0(getwd(),"/","data/","elegante.csv"),sep="\t",header=FALSE,dec=",")# serie do IBGE sem cabeçalho 
      
      if (dados[linhas[i],3]=="Mensal"){
        if(as.character(api_t[1,2])=="OPCAO"){
          api<-api[,-1]
          api2<-data.frame(t(api[,-c(1:2)]))
          names(api2)<-api[,1]
          apit<-data.frame(t(api_t))
          apit<-apit[-c(1,2),]
        }else{
          api2<-data.frame(t(api[,-1]))
          names(api2)<-api[,1]
          apit<-data.frame(t(api_t))
          apit<-apit[-1,] #retirar o a primeira linha com strings
        }
        
        apit$mes<-substr(apit[,1],1,3)# criando a coluna mes (dividindo a coluna de periodo,ex:"jul\94")
        apit$ano<-as.numeric(substr(apit[,1],5,7))# criando a coluna ano (dividindo a coluna de periodo,ex:"jul\94")
        
        #classificando os anos
        masc_1900<-apit$ano > 50
        masc_2010<-apit$ano <=50 & apit$ano >=10
        masc_2000<-apit$ano<10
        
        apit$anoof<-0
        apit$mesof<-0
        
        apit[masc_1900,"anoof"]<-19
        apit[masc_2000,"anoof"]<-200
        apit[masc_2010,"anoof"]<-20
        
        apit$anouso<-paste0(apit$anoof,apit$ano)
        
        masc_jan<-apit$mes=="jan"
        masc_fev<-apit$mes=="fev"
        masc_mar<-apit$mes=="mar"
        masc_abr<-apit$mes=="abr"
        masc_mai<-apit$mes=="mai"
        masc_jan<-apit$mes=="jan"
        masc_jun<-apit$mes=="jun"
        masc_jul<-apit$mes=="jul"
        masc_ago<-apit$mes=="ago"
        masc_set<-apit$mes=="set"
        masc_out<-apit$mes=="out"
        masc_nov<-apit$mes=="nov"
        masc_dez<-apit$mes=="dez"
        
        apit[masc_jan,"mesof"]=1
        apit[masc_fev,"mesof"]=2
        apit[masc_mar,"mesof"]=3
        apit[masc_abr,"mesof"]=4
        apit[masc_mai,"mesof"]=5
        apit[masc_jun,"mesof"]=6
        apit[masc_jul,"mesof"]=7
        apit[masc_ago,"mesof"]=8
        apit[masc_set,"mesof"]=9
        apit[masc_out,"mesof"]=10
        apit[masc_nov,"mesof"]=11
        apit[masc_dez,"mesof"]=12 
        
        datas<-row.names(api2)
        api3<-cbind(datas,api2)
        
        if (i==1){
          #dados_a <-api3
          cnames<-as.character(api[,1])
          #data<-apit[,c("mesof","anouso")]   
          mes_inicio <- apit[1,"mesof"]
          ano_inicio <- as.numeric(apit[1,"anouso"])
          ts <- ts(api3[,-1], start =  c(ano_inicio, mes_inicio), freq = t)
          nomes<-paste0(as.character(p2[i]),"_",names(api2))
          #colnames(ts)<-cnames
          nome <-"IBGE"
          
        }else{
          #dados_a <-merge(dados_a,api3,by="data")
          cnames<-c(cnames,as.character(api[,1]))
          #data<-rbind(data,(apit[,c("mesof","anouso")]))
          #data<-data[order(data[,2],data[,1]),]
          mes_inicio <- apit[1,"mesof"]
          ano_inicio <- as.numeric(apit[1,"anouso"])
          ts <-cbind(ts, ts(api3[,-1], start =c(ano_inicio, mes_inicio), freq = t))
          #colnames(ts)<-cnames
          nomes<-c(nomes,paste0(as.character(p2[i]),"_",names(api2)))
          nome <-"IBGE"
        }
        
        
      }else if (dados[linhas[i],3]=="Ano_mes"){
        
        if(as.character(api_t[1,2])=="OPCAO"){
          api<-api[,-1]
          api2<-data.frame(t(api[,-c(1:2)]))
          names(api2)<-api[,1]
          apit<-data.frame(t(api_t))
          apit<-apit[-c(1,2),]
        }else{
          api2<-data.frame(t(api[,-1]))
          names(api2)<-api[,1]
          apit<-data.frame(t(api_t))
          apit<-apit[-1,] #retirar o a primeira linha com strings
        }
        
        apit$mes<-substr(apit[,1],1,3)# criando a coluna mes (dividindo a coluna de periodo,ex:"jul\94")
        apit$ano<-as.numeric(substr(apit[,1],5,7))# criando a coluna ano (dividindo a coluna de periodo,ex:"jul\94")
        
        #classificando os anos
        masc_1900<-apit$ano > 50
        masc_2010<-apit$ano <=50 & apit$ano >=10
        masc_2000<-apit$ano<10
        
        apit$anoof<-0
        apit$mesof<-0
        
        apit[masc_1900,"anoof"]<-19
        apit[masc_2000,"anoof"]<-200
        apit[masc_2010,"anoof"]<-20
        
        apit$anouso<-paste0(apit$anoof,apit$ano)
        
        masc_jan<-apit$mes=="jan"
        masc_fev<-apit$mes=="fev"
        masc_mar<-apit$mes=="mar"
        masc_abr<-apit$mes=="abr"
        masc_mai<-apit$mes=="mai"
        masc_jan<-apit$mes=="jan"
        masc_jun<-apit$mes=="jun"
        masc_jul<-apit$mes=="jul"
        masc_ago<-apit$mes=="ago"
        masc_set<-apit$mes=="set"
        masc_out<-apit$mes=="out"
        masc_nov<-apit$mes=="nov"
        masc_dez<-apit$mes=="dez"
        
        apit[masc_jan,"mesof"]=1
        apit[masc_fev,"mesof"]=2
        apit[masc_mar,"mesof"]=3
        apit[masc_abr,"mesof"]=4
        apit[masc_mai,"mesof"]=5
        apit[masc_jun,"mesof"]=6
        apit[masc_jul,"mesof"]=7
        apit[masc_ago,"mesof"]=8
        apit[masc_set,"mesof"]=9
        apit[masc_out,"mesof"]=10
        apit[masc_nov,"mesof"]=11
        apit[masc_dez,"mesof"]=12 
        
        datas<-row.names(api2)
        api3<-cbind(datas,api2)
        
        if (i==1){
          #dados_a <-api3
          cnames<-as.character(api[,1])
          #data<-apit[,c("mesof","anouso")]   
          mes_inicio <- apit[1,"mesof"]
          ano_inicio <- as.numeric(apit[1,"anouso"])
          ts <- ts(api3[,-1], start =  c(ano_inicio, mes_inicio), freq = t)
          #colnames(ts)<-cnames
          nomes<-paste0(as.character(p2[i]),"_",names(api2))
          nome <-"IBGE"
          
        }else{
          #dados_a <-merge(dados_a,api3,by="data")
          cnames<-c(cnames,as.character(api[,1]))
          #data<-rbind(data,(apit[,c("mesof","anouso")]))
          #data<-data[order(data[,2],data[,1]),]
          mes_inicio <- apit[1,"mesof"]
          ano_inicio <- as.numeric(apit[1,"anouso"])
          ts <-cbind(ts, ts(api3[,-1], start =c(ano_inicio, mes_inicio), freq = t))
          #colnames(ts)<-cnames
          nomes<-c(nomes,paste0(as.character(p2[i]),"_",names(api2)))
          nome <-"IBGE"
        }
        
      }else if(dados[linhas[i],3]=="Anual"){
        if(as.character(api_t[1,2])=="OPCAO"){
          api<-api[,-1]
          api2<-data.frame(t(api[,-c(1:2)]))
          names(api2)<-api[,1]
          apit<-data.frame(t(api_t))
          apit<-apit[-c(1,2),]
        }else{
          api2<-data.frame(t(api[,-1]))
          names(api2)<-api[,1]
          apit<-data.frame(t(api_t))
          apit<-apit[-1,] #retirar o a primeira linha com strings
        }
        
        apit$ano<-as.numeric(substr(apit[,1],1,4))
        
        if (is.na(apit$ano)==TRUE){
          apit$ano<-as.numeric(as.character(apit[,1]))
        }
        
        
        datas<-row.names(api2)
        api3<-cbind(datas,api2)
        
        if (i==1){
          #dados_a <-api3
          cnames<-as.character(api[,1])
          #data<-apit[,c("mesof","anouso")]   
          ano_inicio <- as.numeric(apit[1,"ano"])
          ts <- ts(api3[,-1], start =ano_inicio, freq = t)
          #colnames(ts)<-cnames
          nomes<-paste0(as.character(p2[i]),"_",names(api2))
          nome <-"IBGE"
          
        }else{
          #dados_a <-merge(dados_a,api3,by="data")
          cnames<-c(cnames,as.character(api[,1]))
          #data<-rbind(data,(apit[,c("mesof","anouso")]))
          #data<-data[order(data[,2],data[,1]),
          ano_inicio <- as.numeric(apit[1,"ano"])
          ts <-cbind(ts, ts(api3[,-1], start =ano_inicio, freq = t))
          #colnames(ts)<-cnames
          nomes<-c(nomes,paste0(as.character(p2[i]),"_",names(api2)))
          nome <-"IBGE"
        }
      }else if(dados[linhas[i],3]=="Trimestral"){
        if(as.character(api_t[1,2])=="OPCAO"){
          api<-api[,-1]
          api2<-data.frame(t(api[,-c(1:2)]))
          names(api2)<-api[,1]
          apit<-data.frame(t(api_t))
          apit<-apit[-c(1,2),]
        }else{
          api2<-data.frame(t(api[,-1]))
          names(api2)<-api[,1]
          apit<-data.frame(t(api_t))
          apit<-apit[-1,] #retirar o a primeira linha com strings
        }
        
        apit$ano<-as.numeric(substr(apit[,1],1,4))
        apit$trimestre<-as.numeric(substr(apit[,1],7,7))
        
        datas<-row.names(api2)
        api3<-cbind(datas,api2)
        
        if (i==1){
          #dados_a <-api3
          cnames<-as.character(api[,1])
          #data<-apit[,c("mesof","anouso")]   
          ano_inicio <- as.numeric(apit[1,"ano"])
          trimestre_inicio<- as.numeric(apit[1,"trimestre"])
          ts <- ts(api3[,-1], start =c(ano_inicio,trimestre_inicio), freq = t)
          #colnames(ts)<-cnames
          nomes<-paste0(as.character(p2[i]),"_",names(api2))
          nome <-"IBGE"
          
        }else{
          #dados_a <-merge(dados_a,api3,by="data")
          cnames<-c(cnames,as.character(api[,1]))
          #data<-rbind(data,(apit[,c("mesof","anouso")]))
          #data<-data[order(data[,2],data[,1]),
          ano_inicio <- as.numeric(apit[1,"ano"])
          trimestre_inicio<- as.numeric(apit[1,"trimestre"])
          ts <-cbind(ts, ts(api3[,-1], start =c(ano_inicio,trimestre_inicio), freq = t))
          #colnames(ts)<-cnames
          nomes<-c(nomes,paste0(as.character(p2[i]),"_",names(api2)))
          #nome <-"IBGE"
          colnames(ts)<-nomes
        }
      }
      
      b=t
      # erro<-0
    }else{
      erro<-1
      break()
    }
  }
  
  if (i>1){
    colnames(ts)<-nomes
    
  }
  
  data_ibge=as.character(as.Date(ts))
  
  list(apit=apit,ts=ts,codigo=p2,nome=nomes,erro=erro,data=data_ibge,tipo=dados[linhas[i],3],descricao=as.character((dados[linhas,2])))
  
  
}

ts
