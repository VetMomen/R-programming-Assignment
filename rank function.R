rankhospital<-function(df,state,outcome,ranke=1){
        if(ranke=='best'){
                sat<-unique(df$state)
                if(any(state==sat)){
                        data1<-df[df$state==state,]
                        data2<-data1%>%select("Hospital_name","state","Heart_Attack_rate","Heart_Failure_rate",
                                              "Pneumonia_rate")%>%drop_na()
                        if(outcome=='Heart Attack'){
                                data2$rank<-rank(data2$Heart_Attack_rate,ties.method = 'min')
                                data3<-data2[data2$rank==1,c('Hospital_name','Heart_Attack_rate','rank')]
                        } else if(outcome=='Heart Failure'){
                                data2$rank<-rank(data2$Heart_Failure_rate,ties.method = 'min')
                                data3<-data2[data2$rank==1,c('Hospital_name','Heart_Failure_rate','rank')]
                        }else if(outcome=='Pneumonia'){
                                data2$rank<-rank(data2$Pneumonia_rate,ties.method = 'min')
                                data3<-data2[data2$rank==1,c('Hospital_name','Pneumonia_rate','rank')]   
                        }else stop('invalide outcome')
                } else {
                        stop('invalid state')
                }
        }else if(ranke=='worst'){
                sat<-unique(df$state)
                if(any(state==sat)){
                        data1<-df[df$state==state,]
                        data2<-data1%>%select("Hospital_name","state","Heart_Attack_rate","Heart_Failure_rate",
                                              "Pneumonia_rate")%>%drop_na()
                        if(outcome=='Heart Attack'){
                                data2$rank<-rank(data2$Heart_Attack_rate,ties.method = 'min')
                                data3<-data2[data2$rank==max(data2$rank),c('Hospital_name','Heart_Attack_rate','rank')]
                        } else if(outcome=='Heart Failure'){
                                data2$rank<-rank(data2$Heart_Failure_rate,ties.method = 'min')
                                data3<-data2[data2$rank==max(data2$rank),c('Hospital_name','Heart_Failure_rate','rank')]
                        }else if(outcome=='Pneumonia'){
                                data2$rank<-rank(data2$Pneumonia_rate,ties.method = 'min')
                                data3<-data2[data2$rank==max(data2$rank),c('Hospital_name','Pneumonia_rate','rank')]   
                        }else stop('invalide outcome')
                } else {
                        stop('invalid state')
                }
        }else {
                sat<-unique(df$state)
                if(any(state==sat)){
                        data1<-df[df$state==state,]
                        data2<-data1%>%select("Hospital_name","state","Heart_Attack_rate","Heart_Failure_rate",
                                              "Pneumonia_rate")%>%drop_na()
                        if(outcome=='Heart Attack'){
                                data2$rank<-rank(data2$Heart_Attack_rate,ties.method = 'min')
                                data3<-data2[data2$rank==ranke,c('Hospital_name','Heart_Attack_rate','rank')]
                        } else if(outcome=='Heart Failure'){
                                data2$rank<-rank(data2$Heart_Failure_rate,ties.method = 'min')
                                data3<-data2[data2$rank==ranke,c('Hospital_name','Heart_Failure_rate','rank')]
                        }else if(outcome=='Pneumonia'){
                                data2$rank<-rank(data2$Pneumonia_rate,ties.method = 'min')
                                data3<-data2[data2$rank==ranke,c('Hospital_name','Pneumonia_rate','rank')]   
                        }else stop('invalide outcome')
                } else {
                        stop('invalid state')
                }
        }
        if(length(data3$Hospital_name)>=1){print(data3)}else{
                warning('you choosed a rank exceed the number of hospitals')
                print(data3)
        }
}


rankhospital(df=outcome,state = 'AR',outcome = 'Heart Failure',ranke = 5)


