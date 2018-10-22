best<-function(df,state,outcome){
        sat<-unique(df$state)
        if(any(state==sat)){
                data1<-df[df$state==state,]
                data2<-data1%>%select("Hospital_name","state","Heart_Attack_rate","Heart_Failure_rate",
                                      "Pneumonia_rate")%>%drop_na()
                if(outcome=='Heart Attack'){
                        data4<-data2[data2$Heart_Attack_rate==min(data2$Heart_Attack_rate),"Hospital_name"]
                } else if(outcome=='Heart Failure'){
                        data4<-data2[data2$Heart_Failure_rate==min(data2$Heart_Failure_rate),"Hospital_name"]
                }else if(outcome=='Pneumonia'){
                        data4<-data2[data2$Pneumonia_rate==min(data2$Pneumonia_rate),"Hospital_name"]   
                }else stop('invalide outcome')
        } else {
                stop('invalid state')
        }
        data4<-data4%>%arrange()
        print(data4)
}