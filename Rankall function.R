rankall<-function(df,outcome,rank=1){
        if(outcome=='Heart Attack'){
                data1<-df%>%select(state,Hospital_name,Heart_Attack_rate,Heart_Failure_rate,Pneumonia_rate)
                data2<-data1%>%group_by(state)%>%mutate('rank'=rank(Heart_Attack_rate,na.last = 'keep',ties.method = 'min'))%>%drop_na()
                data3<-data2[data2$rank==rank,c('state','Hospital_name','rank')]
        }else if(outcome=='Heart Failure'){
                data1<-df%>%select(state,Hospital_name,Heart_Attack_rate,Heart_Failure_rate,Pneumonia_rate)
                data2<-data1%>%group_by(state)%>%mutate('rank'=rank(Heart_Failure_rate,na.last = 'keep',ties.method = 'min'))%>%drop_na()
                data3<-data2[data2$rank==rank,c('state','Hospital_name','rank')]
        }else if(outcome=='Pneumonia'){
                data1<-df%>%select(state,Hospital_name,Heart_Attack_rate,Heart_Failure_rate,Pneumonia_rate)
                data2<-data1%>%group_by(state)%>%mutate('rank'=rank(Pneumonia_rate,na.last = 'keep',ties.method = 'min'))%>%drop_na()
                data3<-data2[data2$rank==rank,c('state','Hospital_name','rank')]
        }else{
                stop('Invalid Outcome')
        }
        print(data3)
}


rankall(df = outcome,outcome = 'Heart Failure',rank = 5)

