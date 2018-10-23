library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)


outcome<-read_csv(file = 'D:/mo2men/R/data sets/Hospitals/outcome-of-care-measures.csv',
                  col_types = 'ccccccccccncnnncncnnncncnnncncnnncncnnncncnnnc')

str(outcome)
dim(outcome)
#renaming variables 
names(outcome)<-c('code','Hospital_name',paste('Adress',c(1,2,3)),
                  'city','state','zib_code','country_name','phone_number',
                  paste('Heart_Attack',c('rate','comp','lower','upper','count','footnote'),sep = '_'),
                  paste('Heart_Failure',c('rate','comp','lower','upper','count','footnote'),sep = '_'),
                  paste('Pneumonia',c('rate','comp','lower','upper','count','footnote'),sep = '_'),
                  paste('RHeart_Attack',c('rate','comp','lower','upper','count','footnote'),sep = '_'),
                  paste('RHeart_Failure',c('rate','comp','lower','upper','count','footnote'),sep = '_'),
                  paste('RPneumonia',c('rate','comp','lower','upper','count','footnote'),sep = '_'))


#ploting heart attack mortality rate 

outcome%>%ggplot(aes(x=Heart_Attack_rate))+
        geom_histogram(colour='black',fill='white')+
        theme(panel.background = element_rect(fill = 'white'))+
        xlab('Heart Attack Mortality Rate')+
        ylab("")+
        ggtitle('Frequency of heart attack mortality rate')
        

# building a function which choose the best hosbital 

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

#Example
best(df = outcome,"MD", "Heart Attack")

#building a function to specify the hospitals of certain rank

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


# adding function to rank all according to states

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