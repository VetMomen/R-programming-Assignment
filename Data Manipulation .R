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
        out<-c('Heart Attack','Heart Failure','Pneumonia')
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
best(df = outcome,state = 'NY',outcome = 'Pneumonia')
