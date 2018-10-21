outcome<-read_csv(file = 'D:/mo2men/R/data sets/Hospitals/outcome-of-care-measures.csv')

#chanching class of some variables

outcome<-outcome%>%mutate(`Hospital 30-Day Death (Mortality) Rates from Heart Attack`=as.numeric(`Hospital 30-Day Death (Mortality) Rates from Heart Attack`),
                          `Hospital 30-Day Death (Mortality) Rates from Heart Failure`=as.numeric(`Hospital 30-Day Death (Mortality) Rates from Heart Failure`),
                          `Hospital 30-Day Death (Mortality) Rates from Pneumonia`=as.numeric(`Hospital 30-Day Death (Mortality) Rates from Pneumonia`))


#select the most important variables

outcome_MR<-outcome%>%
        select(`Hospital Name`,State,
               `Hospital 30-Day Death (Mortality) Rates from Heart Attack`,
               `Hospital 30-Day Death (Mortality) Rates from Heart Failure`,
               `Hospital 30-Day Death (Mortality) Rates from Pneumonia`)%>%drop_na()%>%
        data.frame()

#renaming variables 
outcome_MR<-outcome_MR%>%rename('Heart Attack'=Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                'Hear Failure'=Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                'Pneumonia'=Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)

#ploting heart attack mortality rate 

outcome_MR%>%ggplot(aes(x=`Heart Attack`))+
        geom_histogram()

# building a function which choose the best hosbital 

