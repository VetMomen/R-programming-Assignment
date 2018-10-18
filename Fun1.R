polutantmean<-function(directory,polutant,id){
        if(!require(readr)){install.packages('readr')}
        if(!require(magrittr)){install.packages('magrittr')}
        if(!require(dplyr)){install.packages('dplyr')}
        if(!require(tidyr)){install.packages('tidyr')}
        directory<-directory
        setwd(directory)
        temp<-dir(pattern = '*.csv')
        data<-lapply(temp,read_csv)
        idx<-id
        data2<-data[idx]
        data3<-bind_rows(data2)
        mean0<-data3%>%select(polutant)%>%drop_na()%>%colMeans()
        mean0<-round(mean0,3)
        return(paste('the mean of',polutant,'is',mean0))
}

