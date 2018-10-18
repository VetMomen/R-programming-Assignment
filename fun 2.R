

complete0<-function(directory,id){
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
        data3<-bind_rows(!!!data2)
        data4<-data3%>%drop_na()%>%group_by(ID)%>%summarize(nobs=n())
        return(data4)
}
complete0(directory = 'D:/mo2men/R/data sets/specdata',id = 1:250)
