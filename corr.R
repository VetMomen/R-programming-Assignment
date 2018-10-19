corr<-function(directory,threshold=0){
        if(!require(readr)){install.packages('readr')}
        if(!require(magrittr)){install.packages('magrittr')}
        if(!require(dplyr)){install.packages('dplyr')}
        if(!require(tidyr)){install.packages('tidyr')}
        threshold<-threshold+1
        directory<-directory
        setwd(directory)
        temp<-dir(pattern = '*.csv')
        data<-lapply(temp,read_csv)
        data2<-bind_rows(!!!data[1:250])%>%drop_na()
        data3<-data2[threshold:nrow(data2),]
        corr<-with(data3,tapply(1:nrow(data3),ID,function(x){
                cor(nitrate[x],sulfate[x])
        }))
        
}
comp<-corr(directory = 'D:/mo2men/R/data sets/specdata')

summary(comp)