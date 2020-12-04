# C:/Users/jeong/proj/public_data/R/pub1.r
library(pacman)
p_load(plyr,tidyverse,XML,RCurl,clipr,glue,here)
p_load(tidyverse,XML,data.table)
# 
svc_key = "hz5WTNjJRacuuKnFDp4wzcEw339GCX9K7moiNaSvtHC4J9n8ZMB9hnIa32gi33H4t2MB4qIYqJo8WTQbh9ETJg%3D%3D"
url = str_glue("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/
RTMSOBJSvc/getRTMSDataSvcAptTrade
?serviceKey={svc_key}
&LAWD_CD=11110
&DEAL_YMD=201512") %>% 
  str_remove_all('\n')

a = xmlParse(url) %>% 
  xmlRoot() %>% 
  getNodeSet('//item') %>% 
  xmlToDataFrame(stringsAsFactors = F)
Encoding(names(a))<-'UTF-8'
glimpse(a)
# replicate
cd1<-unique(lawd$cd1)
res<-list()
k<-0
for(i in cd1){
  for(j in c(201709,201809)){
    k = k+1
    url = str_glue("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/
    RTMSOBJSvc/getRTMSDataSvcAptTrade
    ?serviceKey={svc_key}
    &LAWD_CD={i}
    &DEAL_YMD={j}
    &numOfRows=1000") %>% 
      str_remove_all('\n')
    a = xmlParse(url) %>% 
      xmlRoot() %>% 
      getNodeSet('//item') %>% 
      xmlToDataFrame(stringsAsFactors = F)
    Encoding(names(a))<-'UTF-8'
    res[[k]] <- a
    print(i)
  }
  if(i==last(cd1) & j==201809){
    resd<-bind_rows(res)
    print('done')
  }
}
v<-0
resd<-list()
for(i in 1:length(res)){
  v<-v+1
  if(is.data.frame(res[[i]])){
    resd[[v]]<-res[[i]]
  }
}
resd<-bind_rows(resd)
resd<-tbl_df(resd)
resd<-resd %>% 
  mutate_all(~str_remove_all(.,",")) %>% 
  mutate_at(vars(거래금액,건축년도,년,월,전용면적,층),as.numeric)
p_load(DataExplorer)
plot_str(resd)
introduce(resd)
create_report(resd)
glimpse(resd)
resd1<-resd %>% left_join(
  distinct(lawd,cd1,sigu,sido,gungu),
  by=c('지역코드'='cd1')
) %>% distinct()
glimpse(resd1)
dim(resd)
# 
theme_set(theme_minimal())
resd1 %>% 
  group_by(sido) %>% 
  mutate(avg=mean(거래금액)) %>% 
  ungroup() %>% 
  ggplot(aes(factor(년),거래금액,fill=factor(년)))+
  geom_boxplot(color='steelblue',alpha=.5)+
  scale_y_continuous(labels=function(x) x/10000)+
  coord_flip()+
  labs(fill='year')+
  facet_wrap(~sido,ncol=4,scales='free')
