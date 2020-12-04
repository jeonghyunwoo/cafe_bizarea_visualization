# C:/Users/09925296/proj/pub_info/R/

library(pacman)
p_load(plyr,tidyverse,XML,jsonlite,clipr)
# url생성함수
urlf<-function(x){
  if(any(class(x)=='glue')) res = str_remove_all('\n')
  else res = str_glue(x) %>% str_remove_all('\n')
  return(str_remove_all(res,' '))
}

# SGIS 데이터 API
sgis_svc_id = "821ad980f33144d4a746"
sgis_svc_key = "89eb5842f7734cec846c"
library(jsonlite)
library(tidyverse)
# accessToken받기 
url= str_glue("https://sgisapi.kostat.go.kr/OpenAPI3/auth/authentication.json?
              consumer_key={sgis_svc_id}
              &consumer_secret={sgis_svc_key}") %>% 
                str_remove_all(.,'\n')
data = fromJSON(url)
token<-data$result$accessToken
# 소상공인 업종별 사업체증감 API
adrs="https://sgisapi.kostat.go.kr/OpenAPI3/startupbiz/corpindecrease.json"

url = str_glue("{adrs}?accessToken={token}&adm_cd={11110}")
data= fromJSON(url) %>% 
  `[[`('result') %>% 
  as_tibble() %>% 
  unnest() %>% 
  left_join(distinct(lawd,cd1,sido,gungu),
            by=c('adm_cd'='cd1'))
# theme_cd: 테마코드(업종코드)
# theme_nm: 테마명(업종명)
# corp_cnt: 사업체수
count(data,theme_nm) %>% View # 62
count(data,sido)
count(data,year)
# 편의점수 
# 스타벅스 그린: #00704a
# 스타벅스 화이트: #ffffff
# 스타벅스 주제로 색깔제조
library(RColorBrewer)
sbux<-colorRampPalette(c('#00704a','#ffffff'))

filter(data,theme_nm=='편의점') %>% 
  ggplot(aes(year,corp_cnt))+
  geom_col(fill='#00704a')

filter(data,theme_nm %in% c('편의점','카페','한식')) %>% 
  mutate(year=as.numeric(year)) %>% 
  ggplot(aes(year,corp_cnt,fill=theme_nm))+
  # geom_line()+
  geom_col()+
  scale_fill_manual(values=sbux(5)[1:3])+
  facet_wrap(~theme_nm,ncol=1,scales='free_y')+
  theme_minimal()+
  theme(legend.position = 'none')
  
# 주소-->좌표 : 지오코딩
library(leaflet)
address="중동로 108" 
fileurl = urlf("https://sgisapi.kostat.go.kr/OpenAPI3/addr/geocode.json
?accessToken={token}
&address={str_replace_all(address,' ','%20')}")
a<-fromJSON(fileurl) %>% 
  `[[`('result') %>% 
  `[[`('resultdata') %>% 
  mutate_at(vars(x,y),as.numeric) %>% 
  as_tibble()
  
# UTM-K (GRS80) => EPSG:5179
# BESSEL 경/위도 => EPSG:4004
# 좌표변환(경도/위도로)
coord_res<-list()
for(i in 1:nrow(a)){
  x = a$x[i]
  y = a$y[i]
  coord = str_glue("https://sgisapi.kostat.go.kr/OpenAPI3/transformation/transcoord.json
             ?accessToken={token}
             &src=EPSG:5179
             &dst=EPSG:4004
             &posX={x}
             &posY={y}") %>% 
          str_remove_all("(\b|\n)")
  ret<-fromJSON(coord)
  a$lng[i]<-ret$result[['posX']]
  a$lat[i]<-ret$result[['posY']]
}
a %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircles(lng=~lng,lat=~lat)
