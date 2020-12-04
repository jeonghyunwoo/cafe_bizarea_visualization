# c:/users/jeong/proj/public_data/r/상권분석.r
library(pacman)
p_load(plyr,tidyverse,XML,leaflet,data.table,scales)
source('c:/r/myfunc.r',encoding='cp949')
# 상권정보업종 코드집 
fn<-list.files('data','.csv',full.names = T)
info<-fread(fn) %>% tbl_df
# 
zfn<-list.files('data','.zip',full.names=T)
unzip(zfn[1],exdir = 'data')
unzip(zfn[2],exdir = 'data')
fns1<-list.files('data','\\w.*2017\\d{2}\\_',full.names = T)
fns2<-list.files('data','\\w.*2018\\d{2}\\_',full.names = T)
library(tictoc)
tic()
df1<-data_frame()
for(f in fns1){
  a<-fread(f)
  df1<-bind_rows(df1,a)
}
df2<-list();i=0
for(f in fns2){
  i=i+1
  df2[[i]]<-fread(f)
}
toc()
df2<-rbindlist(df2)

saveRDS(df2,'data/sangwon1809.rds')
rm(df2)
gc()
names(df1)

df2<-read_rds('data/sangwon1809.rds')
sl<-read_rds('data/sangwon1809.rds') %>% 
  # filter(str_detect(시도명,enc2utf8('서울'))) %>% 
  mutate_if(is.character,trimws) %>% 
  select(1:19,층정보,경도,위도)
# saveRDS(sl,'data/sl.rds')
setDT(sl)
saveRDS(sl,'data/sl.rds')
setkey(sl,상권업종중분류명)
# sl[상권업종대분류명=='음식',.N,상권업종중분류명]
sl<-read_rds('data/sl.rds')
caffe<-sl[str_detect(상권업종소분류명,'카페') &
          str_detect(상호명,'(스타벅스|커피빈|폴바셋|투썸플레이스|엔제리너스|이디야|카페베네)'),]
caffe[,frc:=str_extract(상호명,'(스타벅스|폴바셋|투썸플레이스|엔제리너스|커피빈|이디야|카페베네)')]
saveRDS(caffe,'data/caffe.rds')
# 상호별로 나누어 그룹으로 분석해보기 
caffe<-read_rds('data/caffe.rds')
#
sl_caffe<-caffe[str_detect(enc2utf8(시도명),'부천'),]
setkey(sl_caffe,frc)
sl_caffe<-caffe
setkey(sl_caffe,frc)
#
library(leaflet)
chf<-function(x){
  s=sl_caffe[x,]
  s=s[chull(s$경도,s$위도)]
  return(s)
}
spot<-function(x){
  sl_caffe[x,]
}
sbux<-chf('스타벅스')
tsum<-chf('투썸플레이스')
agel<-chf('엔제리너스')
idya<-chf('이디야')
bene<-chf('카페베네')
cbean<-chf('커피빈')
pbas<-chf('폴바셋')
# bbck<-chf('빽다방')
library(scales)
colqry('brown') %>% show_col()
sl_caffe %>% 
  leaflet() %>% 
  addTiles(group='OSM (default)') %>% 
  addProviderTiles(providers$Stamen.Watercolor, group = "WaterColor") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  # addPolygons(data=bbck,lng=~경도,lat=~위도,color='gold3',fillcolor='yellow',group='빽다방') %>% 
  addPolygons(data=chf('투썸플레이스'),lng=~경도,lat=~위도,color='black',fillColor='grey',group='투썸플레이스') %>% 
  addPolygons(data=chf('스타벅스'),lng=~경도,lat=~위도,color='#00704a',fillColor='#00704a',group='스타벅스') %>% 
  addPolygons(data=chf('커피빈'),lng=~경도,lat=~위도,color='brown',fillColor='brown',group='커피빈') %>% 
  addPolygons(data=chf('폴바셋'),lng=~경도,lat=~위도,color='saddlebrown',fillColor='saddlebrown',group='폴바셋') %>% 
  addPolygons(data=chf('이디야'),lng=~경도,lat=~위도,color='#b87333',fillColor='#b87333',group='이디야') %>% 
  addPolygons(data=chf('카페베네'),lng=~경도,lat=~위도,color='red',fillColor='red',group='카페베네') %>% 
  # addCircles(data=spot('빽다방'),lng=~경도,lat=~위도,color='gold3',group='빽다방') %>% 
  addCircles(data=spot('투썸플레이스'),lng=~경도,lat=~위도,color='black',group='투썸플레이스') %>% 
  addCircles(data=spot('스타벅스'),lng=~경도,lat=~위도,color='#00704a',group='스타벅스') %>% 
  addCircles(data=spot('커피빈'),lng=~경도,lat=~위도,color='firebrick',group='커피빈') %>%
  addCircles(data=spot('폴바셋'),lng=~경도,lat=~위도,color='saddlebrown',group='폴바셋') %>% 
  addCircles(data=spot('이디야'),lng=~경도,lat=~위도,color='#b87333',group='이디야') %>% 
  addCircles(data=spot('카페베네'),lng=~경도,lat=~위도,color='red',group='카페베네') %>% 
  addLayersControl(baseGroups = c('OSM (default)','WaterColor','Toner Lite'),
                   overlayGroups = c('스타벅스','커피빈','폴바셋','이디야','카페베네','빽다방','투썸플레이스'),
                   options=layersControlOptions(collapsed=F))
# 함수로 만듬----
caffe<-caffe %>% mutate_if(is.numeric,as.numeric) %>% 
  mutate_if(is.character,~str_conv(.,encoding='UTF-8'))
cafem<-function(x){
  sl_caffe<-caffe[str_detect(enc2utf8(시도명),x)|str_detect(enc2utf8(시군구명),x),]
  setkey(sl_caffe,frc)
  chf<-function(x){
    s=sl_caffe[x,]
    if(!is.na(s$상호명[1])) {
      s=s[chull(s$경도,s$위도)]
    }
    return(s)
  }
  spot<-function(x){
    sl_caffe[x,]
  }
  sl_caffe %>% 
    leaflet() %>% 
    addTiles(group='OSM (default)') %>% 
    addProviderTiles(providers$Stamen.Watercolor, group = "WaterColor") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    # addPolygons(data=bbck,lng=~경도,lat=~위도,color='gold3',fillcolor='yellow',group='빽다방') %>% 
    addPolygons(data=chf('투썸플레이스'),lng=~경도,lat=~위도,color='black',fillColor='grey',group='투썸플레이스') %>% 
    addPolygons(data=chf('스타벅스'),lng=~경도,lat=~위도,color='#00704a',fillColor='#00704a',group='스타벅스') %>% 
    addPolygons(data=chf('커피빈'),lng=~경도,lat=~위도,color='brown',fillColor='brown',group='커피빈') %>% 
    addPolygons(data=chf('폴바셋'),lng=~경도,lat=~위도,color='saddlebrown',fillColor='saddlebrown',group='폴바셋') %>% 
    addPolygons(data=chf('이디야'),lng=~경도,lat=~위도,color='#b87333',fillColor='#b87333',group='이디야') %>% 
    addPolygons(data=chf('카페베네'),lng=~경도,lat=~위도,color='red',fillColor='red',group='카페베네') %>% 
    # addCircles(data=spot('빽다방'),lng=~경도,lat=~위도,color='gold3',group='빽다방') %>% 
    addCircles(data=spot('투썸플레이스'),lng=~경도,lat=~위도,color='black',group='투썸플레이스',label=~str_c(상호명,지점명,sep='/')) %>% 
    addCircles(data=spot('스타벅스'),lng=~경도,lat=~위도,color='#00704a',group='스타벅스',label=~str_c(상호명,지점명,sep='/')) %>%
    # addMarkers(data=spot('스타벅스'),lng=~경도,lat=~위도,group='스타벅스',icon=sbukicon) %>% 
    addCircles(data=spot('커피빈'),lng=~경도,lat=~위도,color='firebrick',group='커피빈',label=~str_c(상호명,지점명,sep='/')) %>%
    addCircles(data=spot('폴바셋'),lng=~경도,lat=~위도,color='saddlebrown',group='폴바셋',label=~str_c(상호명,지점명,sep='/')) %>% 
    addCircles(data=spot('이디야'),lng=~경도,lat=~위도,color='#b87333',group='이디야',label=~str_c(상호명,지점명,sep='/')) %>% 
    addCircles(data=spot('카페베네'),lng=~경도,lat=~위도,color='red',group='카페베네',label=~str_c(상호명,지점명,sep='/')) %>% 
    addLayersControl(baseGroups = c('OSM (default)','WaterColor','Toner Lite'),
                     overlayGroups = c('스타벅스','커피빈','폴바셋','이디야','카페베네','투썸플레이스'),
                     options=layersControlOptions(collapsed=F))
}

# test----
cafem('서울')
cafem('부산')
cafem('경기')
cafem('부천')
# icon
sbukicon<-iconList(
  sbuk=makeIcon('data/sbux_icon.png',
                iconWidth = 20,iconHeight = 20)
)
