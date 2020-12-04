# 경영데이터분석론 발표자료 준비
# 구조: mindmup
# 
library(pacman)
p_load(plyr,tidyverse,data.table,leaflet,clipr)
p_load(scales,kormaps2014)
getwd()
f8<-list.files('data','2018\\d{2}_\\d.csv',full.names=T)
f7<-list.files('data','2017\\d{2}_\\d{2}.csv',full.names=T)
# 2018 data
cafe = data_frame()
cafe7 = data_frame()
for(i in 1:4){
  a = fread(f8[i])
  a = a[grepl('커피',상권업종소분류명),]
  cafe = bind_rows(cafe7,a)
  a = fread(f8[i])
  a = a[grepl('커피',상권업종소분류명),]
  cafe = bind_rows(cafe,a)
}
dim(cafe7)
rm(a)
saveRDS(cafe,'data/cafe8.rds')
saveRDS(cafe7,'data/cafe7.rds')
# 기초 데이터 분석 
cafe<-read_rds('data/cafe8.rds')

names(cafe) %>% print() %>% write_clip()
count(cafe,상권업종대분류명)
chk<-fread(f8[1],select=c(str_c('상권업종',c('대분류명','중분류명','소분류명'))))

count(chk,소분류=상권업종소분류명) %>% print() %>% write_clip()

chk[상권업종대분류명=='음식',.N,상권업종중분류명][order(N,decreasing=T)] %>% print() %>% write_clip()
chk[상권업종중분류명=='커피점/카페',.N,상권업종소분류명][order(N,decreasing=T)] %>% 
  print() %>% write_clip()
chk[grepl('커피',상권업종소분류명),.N,상호명]

# 브랜드 추출
brnd <- c('스타벅스','이디야','투썸플레이스','엔제리너스',
          '요거프레소','카페베네','빽다방','할리스','파스쿠찌',
          '커피베이','폴바셋','커피빈','탐앤탐스','커핀그루나루')
reg_brd<-toString(brnd) %>% str_replace_all(', ','|') %>% str_c('(',.,')')
cafe<-cafe %>% 
  mutate(상호명=tolower(상호명),
         상호명=case_when(str_detect(상호명,'starbucks')~'스타벅스',
                          str_detect(상호명,'coffeebean')~'커피빈',
                          str_detect(상호명,'pascucci')~'파스쿠찌',
                          str_detect(상호명,'paulbassett')~'폴바셋',
                          str_detect(상호명,'twosomeplace')~'투썸플레이스',
                          str_detect(상호명,'angelinus')~'엔젤리너스',
                          str_detect(상호명,'hollys')~'할리스',
                          str_detect(상호명,'cafebene')~'카페베네',
                          TRUE~상호명),
        brand=str_extract(상호명,reg_brd))
# p_load(writexl)
# write_xlsx(head(cafe),'data/cafe18_smp.xlsx')
count(cafe,brand,sort=T) %>% print %>% write_clip()
# 상호명 이상치 탐지 
filter(cafe,is.na(brand)) %>% 
  count(상호명) %>% 
  View
filter(cafe,str_detect(상호명,'(coffebay|yog|cafebene|coffeebean|starbucks|pascu|paul|idiya|twosome|angel|hollys)')) %>% 
  select(상호명) %>% arrange(상호명) %>% View
# 골목상권명 데이터
p_load(sf)
sg<-st_read('data/TBGIS_ALLEY_TRDAR_RELM.shp')
sg<-read_sf('data/TBGIS_ALLEY_TRDAR_RELM.shp')
# 전국 브랜드별 leaflet

# 수도권 브랜드별 leaflet

# 수도권 브랜드별 ggplot