library(pacman)
p_load(tidyverse,leaflet,clipr,scales,data.table)
p_load(sf)

list.files('z:/proj/pub_info/data/sl_1807')
list.files('z:/proj/pub_info/data')

# data1 : cafe.rds (pub_info/data)

# 상권집계매출
s1<-data.table::fread('data/raw/tbsm_trdar_selng.txt',skip=1,
                      encoding='UTF-8') %>% 
  as_tibble()
s2<-data.table::fread('data/raw/tbsm_trdhl_selng.txt',skip=1,
                      encoding='UTF-8') %>% 
  as_tibble()
# 항목명 
library(readxl)
vrn<-read_xlsx('data/raw/1-2.테이블 및 필드 정의서_180402.xlsx',sheet=2)
vrn<-fill(vrn,No:데이터한글명)
names(s1)<-filter(vrn,tolower(데이터명)=='tbsm_trdar_selng') %>% 
  pull(컬럼명)
names(s2)<-filter(vrn,tolower(데이터명)=='tbsm_trdhl_selng') %>% 
  pull(컬럼명)
# s1,s2
s1<-mutate_if(s1,is.numeric,as.numeric) %>% 
  select_all(tolower)
s2<-mutate_if(s2,is.numeric,as.numeric) %>% 
  select_all(tolower)
# 저장
saveRDS(s1,'data/s1.rds')
saveRDS(s2,'data/s2.rds')
s_col_info<-filter(vrn,tolower(데이터명) %in% c('tbsm_trdar_selng',
                                                'tbsm_trdhl_selng')) %>% 
  select(데이터명,데이터한글명,컬럼한글명=컬럼럼한글명,컬럼명,컬럼타입)
s_col_info<-s_col_info %>% 
  mutate(gb=ifelse(str_detect(데이터명,'TBSM_TRDAR'),'s1','s2'))
saveRDS(s_col_info,'data/s_col_info.rds')

# 주소
file.rename('data/raw/5-2.골목상권별행정동코드_180402.xls',
            'data/raw/addr.xls')
addr<-read_xls('data/raw/addr.xls',sheet=1)
addr<-select_all(addr,tolower)
saveRDS(addr,'data/addr.rds')
# cafe만들기 
f8<-list.files('data','2018\\d{2}_\\d.csv',full.names=T)
f7<-list.files('data','2017\\d{2}_\\d{2}.csv',full.names=T)
# 2018 data
cafe = data_frame()
cafe7 = data_frame()
for(i in 1:4){
  a = fread(f7[i]) 
  a = a[grepl('커피',상권업종소분류명),]
  cafe7 = bind_rows(cafe7,a)
  a = fread(f8[i])
  a = a[grepl('커피',상권업종소분류명),]
  cafe = bind_rows(cafe,a)
}

library(kormaps2014)
preproc_df<-function(df){
  # 브랜드 추출
  brnd <- c('스타벅스','이디야','투썸플레이스','엔제리너스',
            '요거프레소','카페베네','빽다방','할리스','파스쿠찌',
            '커피베이','폴바셋','커피빈','탐앤탐스','커핀그루나루')
  reg_brd<-toString(brnd) %>% str_replace_all(', ','|') %>% str_c('(',.,')')
  df<-df %>% 
    mutate(상호명=tolower(상호명),
           상호명1=case_when(str_detect(상호명,'starbucks')~'스타벅스',
                             str_detect(상호명,'coffeebean')~'커피빈',
                             str_detect(상호명,'pascucci')~'파스쿠찌',
                             str_detect(상호명,'paulbassett')~'폴바셋',
                             str_detect(상호명,'twosomeplace')~'투썸플레이스',
                             str_detect(상호명,'angelinus')~'엔젤리너스',
                             str_detect(상호명,'hollys')~'할리스',
                             str_detect(상호명,'cafebene')~'카페베네',
                             TRUE~상호명),
              brand=str_extract(상호명1,reg_brd))
  
  # 카페트 제거
  df<-filter(df,!str_detect(상호명,'카페트$'))
  
  # addr: 주소 
  sido<-areacode %>% changeCode() %>% 
    select(시도명=name,시도=name1,-code)
  if(Encoding(df$시도명[1])!='UTF-8') {
    df<-df %>% mutate(시도명=enc2utf8(시도명))
  }
  df<-df %>% 
    left_join(sido,by='시도명') %>% 
    mutate(addr=str_c(시도,시군구명,행정동명,도로명,sep=' '))
  df<-df %>% 
    mutate(brand=coalesce(brand,'기타'),
           brandyn = ifelse(brand=='기타',brand,'브랜드'))
  return(df)
}
cafe<-preproc_df(cafe)
cafe7<-preproc_df(cafe7)
saveRDS(cafe,'data/cafe8.rds')
cafe7 %>% left_join(sido,by='시도명')
# 
source('c:/r/myconfig.r')

# 테이블 설명서 
# data/raw/2018년07월_데이터수급적재내역 및 프로파일링데이터 서비스 내역.xlsx
tbl_info<-read_clip_tbl()
p_load(googlesheets)
gs_ls()
gt<-gs_title('테이블설명서')
tbl_info<-gt %>% gs_read(ws='2018',range='b2:f90')
tbl_info<-tbl_info %>% fill(그룹)
saveRDS(tbl_info,'data/tbl_info.rds')

# 항목명 설명서
p_load(googlesheets)
gg<-gs_title("1-2.테이블 및 필드 정의서_180402")
col_info<-gg %>% gs_read(ws=1)
col_info<-col_info %>% fill(No:데이터한글명)
saveRDS(col_info,'data/col_info.rds')

# 테이블읽기
# 현존하는 테이블명
istable<-str_remove_all(txts,'(data/raw/|.txt)')
tbl_info %>% filter(물리명 %in% istable)
tbl_info
a<-data.table::fread('data/raw/TBDW_SOPSRT_BSSH_INFO_NEW.txt')
# data/raw의 모든 테이블 읽기
p_load(data.table,DataExplorer,data.tree,networkD3)
(txts<-list.files('data/raw','.txt',full.name=T))
cols<-str_remove_all(txts,'(data/raw/|.txt)')
golmok<-list()
i<-0
for(f in txts){
  i<-i+1
  col_name<-filter(col_info,데이터명==cols[i]) %>% pull(컬럼명)
  tryCatch({golmok[[i]]<-fread(f,encoding='UTF-8')},
           warning = function(w) {
             print(str_glue('warning occured at {f}'))
             })
  print(i)
}
plot_str(golmok)

# shp 
# sf
relm<-st_read('data/raw/TBGIS_ALLEY_TRDAR_RELM.shp') %>% 
  left_join(
    transmute(cafe_area1,thsmon_selng_amt,ALLEY_TRDA=str_pad(trdar_cd,width=6,pad='0'))
  )
saveRDS(relm,'data/relm.rds')

# cafe_area
cafe_area1<-filter(s1,str_detect(svc_induty_cd_nm,'커피음료'))
cafe_area2<-filter(s2,str_detect(svc_induty_cd_nm,'커피음료'))
saveRDS(cafe_area1,'data/cafe_area1.rds')
saveRDS(cafe_area2,'data/cafe_area2.rds')