# funcs.R (after analysis.R)

# 시군구,읍면동별 데이터 산출 
# src: cafe$도로명 addr$alley_trdar_nm 매칭함수

# cafe$도로명과에 상권명(addr$alley_trdar_nm) 매칭 ----

# subc: cafe의 주소지로 서브데이터생성 함수---- 
subc<-function(x=NULL){ # x: 시도,군구,읍면동 아무거나
  if(is.null(x)) cafe
  else filter(cafe,str_detect(addr,x))
}

# basemap----
basemap<-function(df){
  df %>% 
    leaflet() %>% 
    addTiles(group='OSM (default)') %>% 
    addProviderTiles(providers$OpenMapSurfer.Grayscale,group='grey') %>% 
    addProviderTiles(providers$Stamen.TonerLite,group='toner') %>% 
    addProviderTiles(providers$Stamen.Watercolor,group='watercol')
}
# nullmap----
nullmap<-function(){
  leaflet() %>% 
    addTiles(group='OSM (default)') %>% 
    addProviderTiles(providers$OpenMapSurfer.Grayscale,group='grey') %>% 
    addProviderTiles(providers$Stamen.TonerLite,group='toner') %>% 
    addProviderTiles(providers$Stamen.Wat,ercolor,group='watercol')
}

# 지역별 brand vs 기타---- 

reg_all<-function(x=NULL,n=NULL){
  srcd<-subc(x)
  if(!is.null(n)) srcd<-srcd %>% sample_n(n)
  pal<-colorFactor(c('darkorange4','#00704a'),domain=srcd$brandyn)
  # pal<-colorFactor('Reds',domain=srcd$brandyn)
  basemap(srcd) %>% 
    addCircles(data=filter(srcd,brandyn=='브랜드'),~경도,~위도,color=~pal(brandyn),
               group = '브랜드',
               popup=~str_c(str_c("<b><a>",상호명,"</a></b>"),addr,sep='<br/>')) %>% 
    addCircles(data=filter(srcd,brandyn=='기타'),~경도,~위도,color=~pal(brandyn),
               group = '기타',
               popup=~str_c(str_c("<b><a>",상호명,"</a></b>"),addr,sep='<br/>')) %>% 
    addLegend('bottomright',pal=pal,values=~brandyn,title='',opacity=1) %>% 
    addLayersControl(baseGroups = c('watercol','grey','OSM (default)'),
                     overlayGroups = c('브랜드','기타'),
                     options = layersControlOptions(collapse=F))
}
# reg_all('인천')

# 지역별 brand별 (기타제외)----
reg_brand<-function(x=NULL){
  srcd<-subc(x) %>% filter(brandyn=='브랜드')
  pal<-colorFactor('Spectral',domain=srcd$brand)
  basemap(srcd) %>% 
    addCircles(~경도,~위도,color=~pal(brand),popup=~str_c(str_c("<b><a>",상호명,"</a></b>"),addr,sep='<br/>')) %>% 
    addLegend('bottomright',pal=pal,values=~brand,title='',opacity=1) %>% 
    addLayersControl(baseGroups = c('OSM (default)','grey','toner','watercol'))
}

# 지역별 주요brand polygon (기타제외): reg_brand_poly ----

# brpal 정의
library(RColorBrewer)
brpal<-function(p,n=NULL){
  maxcol<-brewer.pal.info 
  row.names(maxcol)<-row.names(maxcol) %>% tolower()
  p<-tolower(p)
  maxcol<-maxcol[p,'maxcolors']
  bp<-brewer.pal(maxcol,'Spectral')
  colorRampPalette(bp)(n)
}
# 브랜드별 색깔지정(브랜드명으로 인덱싱)
brand_name<-count(cafe,brand,sort=T) %>%
  filter(brand!='기타') %>% pull(brand)
colb<-brpal('spectral',14)
# palettes_d_names %>% filter(length>=length(brand_name))
# colsel<-palettes_d_names %>% filter(length>=length(brand_name)) %>% 
#   sample_n(1)
# colb<-paletteer_d(!!colsel$package,!!colsel$palette,14)
# show_col(colb)
colc<-function(x) {
  colb[which(brand_name==x)]
}

# sub함수1: 브랜드별 marker용 데이터
mkrdt<-function(x=NULL,srcd){
  # srcd는 메인함수에서 생성 
  srcd %>% filter(str_detect(brand,x)) %>% 
    select(경도,위도,brand,addr,상호명)
}
# sub함수2: 브랜드별 polygon용 데이터 
poldt<-function(x=NULL,srcd){
  srcd %>% filter(str_detect(brand,x)) %>% 
    slice(chull(.$경도,.$위도)) %>% 
    select(경도,위도)
}
# head tail 함수 
ht<-function(x,n){
  hn<-ceiling(n/2)
  c(head(x,hn),tail(x,n-hn))
}

# main함수: reg_brand_poly
brand_name<-count(cafe,brand,sort=T) %>%
  filter(brand!='기타') %>% pull(brand) %>% c(.,'기타') 
reg_brand_poly<-function(x=NULL,n=15){
  srcd<-subc(x) %>% 
    mutate(brand = fct_relevel(brand,brand_name)) %>% 
    filter(brand %in% c(brand_name[1:n],'기타')) %>% 
    mutate(brand=fct_drop(brand))
  exist_brand<-levels(srcd$brand)
  
  resmap<-basemap(srcd)
  
  set.seed(7)
  colb<-ht(brpal('spectral',n*2),n) %>% sample()
  colc<-function(x) colb[which(brand_name==x)]
  pal<-colorFactor(colb,domain=exist_brand)
  
  for(bn in exist_brand){
    resmap<-resmap %>% 
      addPolygons(data=poldt(bn,srcd),lng=~경도,lat=~위도,
                  # color = colc(bn),
                  # fillColor=colc(bn),
                  color = ~pal(bn),
                  fillColor=~pal(bn),
                  group=str_c('{',bn,'}'))
  }
  for(bn in exist_brand[-length(exist_brand)]){
    resmap<-resmap %>% 
      addCircles(data=mkrdt(bn,srcd),lng=~경도,lat=~위도,
                 # color=colc(bn),
                 color = ~pal(bn),
                 popup=~str_c(str_c('<b><a>',상호명,'</a></b>'),
                              addr,sep='</br>'),
                 group=bn)
  }
  resmap %>% 
    addLegend('bottomleft',data=srcd,pal=pal,values=~brand,title='',opacity=1) %>% 
    addLayersControl(baseGroups = c('OSM (default)','grey','toner','watercol'),
                     overlayGroups = c(exist_brand[-length(exist_brand)],str_c('{',exist_brand,'}')),
                     options = layersControlOptions(collapse=F)) %>% 
    hideGroup(c(exist_brand[-1],str_c('{',exist_brand,'}')[-1]))
}

reg_brand_poly_nopts<-function(x=NULL,n=15){
  srcd<-subc(x) %>% 
    mutate(brand = fct_relevel(brand,brand_name)) %>% 
    filter(brand %in% c(brand_name[1:n],'기타')) %>% 
    mutate(brand=fct_drop(brand))
  exist_brand<-levels(srcd$brand)
  
  resmap<-basemap(srcd)
  
  set.seed(7)
  colb<-ht(brpal('spectral',n*2),n) %>% sample()
  colc<-function(x) colb[which(brand_name==x)]
  pal<-colorFactor(colb,domain=exist_brand)
  
  for(bn in exist_brand){
    resmap<-resmap %>% 
      addPolygons(data=poldt(bn,srcd),lng=~경도,lat=~위도,
                  # color = colc(bn),
                  # fillColor=colc(bn),
                  color = ~pal(bn),
                  fillColor=~pal(bn),
                  group=str_c('{',bn,'}'))
  }
  resmap %>% 
    addLegend('bottomleft',data=srcd,pal=pal,values=~brand,title='',opacity=1) %>% 
    addLayersControl(baseGroups = c('OSM (default)','grey','toner','watercol'),
                     overlayGroups = c(str_c('{',exist_brand,'}')),
                     options = layersControlOptions(collapse=F)) %>% 
    hideGroup(c(exist_brand[-1],str_c('{',exist_brand,'}')[-1]))
}
# reg_brand_poly('울릉')
# reg_brand_poly('(서울|경기|인천)',10)
# reg_brand_poly('서울',10)
# reg_brand_poly('강남구',10)
# reg_brand_poly('서대문구',10)
# reg_brand_poly('성북구',10)
# reg_brand_poly('종로구',10)

# 매출액대별 상권----
# quantile(relm$thsmon_selng_amt,probs=c(.5,.9,.99),na.rm=T)/100000000
# 50%       90%       99% 
# 0.2577991 1.0800001 3.1926415 
# mean(relm$thsmon_selng_amt,na.rm=T)/100000000
# 0.4679272
biz_amt<-function(amt=1){
  amt<-amt*100000000 # 억단위 
  
  relm %>% 
    ggplot(aes(x=XCNTS_VALU,y=YDNTS_VALU,
               size=thsmon_selng_amt/100000000,
               color=thsmon_selng_amt/100000000))+
    geom_point()+
    scale_size_area()+
    guides(color=guide_legend('당월매출(억)'),
           size=guide_legend('당월매출(억)'))+
    ggrepel::geom_label_repel(aes(label=ALLEY_TR_1),size=3,family='ng',
                             color='firebrick',fontface='bold',
                             data = . %>% filter(thsmon_selng_amt>=amt))+
    theme(axis.text = element_blank(),
          axis.title = element_blank())
}
