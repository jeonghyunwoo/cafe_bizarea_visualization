source('r/initialize.r')
source('r/funcs.r',encoding='UTF-8')
# data ----
cafe<-read_rds('data/cafe8.rds') # 상호,brand,경도,위도
s1<-read_rds('data/s1.rds') # 상권집계매출 
s2<-read_rds('data/s2.rds') # 상권배후집계매출
s_col_info<-read_rds('data/s_col_info.rds') # s1,s2 항목설명 
addr<-read_rds('data/addr.rds') # 상권주소 (cafe와 s1,s2 링크용)
relm<-read_rds('data/relm.rds')
library(sf)


ggplot(relm)+
  geom_sf(aes(fill=thsmon_selng_amt/1000000))+
  geom_density2d(aes(x=XCNTS_VALU,y=YDNTS_VALU,fill=stat(level)))+
  scale_fill_viridis_c('당월매출(백만)')
quantile(relm$thsmon_selng_amt/100000000,probs=.99,na.rm=T)
relm %>% 
  ggplot(aes(x=XCNTS_VALU,y=YDNTS_VALU,
             size=thsmon_selng_amt/100000000,
             color=thsmon_selng_amt/100000000))+
  geom_point()+
  scale_size_area()+
  guides(color=guide_legend('당월매출(억)'),
         size=guide_legend('당월매출(억)'))+
  ggrepel::geom_text_repel(aes(label=ALLEY_TR_1),size=3,family='ng',
                           color='red',fontface='bold',
                           data = . %>% filter(thsmon_selng_amt>310000000))+
  theme(axis.text = element_blank(),
        axis.title = element_blank())
# 상권의 길이와 면적에 비례하는가?
library(corrr)
select(relm,Shape_Leng,Shape_Area,thsmon_selng_amt) %>% 
  tbl_df() %>% 
  select(-geometry) %>% 
  mutate_all(as.numeric) %>% 
  na.omit() %>% 
  correlate() %>% 
  focus(thsmon_selng_amt)
# A tibble: 2 x 2
# rowname    thsmon_selng_amt
# <chr>                 <dbl>
# 1 Shape_Leng            0.202
# 2 Shape_Area            0.139

# 커피음료 상권----
cafe_area1<-read_rds('data/cafe_area1.rds')
cafe_area1 %>% 
  arrange(desc(thsmon_selng_amt)) %>% 
  transmute(trdar_cd_nm,당월매출_백만=thsmon_selng_amt/1000000,svc_induty_cd_nm)
x<-cafe_area1 %>% 
  select(avrg_bsn_month_co:stor_co) %>% 
  na.omit()
correlate(x) %>% 
  shave() %>% 
  focus(thsmon_selng_amt) %>% 
  arrange(desc(thsmon_selng_amt))
# 상권정보 항목명----
s_col_info %>% filter(gb=='s1') %>% write_clip()
s_col_info %>% filter(gb=='s2') %>% write_clip()
# 시도별 브랜드별 매장수 ----
cafe_n<-count(cafe,시도,sort=T) %>% 
  bind_rows(count(cafe,시도,brand,sort=T)) %>% 
  mutate(brand=coalesce(brand,'total'))
theme_set(theme_minimal(10,'ng'))
cafe_n %>% 
  filter(brand=='total') %>% 
  ggplot(aes(reorder(시도,n),n))+
  geom_col(fill='steelblue')+
  coord_flip()+
  labs(x='',y='매장수')+
  theme(legend.position='none')

# 커피업종 매출 영향요인 분석 ----
p_load(modelgrid,recipes,caret,rsample)
# y : 당월매출금액(THSMON_SELNG_AMT)
# x : MDWK_SELNG_RATE~STOR_CO
# id: 
# split
# target: 매출 상위 10% 여부 
qtl<-quantile(cafe_area1$thsmon_selng_amt,.9)
cafe_area1<-cafe_area1 %>% 
  mutate(target = ifelse(thsmon_selng_amt>=qtl,1,0),
         target = factor(target))
set.seed(7)
spl<-cafe_area1 %>% select(target,trdar_cd_nm,mdwk_selng_rate:stor_co) %>% 
  initial_split(prop=.7,strata='target')
tr<-training(spl)
te<-testing(spl)
saveRDS(te,'data/te.rds')
# skim_to_list(tr) %>% `[[`('numeric') %>% count(missing)
# ggplot(tr,aes(thsmon_selng_amt))+
#   geom_histogram()+
#   geom_vline(xintercept = )
rec<-recipe(tr) %>% 
  add_role(target,new_role='outcome') %>% 
  add_role(trdar_cd_nm,new_role='id variable') %>% 
  add_role(-target,-trdar_cd_nm,new_role='predictor') %>% 
  step_zv(all_predictors()) %>% 
  step_corr(all_predictors(),threshold = .8)

ctrl<-trainControl(method='cv',number=5)
rf<-train(x=rec, data=tr, method='rf',
          tuneLength=5,
          trControl = ctrl,
          metric='Kappa')  
saveRDS(rf,'data/rf.rds')
vi<-varImp(rf)$importance %>% rownames_to_column('var') %>% 
  left_join(transmute(s_col_info,var=tolower(컬럼명),컬럼한글명))
saveRDS(vi,'data/vi.rds')
saveRDS(pred,'data/pred.rds')
theme_set(theme_minimal(10,'ng'))
ggplot(vi,aes(reorder(컬럼한글명,Overall),Overall))+
  geom_col(fill='steelblue')+
  labs(x='',y='importance')+
  coord_flip()
pred<-predict(rf,te)
cf<-confusionMatrix(pred,te$target)
saveRDS(cf,'data/cf.rds')
rf
cafe_area1 %>% 
  top_n(20,thsmon_selng_amt) %>% 
  ggplot(aes(reorder(trdar_cd_nm,thsmon_selng_amt),thsmon_selng_amt))+
  geom_col(fill='firebrick')+
  coord_flip()
# 매출액별 상권(biz_amt)----
biz_amt(1)
biz_amt(3)
biz_amt(5)
biz_amt(6)
# 지역별 브랜드/비브랜드 비중----
cafe %>% 
  filter(시도=='서울')

# 제목: 공공데이터를 활용한 상권정보 시각화
# 1.