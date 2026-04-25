# Cafe Business Area Visualization

공공데이터의 상업적 활용방안 탐색을 목적으로 한 카페 상권 시각화 및 매출 영향요인 분석 포트폴리오입니다.

## Project purpose

- 공공데이터 기반 상업적 활용 가능성 탐색
- 카페 매장 위치정보의 지도화 및 브랜드 커버리지 비교
- 서울 상권 프로파일링 기반 매출 영향요인 분석
- 매출 상위권 상권의 설명요인 탐색

## What this project demonstrates

- 전국 카페 매장 위치 데이터를 활용한 인터랙티브 지도 시각화
- 주요 카페 브랜드의 전국 및 수도권 커버리지 비교
- 서울시 카페 상권 매출 규모 및 고매출 상권 분석
- Random Forest 기반 매출 상위권 영향요인 탐색

## Key numbers

| Item | Value |
|---|---:|
| Cafe store records | 85,528 |
| Major brand store records | 8,490 |
| Seoul trade-area rows | 1,356 |
| Model validation Kappa | 0.84 |

## Repository structure

```text
.
├── index.html
├── README.md
├── .nojekyll
└── Rmd/pres_1112_jhw_files/figure-html/
```

`index.html`에서 기존 Rmd 폴더의 원본 시각화 이미지를 참조합니다. 따라서 `Rmd/pres_1112_jhw_files/figure-html/` 폴더는 삭제하지 않는 것이 좋습니다.

## Portfolio note

기존 수업 발표자료를 그대로 노출하지 않고, 포트폴리오 관점에서 목적 → 데이터 → 위치 시각화 → 상권 매출 분석 → 모형 검증 → 상업적 활용 가능성 순서로 재구성했습니다.
