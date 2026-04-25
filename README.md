# Cafe Business Area Visualization

공공데이터 기반 카페 상권 시각화 & 매출 영향요인 분석 포트폴리오입니다.

[![Live Demo](https://img.shields.io/badge/Live%20Demo-GitHub%20Pages-10233f?style=for-the-badge)](https://jeonghyunwoo.github.io/cafe_bizarea_visualization/)
[![Tech](https://img.shields.io/badge/R-leaflet%20%7C%20sf%20%7C%20randomForest-c03221?style=for-the-badge)](#)

## What this project shows

- 전국 카페 매장 위치 데이터를 활용한 인터랙티브 지도 시각화
- 주요 카페 브랜드의 전국/수도권 커버리지 비교
- 서울시 카페 상권 매출 규모 및 고매출 상권 분석
- Random Forest 기반 매출 상위권 영향요인 탐색

## Key numbers

| Area | Summary |
|---|---:|
| Cafe store records | 85,528 |
| Major brand stores | 8,490 |
| Seoul trade-area rows | 1,356 |
| Model validation | Kappa 0.84 |

## Repository structure

```text
.
├── index.html   # GitHub Pages에서 바로 열리는 포트폴리오 슬라이드
└── README.md    # 프로젝트 설명
```

## How to publish on GitHub Pages

1. 이 저장소 루트에 `index.html`과 `README.md`를 업로드합니다.
2. GitHub repo에서 `Settings → Pages`로 이동합니다.
3. `Build and deployment`에서 `Deploy from a branch`를 선택합니다.
4. Branch는 `main`, folder는 `/root`로 설정합니다.
5. 몇 분 후 아래 주소에서 확인합니다.

```text
https://jeonghyunwoo.github.io/cafe_bizarea_visualization/
```

## Portfolio note

기존 발표자료를 그대로 올리기보다, 포트폴리오 관점에서 문제정의 → 데이터 → 지도 시각화 → 상권 매출 분석 → 모델 검증 순서로 재구성했습니다.
