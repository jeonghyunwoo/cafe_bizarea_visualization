# Cafe Business Area Visualization

공공데이터 기반 카페 상권 시각화 및 매출 영향요인 분석 포트폴리오입니다.

**Live portfolio page**  
https://jeonghyunwoo.github.io/cafe_bizarea_visualization/

**Interactive slide deck**  
https://jeonghyunwoo.github.io/cafe_bizarea_visualization/slides.html

---

## Project summary

This project analyzes cafe store distribution and business-area characteristics using public datasets.

The goal is not only to create an interactive map, but to connect location data, brand footprint, trade-area profiles, and sales-related variables into a business decision-support narrative.

---

## Key numbers

| Item | Value |
|---|---:|
| Cafe store records | 85,528 |
| Major-brand stores | 8,490 |
| Seoul trade-area rows | 1,356 |
| Model validation Kappa | 0.84 |

---

## What this project shows

- Public data preprocessing for business-area analysis
- Geospatial visualization of cafe stores and major brands
- Seoul cafe trade-area sales comparison
- Random Forest-based exploration of high-sales area drivers
- Translation of EDA outputs into business interpretation

---

## Repository structure

```text
.
├── index.html      # Portfolio landing page for GitHub Pages
├── slides.html     # Interactive slide deck
├── README.md       # Project overview
├── .nojekyll       # GitHub Pages setting
├── R/              # Analysis scripts, if kept
└── Rmd/            # Original R Markdown materials, if kept
```

Recommended cleanup:

- Remove old ZIP files from the repository root.
- Keep `index.html` light and professional.
- Keep the full interactive deck as `slides.html`.
- Move source scripts and old materials into clearly named folders.

---

## Portfolio note

This repository is reorganized from an earlier academic presentation into a concise portfolio format.

The framing is: **problem definition → public data → geospatial visualization → trade-area analysis → model-based validation → business implications**.
