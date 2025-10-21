# IHSD 7440 Assignment — Fall 2024

This repository contains my R code and assignment files for **IHSD 7440: Household Survey Design (Fall 2024)**.  
The analysis replicates key steps in estimating malaria-related indicators using complex survey methods applied to the **2007 Zambia Demographic and Health Survey (DHS)**.

---

## 📘 Assignment Overview
The goal of this assignment is to calculate malaria-related indicators consistent with the **Roll Back Malaria (RBM)** initiative framework, using survey weights, clustering, and stratification to produce accurate population estimates.

Indicators analyzed:

- **Household-level:** Proportion of households with at least one insecticide-treated net (ITN)  
- **Child-level:** Proportion of children under age 5 who slept under an ITN the previous night  

Both indicators are disaggregated by:
- Residence (urban/rural)
- Wealth quintile (socioeconomic status)
- Education of household head (household-level)
- Child age and mother’s education (child-level)

---

## 📊 Data Sources
Two datasets were provided through the IHSD 7440 course materials:
- `2007_Zambia_HH_2024.csv` — Household-level DHS data  
- `2007_Zambia_Child_2024.csv` — Child-level DHS data  

These datasets include cluster (PSU), strata, and sample weight variables:
| Variable | Description |
|-----------|-------------|
| `HV005` / `V005` | Sampling weight |
| `HV021` / `V021` | Primary Sampling Unit (PSU) |
| `HV022` / `V022` | Sample stratum number |

---

## ⚙️ Methods

The R script `IHSD_HW.R` performs the full analysis workflow in five main steps for both datasets:

1. **Data Cleaning & Variable Renaming**  
   - Standardizes variable names (e.g., `HV021 → Primary_sampling_unit`)  
   - Creates a weight variable (`pw`) scaled to 1,000,000  

2. **Descriptive Statistics**  
   - Determines number of elements (n), clusters, strata, and domains  

3. **SRS Estimates (Unweighted & Weighted)**  
   - Computes proportions and standard errors under Simple Random Sampling assumptions  

4. **Two-Stage Cluster Sampling (Weighted)**  
   - Incorporates clustering at the PSU level using the `survey` package  

5. **Two-Stage Cluster Sampling (Weighted & Stratified)**  
   - Adds stratification to improve precision  
   - Compares effects of design on estimates and standard errors  

---

## 📦 R Packages Used
The following packages are installed automatically if missing:
```r
data.table
survey
tidyverse
Hmisc
