# WTC First Responders Frailty Study

**Status:** Ongoing | **Role:** Lead Biostatistician | **Software:** Stata

## Overview

This repository contains the statistical analysis code for a longitudinal study examining frailty trajectories among World Trade Center (WTC) first responders. The study investigates frailty prevalence, predictors, risk factors, and health outcomes in this well-characterized cohort with long-term follow-up data.

## Research Questions

- What is the prevalence of frailty in WTC first responders, and how has it changed over time?
- What are the key predictors and risk factors for frailty in this population?
- How does frailty relate to downstream health outcomes including hospitalizations, functional decline, and mortality?

## Study Design

Longitudinal / repeated measures — participants assessed at multiple time points enabling analysis of frailty onset, progression, and reversal over time.

## Statistical Methods

- Descriptive statistics (frailty prevalence and sample characteristics)
- Longitudinal regression (frailty trajectories over time)
- Mixed effects models (within-person change)
- Survival analysis (time to frailty onset and outcomes)
- Causal inference (effect of WTC exposures on frailty risk)

## Repository Structure

```
wtc-frailty/
├── 01_data_cleaning/       # Data harmonization and cleaning scripts
├── 02_descriptive/         # Descriptive statistics and tables
├── 03_longitudinal/        # Mixed effects and trajectory models
├── 04_survival/            # Time-to-event analyses
├── 05_causal/              # Causal inference models
├── 06_sensitivity/         # Sensitivity and robustness analyses
└── notes/                  # Analytical decision notes and SAP
```

## How to Use

All scripts are written in Stata. Run in order (01 → 06). Each script includes inline comments explaining analytical decisions and model specifications.

## Contact

Yihan Wang, PhD · [honoluluhan717.github.io/yihanwang-site](https://honoluluhan717.github.io/yihanwang-site) · [LinkedIn](https://www.linkedin.com/in/yihan-wang-6a1832294/)
