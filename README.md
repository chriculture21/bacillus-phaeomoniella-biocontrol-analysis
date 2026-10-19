# *Bacillus halotolerans* biocontrol of *Phaeomoniella chlamydospora* in grapevine propagation material

R scripts for the statistical analyses and figures of the manuscript:

> **Evaluation of a grapevine endophytic *Bacillus halotolerans* strain as a potential biocontrol agent: effects on *Phaeomoniella chlamydospora* infecting propagation material using a nursery practical application**
>
> Tsoukas C. et al. *Manuscript submitted for publication.*

The citation and DOI will be added here once the article is published.

## Overview

The study evaluated endophytic bacterial strains (ba4, ba5, ba11, ba24 and their consortium) against *Phaeomoniella chlamydospora* in grapevine propagation material, followed by a field trial of the most promising strain, *Bacillus halotolerans* ba4.

The scripts cover:

- **Nursery experiments**, analysed as two experimental groups:
  - **Experimental group 1:** vines artificially inoculated with *P. chlamydospora* (bacterial treatments compared with the pathogen-only control, *Pch*)
  - **Experimental group 2:** naturally infected vines (bacterial treatments compared with the untreated control)
- **Field trial:** ba4 compared with an untreated control

Response variables: area under the disease progress curve (AUDPC), length of vascular discoloration, and fungal biomass quantified by qPCR.

## Repository contents

| Script | Description |
|---|---|
| `Statistical_analysis.R` | Linear mixed-effects models for the nursery experiments: model fitting, assumption checks, model comparison (equal vs. treatment-specific variances), F-tests and Dunnett's comparisons against the control |
| `Manuscript_plots.R` | Bar plots (mean ± SE) of AUDPC, vascular discoloration and fungal biomass for both experimental groups, annotated with significance levels from Dunnett's tests |
| `Field_trial_statistical_analysis.R` | Randomised field layout (agricolae), linear models for the field trial, and the combined field-trial figure |

## Statistical approach

**Nursery experiments**

- Each experimental group was analysed separately with linear mixed-effects models (`nlme::lme`), with treatment as a fixed effect and block as a random intercept, fitted by REML.
- AUDPC was log(x + 1)-transformed; vascular discoloration and fungal biomass were analysed on the original scale.
- Model assumptions were assessed on normalized residuals with Q–Q and residual plots, the Shapiro–Wilk test (normality) and the Brown–Forsythe test (homogeneity of variance).
- Where residual variances differed among treatments, a treatment-specific variance structure (`varIdent`) was fitted and compared with the equal-variance model by likelihood ratio test and AIC.
- The overall treatment effect was tested with an F-test, and each treatment was compared with its control using Dunnett's test (`emmeans`, multivariate t adjustment; `set.seed(123)` for reproducibility).

**Field trial**

- Plants were arranged in a randomised complete block design with two blocks (rows), with positions randomised within each row using `agricolae`.
- Data were analysed with linear models with treatment and row as fixed effects; treatment effects were tested with Type II F-tests (`car::Anova`).
- Fungal biomass was log10(x + c)-transformed, where c is half the smallest non-zero value.

## Requirements

R and the following packages:

| Package | Use |
|---|---|
| `nlme` | Linear mixed-effects models with heterogeneous variances |
| `lme4`, `lmerTest` | Mixed-model fitting and tests |
| `emmeans` | Estimated marginal means and Dunnett's tests |
| `car` | Brown–Forsythe (Levene) test and Type II ANOVA |
| `performance`, `DHARMa` | Model diagnostics |
| `readxl` | Importing data from Excel |
| `ggplot2`, `ggpubr` | Figures |
| `agricolae` | Randomised field layout |

Install them with:

```r
install.packages(c("nlme", "lme4", "lmerTest", "emmeans", "car", "performance",
                   "DHARMa", "readxl", "ggplot2", "ggpubr", "agricolae"))
```

## Usage

The scripts read data from Excel files. Replace the placeholder paths (`"directory/Data.xlsx"`) with the location of your data files before running them. Each analysis section is run separately for each response variable and experimental group.

## License

This code is released under the [MIT License].

## Contact

Christos Tsoukas, [chriculture@gmail.com]
