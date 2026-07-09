# CROSSWALK — Analysis Code

This folder contains the R scripts used for data transformation, statistical
analysis, and figure generation for the CROSSWALK project (VR + eye-tracking
study on action decisions under uncertainty), published as:

Aguilar-Lleyda, D., González-Del Pozo, A., López-Moliner, J. et al. Scene
variability affects action decisions, confidence and behaviour dynamics.
*Commun Psychol* (2026). https://doi.org/10.1038/s44271-026-00448-1

## Contents

Contains all scripts to load the data, perform data transformations, run
statistical tests, and produce figures.

**Analysis scripts:**

- `define_functions.R` — Custom functions for data analysis and visualization
- `define_graphical_parameters.R` — Visual styling parameters for all figures
- `data_manipulation.R` — Data transformations and variable creation (calls
  `define_functions.R` and `define_graphical_parameters.R`)
- `results.R` — Statistical analyses and model fitting
- `figures.R` — Figure generation for main text and supplementary material

**Execution order:**

1. `data_manipulation.R` (loads data, creates variables, performs transformations)
2. `results.R` (runs statistical models and tests)
3. `figures.R` (generates all figures)

## Data availability

Raw and processed data files are **not included in this repository**, in
order to protect participant confidentiality. The scripts are shared here to
demonstrate the analysis pipeline and coding approach; they are not intended
to be run end-to-end without the original (restricted) data files.

The full dataset is available upon reasonable request, subject to the data
sharing terms described in the published article.

## Context

This code is part of a portfolio project. A non-technical walkthrough of the
study — motivation, method, and results — is available on the portfolio page:
[CROSSWALK project page](https://antoniogonzalezdp.github.io/project-crosswalk.html)

Full repository: https://github.com/antoniogonzalezdp/antoniogonzalezdp.github.io/tree/main/code_analysis
