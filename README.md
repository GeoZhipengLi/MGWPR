MGWPR: Multiscale Geographically Weighted Panel Regression

MGWPR is a Python code repository for Multiscale Geographically Weighted Panel Regression, extending Multiscale Geographically Weighted Regression (MGWR) to panel data. It includes two approaches:

Pooled MGWR (PMGWR): Fits spatially varying regression coefficients using pooled panel observations, without accounting for individual fixed effects.

Multiscale Geographically Weighted Fixed Effects Regression (MGWFER): Incorporates individual fixed effects to control for unobserved, time-invariant spatial confounders while allowing coefficients to vary across space and operate at different spatial scales.

The MGWFER method is introduced in the following paper:

Li, Z., & Fotheringham, A. S. (2026). Spatial Context as a Time-Invariant Confounder: A Fixed-Effects Extension of MGWR. Annals of the American Association of Geographers, 116(9), 2294–2318. https://doi.org/10.1080/24694452.2026.2654481

Repository Contents

mgwr/ — Modified MGWR source code for panel regression.

MGWFER Simulation.py and MGWFER Georgia.py — MGWFER simulation and Georgia case study.

Pooled MGWR Simulation.py and Pooled MGWR Georgia.py — Pooled MGWR benchmarks.

Demean.R and Georgia_Demean.R — Data preparation scripts.

Input/ and Output/ — Input data and results.

Usage

Run the relevant Python script from the repository root. For example:

python "MGWFER Simulation.py"

Some examples require preparing the input data using the corresponding R script first.

Citation

If you use MGWFER or the accompanying code in your research, please cite the paper linked above.
