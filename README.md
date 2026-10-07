# Reported Lassa fever case fatality in Nigeria, 2017-2026

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23135367.svg)](https://doi.org/10.5281/zenodo.23135367)

Pre-specified analysis plan and analysis code for a study of what published surveillance data can and cannot show about a change in reported Lassa fever case fatality in Nigeria.

## The question

The Nigeria Centre for Disease Control and Prevention (NCDC) reports a case fatality rate (CFR) among laboratory-confirmed Lassa fever cases in every weekly situation report. In 2026 the cumulative CFR at week 23 was 25.0%, against 18.9% in the same period of 2025. A reported CFR is a ratio of two counts produced by surveillance, so it can change for reasons other than a change in the risk of death: who is tested, which deaths are found, where cases occur and when the figure is read. This study sets out those mechanisms, assesses which of them the published reports allow us to evaluate, and evaluates those that can be.

## The analysis plan

`plan/analysis_plan.md` was fixed on 4 October 2026, before the analyses were run on the final dataset. This repository's first commit, tagged `v1.0`, is that version, archived at Zenodo: https://doi.org/10.5281/zenodo.23135368. Any later change is reported, with its reason, in the paper and in a dated addendum to the plan. The plan opens with a summary in plain language.

The plan fixes eight analyses in three roles:

| Role | Analyses |
|---|---|
| Primary: the conclusions rest on these | A1 phase-matched comparison, A2 decomposition, A3 tipping point |
| Secondary: reported in full, no conclusion rests on them alone | A4 hierarchical model, A5 isolated detection |
| Descriptive | A6 suspected-case positivity, A7 use of the probable category, A8 year-end check |

Three addenda, dated 5 October 2026, were added after the plan was fixed and are released as `v1.1`, archived at Zenodo: https://doi.org/10.5281/zenodo.23214077. None changes analyses A1-A8. Addendum 1 adds a descriptive analysis, A9: the cumulative figures printed in each report against the sums of the weekly figures. Addendum 2 adds a check of rebuilt and missing weeks against national counts from NCDC's Weekly Epidemiological Report. Addendum 3 repeats the annual and phase-matched comparisons on the printed cumulative figures, as point estimates. Each addendum gives its reason at the end of the plan.

## Contents

- `plan/analysis_plan.md`: the pre-specified analysis plan.
- `R/`: the analysis. `00_functions.R` holds shared helpers, and `addendum1_` to `addendum3_` carry out the addenda.
- `sim/`: design simulations that informed choices in the plan. They use the structure of the data with simulated deaths.
- `renv.lock`: package versions.

## Data

The data are counts of suspected, confirmed and probable cases and of deaths among confirmed cases, by state and epidemiological week, read by hand from NCDC Lassa fever situation reports (https://ncdc.gov.ng). The reports are public. The extracted dataset will be released with the paper.

## Running the analysis

Requires R 4.6 and a C++ toolchain for CmdStan (Rtools on Windows).

```r
renv::restore()
cmdstanr::install_cmdstan(version = "2.40.0")
```

The scripts read the extraction ledger from `data-raw/lassa_state_week_ledger.csv`, one row per state, variable and report, with columns `year`, `epi_week`, `epi_week_span`, `state`, `region_id`, `variable`, `value`, `extraction`, `source_file` and `note`. Addenda 1 and 3 also read the cumulative figures printed in the reports, from `data-raw/lassa_cumulative_national.csv` (every report) and `data-raw/lassa_cumulative_state.csv` (the final report of each year from 2020). Addendum 2 downloads the Weekly Epidemiological Report dataset at a fixed revision, so it needs a network connection. Run `01` to `11` in order, then the three addendum scripts, then `12_figures.R`. The scripts default to a small test size; set the environment variable `LASSA_SMOKE=FALSE` for the full analysis. The model fits take about an hour on a desktop machine.

Outputs are written to:

| Folder | Contents |
|---|---|
| `data/intermediate/` | Analysis-ready data built from the ledger |
| `data/qc/` | Validation flags and the decomposition strata |
| `data/results/` | One file per result, read by the figures and the manuscript |
| `data/models/` | Fitted models |
| `figures/` | Main and supplementary figures |
| `tables/` | Supplementary tables |

## Licence

CC0 1.0 Universal: the code, the analysis plan and, when released, the extracted dataset are dedicated to the public domain. See `LICENSE`.

## Use of AI tools

AI tools (Claude Opus 5.5, Anthropic) were used to develop, test and comment the analysis code, and to review the text of the analysis plan and this repository. I accept full responsibility for their use and for the results produced under the plan.

## Author

David Simons, Uppsala University.
