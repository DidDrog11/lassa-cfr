# Pre-specified analysis plan

**What published surveillance data can and cannot show about a change in reported Lassa fever case fatality in Nigeria, 2017-2026**

David Simons, Uppsala University

Version 1.0, fixed 4 October 2026. This version is the initial commit of the public repository, tagged `v1.0`.

## Summary in plain language

**The question.** Every week, the Nigeria Centre for Disease Control and Prevention (NCDC) publishes the share of people with laboratory-confirmed Lassa fever who died. This is the reported case fatality rate (CFR). In 2026 it was higher than at the same point in 2025, and this was read as a reason to look closely at how quickly patients reach diagnosis and treatment. A reported CFR can also change for reasons that have nothing to do with the risk of death: for example, if fewer people with mild illness are tested, if more cases come from states where the CFR has always been higher, or if two years are compared at different stages of their outbreaks. This study asks which of these explanations the published reports can tell apart, and which they cannot.

**The data.** The weekly situation reports NCDC published from January 2017 to 2026 week 36, the week ending 6 September 2026, the last report available when this plan was fixed. Every number was read from the reports by hand and then checked against them.

**What this plan fixes before the analysis is run on the final data.**

- The possible explanations for a change in reported CFR (M1-M11), and whether the reports contain what is needed to check each one.
- Eight analyses (A1-A8) in three roles. The conclusions rest on three primary analyses (A1-A3), each answering a direct question about the reported change. Two secondary analyses (A4, A5) are reported in full but cannot carry a conclusion alone, because of the limits of what they can separate. Three descriptive analyses (A6-A8) give context. For each analysis, the main result and how each possible outcome will be read are fixed now, whichever way the result comes out. I will not change that reading after seeing the results; any departure is reported as a deviation. Other outputs are labelled as secondary, sensitivity or descriptive, so they cannot later replace a main result as the basis for a conclusion.
- The rules for gaps and irregularities in the reports.

**What I knew when I wrote it.** This is stated in full under *Prior knowledge*.

**Why the plan comes first, and how it is used.** Data like these can be analysed and read in many reasonable ways. Fixing the analyses, and how their results will be read, before running them on the final data means the conclusions follow from the plan rather than from whichever results look most interesting. This version is the first commit of a public repository, tagged `v1.0` and dated, and the analysis code in that repository carries out the analyses as written here. The paper reports each analysis as planned, under the same labels. Any change made after this version is reported, with its reason, in the paper and in a dated addendum to this document. A result that does not settle the question is an acceptable outcome: the aim is to show what can and cannot be known from these data.

**How to read the rest.** The sections that follow are technical. Each analysis opens with a short paragraph in plain language, marked *In plain terms*. Terms are explained under *Definitions*.

## Status of this document

This is an analysis plan fixed before the analyses were run on the final dataset. It is not a preregistration. Any change made after this version is reported as a deviation, with its reason, in the paper and in a dated addendum to this document.

Addenda: Addendum 1 (5 October 2026) adds one descriptive analysis, A9. It is at the end of this document.

## Prior knowledge

I extracted the data and was aware of its structure. The design of the current study was informed by an exploratory analysis of an earlier automated extraction of 2020-2026 reports; its estimates are not reported, but the extracted data are available (https://github.com/BioDivHealth/LF_Nigeria_Reports). Analysis code was developed and tested on a preliminary version of the dataset. I did not review these results before fixing this plan.

AI tools (Claude Opus 5.5, Anthropic) were used to develop and test the analysis code and to review the analysis plan. I accept full responsibility for their use and for the results produced under this plan.

## Question

The Nigeria Centre for Disease Control and Prevention (NCDC) reports a case fatality rate (CFR) among laboratory-confirmed Lassa fever cases in every weekly situation report. In 2026 the cumulative CFR at week 23 was 25.0%, against 18.9% in the same period of 2025. A commentary noted that a CFR among confirmed cases cannot by itself separate a change in the risk of death from a change in testing, but argued that the divergence between deaths and cases warrants scrutiny of the route from symptom onset to diagnosis, referral and care, and called for earlier diagnosis and treatment (Abdulrahim, Gulumbe and Adepoju, *Lancet Regional Health – Africa* 2026).

A reported CFR is a ratio of two counts derived from surveillance, so it can move for reasons other than a change in the risk of death. This analysis sets out the mechanisms that can move it, assesses which of them the published reports allow us to evaluate, and evaluates those that can be. It also identifies the limits of inference from current reporting and suggests changes to reporting that would extend them. A null or ambiguous result is an acceptable outcome: the aim is to establish what can be known from these data.

## Data

### Source and extraction

NCDC weekly Lassa fever situation reports from January 2017 to 2026 week 36, the week ending 6 September 2026 (see *Data freeze*). Counts of suspected, confirmed and probable cases and of deaths among confirmed cases are extracted by state and epidemiological week. Every value is read from the report by hand with a structured transcription tool (Map Liberator, doi:10.5281/zenodo.18325137). The completed extraction is checked against the machine-readable parts of each report: national totals on page one from 2020, and the state-by-state enumeration in the highlights text for 2017-2019. Every discrepancy is resolved against the report. Errors found after the final extract was exported are not edited in the data file; they are corrected in a table in the loading script (`01_load.R`), each with the evidence from the report.

### Data freeze

Only reports dated on or before 1 October 2026 are used. A report's date is the date NCDC's report listing gives for it, recorded in the report's file name; a report without a readable date stops the analysis. When this plan was fixed, the last report published covered 2026 week 36, ending 6 September 2026. The last season is therefore partial. The phase-matched comparison (A1), the decomposition (A2) and the model (A4) cut every season to the week covered by the last included report, so that every season-level comparison covers the same weeks. Transmission runs roughly from December to April, across the turn of the epidemiological year, so the weeks after the cut hold the quieter months and the start of the following transmission season; they contain a substantial share of each year's cases, and leaving them in for complete years but not for the partial one would confound any comparison with the last season. A2 and A4 repeat the analysis on full years as a sensitivity analysis.

### Report formats

| Period | Per-state figures | Source in the report |
|---|---|---|
| 2017-2019 | Confirmed cases and deaths by state (and suspected cases by state in 2017) | Highlights text on page one |
| 2020 weeks 1-6 | Confirmed cases only | State table |
| 2020 week 7 onwards | Suspected, confirmed and probable cases and deaths, current week | State table |

### Data-handling rules

These rules are applied mechanically to the final dataset.

1. **States not named in a report.** Only states named in a report are extracted. For each report and each variable: if the report lists that variable by state, an unnamed state is recorded as zero; if the report gives no state breakdown for that variable, every state is recorded as missing. A state entered with a missing value stays missing.
2. **Reports stating that nothing was reported.** A value of zero means the variable was listed by state and no state had any; a missing value means no state breakdown was given.
3. **Weeks with no report**, whether never published or missing from the archive, are missing, not zero. Season totals built over missing weeks are lower bounds and are flagged.
4. **Multi-week totals.** Five weeks of 2022 (weeks 19-23) survive only as one total derived from the difference between two cumulative reports. The total counts towards season totals and enters cumulative series at week 23; weeks 19-22 have an unknown running total. These five weeks are treated as unobserved in any analysis indexed by week. Single missing weeks inferred the same way (2022 week 14, 2023 week 38, 2024 week 16) are used as ordinary weeks.
5. **Late reports.** Cases described in 2017-2019 reports as "not previously reported" are added to the week of the report that carried them.
6. **The 2017 list of suspected cases by state** is taken as complete: a state named for confirmed cases but not for suspected cases had no new suspected cases that week.
7. **Place names.** "Jos" is counted as Plateau State. Rows for places outside Nigeria are dropped.
8. **Deaths reported only as a national total.** In the inference years, seven reports (2018 weeks 3, 4 and 46; 2020 weeks 2-5) give deaths as a national figure with no state breakdown; the 2018 week 46 report places its one death in Edo or Ondo without saying which. The 2018 week 1 report gives no deaths at all, and its deaths stay missing. Seven 2017 reports give deaths only cumulatively or for some states; 2017 is descriptive only, and these deaths stay missing. Any other report found to give deaths without a state breakdown stops the analysis until it is added here. Each week's total is allocated to the states with confirmed cases that week, in proportion to their confirmed cases. The point estimate uses this expected allocation. For uncertainty the allocation is redrawn by assigning each death to one of that week's confirmed cases, without replacement, so no state receives more deaths in a week than it had confirmed cases. The assumption is that, in those weeks, deaths were distributed like cases.
9. **Seasons used for inference.** 2018-2026. 2017 is shown descriptively and excluded from inference, because 18 of its 52 weeks have no surviving report, including the opening and peak weeks of the season, so its totals undercount unevenly by state. 2020 and 2021, which overlap the COVID-19 pandemic, are included as they are. Changes in reported Lassa fever during the pandemic have been attributed both to under-reporting and to reduced transmission (Musa et al., 2022; Reuben et al., 2021), the same ambiguity between detection and incidence that this analysis addresses.
10. **State-weeks used for inference.** Inference uses state-weeks in which both confirmed cases and deaths are known; a state-week with unknown deaths is left out with its confirmed cases, so numerator and denominator always cover the same weeks. After rule 8 this is expected to affect few state-weeks outside 2017; their number is reported.

### Definitions

Terms used in the reports:

- **Suspected case:** a person whose illness meets NCDC's surveillance case definition for Lassa fever. Not every suspected case is tested.
- **Confirmed case:** a suspected case with a positive laboratory test.
- **Probable case:** a suspected case who died, or (from mid-2019) left care, before a sample could be taken for testing. Probable cases are not part of the CFR.
- **Epidemiological week:** the numbered week of the year used in the reports, 1-52 (or 53).
- **Season:** an epidemiological year, weeks 1-52 (or 53), as numbered in the reports.
- **State-week:** the figures for one state in one week, the smallest unit in the data.
- **Reported CFR:** deaths among confirmed cases divided by confirmed cases, as reported by NCDC.
- **Cumulative and incident CFR:** a cumulative CFR uses all cases and deaths from the start of the season to a given week; an incident CFR uses only one week's cases and deaths.
- **Suspected-case positivity:** confirmed cases divided by suspected cases. It is a proxy, not test positivity: suspected cases are those meeting the surveillance case definition, not specimens tested, so the ratio includes the decision to sample a suspected case. It is labelled as a proxy wherever it is reported.

Terms used in the analysis:

- **Ascertainment:** which cases and deaths are found, tested and reported. A change in ascertainment can change the reported CFR when the risk of death has not changed.
- **Primary, secondary and descriptive analyses:** the conclusions rest on the primary analyses. Secondary analyses are reported in full but do not carry a conclusion alone. Descriptive analyses show the data without testing anything.
- **Estimand:** the quantity an analysis sets out to estimate. The **main** estimand of an analysis was chosen in this plan. Within an analysis, **secondary** outputs answer related questions; **sensitivity** analyses repeat the analysis with a different reasonable choice, to show whether the result depends on that choice; **descriptive** outputs show the data without testing anything.
- **Stratum:** a group, here usually a state or a pool of small states, analysed as one unit.
- **Expected value (null):** what a result would be if the explanation being tested were absent. The observed result is compared with it.
- **Interval:** every estimate is given with a 95% interval, a range of values consistent with the data. Most intervals come from a **bootstrap**: the analysis is repeated many times on data resampled from the original, and the spread of the results gives the interval.
- **Posterior probability:** in the model (A4), the probability, given the data and the model's assumptions, that a statement is true; for example, that one source of variation is larger than another.
- **Imputation:** where deaths are reported only as a national total, they are shared among states by a stated rule (data-handling rule 8). The model is fitted to ten such shares together, and the bootstrap draws a new share in each repetition, so that the intervals include this uncertainty.

## Mechanisms and evaluability

*In plain terms.* A reported CFR can change for many reasons. The table lists eleven (M1-M11), what each would look like in the published figures, and whether the reports contain what is needed to check it. Two can be checked, five only in part, and four not at all. What cannot be checked is a result in its own right: it shows what the reports would need to publish for a change in reported CFR to be read as a change in the risk of death.

Each mechanism (M1-M11) is listed with the signature it would leave in the published indicators, the marker that would identify it, whether the published reports allow it to be evaluated, and the analysis (A1-A8, below) that evaluates it. Mechanisms marked "not evaluated" are kept in the table because the gap is itself a finding; the reason for each, and what is left unevaluated for partly evaluated mechanisms, is given under *What is not evaluated*.

| | Mechanism | Suspected cases | Suspected-case positivity | Within-state CFR | Most diagnostic marker | Evaluable from published reports | Analysis |
|---|---|---|---|---|---|---|---|
| M1 | Testing threshold tightens | flat or down | up | up in all states | Proportion sampled at or after death | Partial | A3, A4, A5, A6 |
| M2 | Laboratory capacity expands | up | down | down in all states | Testing volume by site | Partial (proxy only) | A3, A4, A6 |
| M3 | Care-seeking suppressed | down | up | up | Onset-to-presentation interval | No | Not evaluated; bounded jointly with M1-M2 by A3 |
| M4 | Case definition or its application changes | changes | changes | up or down | Case-definition wording; use of the probable category | Wording: yes (reviewed, see below). Application: partial | A7 |
| M5 | Retrospective death investigation intensifies | flat | flat or up | up where case search intensifies | Case-search and contact-tracing activity; proportion sampled at or after death | No | Not evaluated; A5 is sensitive to it but cannot separate it from M1 |
| M6 | Spatial reweighting | - | - | unchanged | State-level case and death counts | Yes | A2 |
| M7 | Demographic shift | - | - | unchanged within strata | Age and sex of confirmed cases | No | Not evaluated |
| M8 | Phase or censoring artefact | - | - | - | Within-season CFR trajectory | Yes | A1 |
| M9 | Care deterioration | - | - | up in affected states only | Onset-to-admission interval; treatment-centre mortality | Partial (pattern only, not cause) | A4 |
| M10 | Outcome follow-up improves | - | - | up | Proportion of cases with ascertained outcome | No | Not evaluated |
| M11 | Retrospective revision | - | - | - | Printed cumulative against summed weekly figures | Partial (year end only) | A8 |

### What is not evaluated

- **M3, care-seeking suppressed.** The interval from symptom onset to presentation is not reported. A3 bounds how large a loss of mild cases from the denominator would have to be, but cannot say whether it came from care-seeking (M3), testing (M1) or laboratory capacity (M2).
- **M5, retrospective death investigation.** Case search and contact tracing are described only in the narrative text of the reports and were not extracted. Its signature, deaths among cases detected at or after death, is the same as that of a tightening testing threshold (M1), so A5 cannot separate the two.
- **M7, demographic shift.** Age, sex and pregnancy status of cases and deaths are not reported by state and week.
- **M9, care deterioration (cause).** A4 shows whether a change is confined to some states, the pattern care deterioration would produce, but not what caused it.
- **M10, outcome follow-up.** The proportion of confirmed cases with an ascertained outcome is not reported.

For the partly evaluated mechanisms, what remains unevaluated:

- **M1, testing threshold.** The share of cases first sampled at or after death is not reported; A5 is a proxy for it, and A6 is a proxy for testing behaviour.
- **M2, laboratory capacity.** Testing volume by site is not reported; A6 sees it only through suspected-case positivity, a proxy.
- **M4, case definition.** The wording was reviewed in every report (A7); how consistently the categories were applied in practice can be seen only through use of the probable category.
- **M11, retrospective revision.** Only the year-end cumulative figure is compared with the summed weekly figures (A8); revisions during a season are not traced.

## Analyses

The analyses have three roles. The conclusions of the study rest on the three **primary** analyses (A1-A3). Each answers a direct question about the reported change, can be stated in plain terms, and makes few assumptions beyond the counts. The two **secondary** analyses (A4, A5) are run and reported in full, but no conclusion rests on them alone. They are secondary for reasons of design and of what these data can support: the model (A4) cannot tell a national change in ascertainment from a national change in care, and its main estimand is read only together with other outputs; isolated detection (A5) cannot separate testing prompted by a death from retrospective death investigation, detects only a large effect, and speaks mainly to states that hold a small share of cases. The three **descriptive** analyses (A6-A8) give context and test nothing.

Each primary and secondary analysis has one main estimand. Its other outputs are labelled secondary, sensitivity or descriptive. Throughout, "target season" is 2026 and "reference season" 2025.

| | Analysis | Role | The question in plain terms | Mechanisms | Main estimand | Secondary | Sensitivity | Descriptive |
|---|---|---|---|---|---|---|---|---|
| A1 | Phase-matched comparison | Primary | Does the difference between 2026 and 2025 hold when both are compared at the same stage of their outbreaks? | M8 | Target minus reference cumulative CFR at the 50th percentile phase point | The same at the 25th percentile point | - | Fixed week 23 comparison; other seasons at the same points |
| A2 | Decomposition | Primary | Did the CFR change within states, or did cases shift towards states where it is usually higher or lower? | M6 | Within-state and between-state components of the 2025-2026 change | Split with every state separate | Weeks with deaths reported only nationally removed from numerator and denominator rather than allocated; full years instead of seasons cut to the freeze week | Components for the seven earlier transitions |
| A3 | Tipping point | Primary | How many surviving cases would have to be missing for 2026 to match 2025? | M1-M3 jointly | Missed survivors needed at week 23 (published figures), as a share of 2026 suspected cases not confirmed | Reconstructed figures; pooled 2018-2025 reference | - | - |
| A4 | Hierarchical model | Secondary | Did states move together (pointing to a national cause) or separately (pointing to local causes)? | M1, M2 (common) against M9 (localised) | Posterior probability that the season SD exceeds the state-within-season SD | 2025-2026 contrast in season effects; 2026 deviations of the states in the 2025-2026 decomposition roster | Beta-binomial likelihood; wider and tighter SD priors; without Edo; without Ondo; full years | Season effects 2018-2026 |
| A5 | Isolated detection | Secondary | Do the first cases after a quiet spell include deaths more often than expected? | M1 | Observed / expected share of events with a death, 3-week lookback, detection week plus one, null from each state's own CFR | National-CFR null; LASCOPE null; by state group; by season phase; by reporting continuity | Every combination of lookback 2, 3, 4 and 6 weeks with follow-up 0, 1 and 2 weeks | By season |
| A6 | Suspected-case positivity | Descriptive | How has the share of suspected cases that are confirmed changed? | M1, M2 | - | - | - | Nationally and for roster states, by season |
| A7 | Probable audit | Descriptive | How often do states use the probable category? | M4 | - | - | - | Use of the probable category by state and season |
| A8 | Year-end check | Descriptive | Do year-end totals match the sum of the weekly reports? | M11 | - | - | - | Final-report cumulative figures against summed weekly figures, by season |

### A1. Phase-matched comparison (M8)

*In plain terms.* Deaths are recorded some time after cases, and seasons start and peak at different times. A CFR read at the same calendar week in two years can therefore differ only because one outbreak was further along than the other. A1 compares 2026 with 2025 at the same stage of each outbreak instead: the week by which a quarter, and then half, of the season's cases had been confirmed. If the difference stays about the same, timing does not explain it. If it shrinks, grows or reverses, part of the difference reported at a fixed week came from when the comparison was made.

Seasons differ in when cases accumulate, and deaths lag cases, so a cumulative CFR read on a fixed calendar week compares seasons at different points in their epidemics. Every season is cut to the last week covered in the target season. The phase point of a season is the first week at which its running total of confirmed cases reaches the given percentile of its total within that window. Only the 25th and 50th percentiles are used: a later percentile of a truncated total describes the truncation as much as the season. Where a percentile falls in weeks with an unknown running total, the point is placed at the next week with a known total and flagged. After a week with no report the running total is not known again that season, so a percentile falling after one cannot be placed; that season's point is then missing and flagged. Cumulative CFR is compared between the target and reference seasons at each point. The fixed week 23 comparison reproduces the comparison the commentary made.

### A2. Decomposition (M6)

*In plain terms.* The national CFR combines all states, and CFR differs between states. If a larger share of cases comes from states where the CFR has always been higher, the national figure rises even if nothing has changed within any state. A2 splits each change from one year to the next into two parts: the change within states, and the change that comes from cases moving between states. The 2025-2026 split is then placed among the seven earlier year-on-year changes; with seven, this is a description, not a test. The split shows where a change appears, not why. A change in which cases are found and a real change in the risk of death both appear in the within-state part.

A national CFR is a case-weighted average of state CFRs, so it changes either because CFR changes within states (rate component) or because cases move between states with different CFRs (composition component). Each consecutive-season change from 2018 is split into these two components (Kitagawa, 1955); the two sum exactly to the observed change. Per-state composition terms are centred on the mean of the two seasons' national CFRs, so a state contributes to the composition component only through the difference between its CFR and that national figure.

Strata: for each pair of seasons, a state is its own stratum if it had at least 30 confirmed cases in both seasons; all other states are pooled into one stratum for that pair. The threshold is on confirmed cases, not deaths, and is applied to full-season counts. The split with every state separate is a secondary output and is labelled as noisy. Every pair, including the seven reference transitions, compares seasons cut to the last week covered in the target season (see *Data freeze*).

The 2025-2026 components are read against the seven earlier transitions by their position among them. With seven references this is a description, not a test.

### A3. Tipping point (M1-M3)

*In plain terms.* Suppose the higher CFR in 2026 came only from people who survived not being confirmed, so that they were missing from the count of cases. How many such survivors would have to be missing for 2026 to match 2025? A3 calculates that number at week 23, the week the commentary used, and sets it beside the number of people suspected of Lassa fever in 2026 who were not confirmed. The smaller the number is beside that pool, the more plausible missed survivors are as an explanation. The pool leaves out people who were never suspected, so it is not an upper limit. A3 does not show that survivors were missed; it shows how many would have to be.

If a change in ascertainment explains the difference between two seasons, some number of non-fatal cases went unconfirmed in the later season. With D deaths and C confirmed cases in the target season and reference CFR r, the number of missed survivors x that reconciles the two solves D / (C + x) = r. Main estimand: the figures published at week 23 of 2026 and the corresponding 2025 figures quoted in the commentary (855 confirmed and 214 deaths; 758 confirmed and 143 deaths), with x expressed as a share of the 2026 suspected cases not confirmed by week 23. Secondary: the same calculation on our reconstruction from weekly reports, and against a pooled reference of the 2018-2025 seasons whose week 23 total is exactly known. The 2026 suspected cases not confirmed by week 23 come from the reconstruction, since the commentary does not quote them. The target counts are taken as observed; uncertainty comes from the reference CFR.

### A4. Hierarchical model (M1, M2 against M9), secondary

*In plain terms.* This is a secondary analysis: it is reported in full, but no conclusion rests on it alone. If the CFR changes because of something that acts across the country, such as a change in who is tested, states should tend to move together from one year to the next. If it changes because of something local, such as care in particular treatment centres, states should move in different directions. A4 estimates how much of the variation in CFR is shared by all states in a year, and how much is specific to individual states within a year, and reports the probability that the shared part is larger. It is reported as a probability, with no cut-off. A mainly shared pattern fits a national cause; a mainly state-specific pattern fits a local one. Neither pattern identifies the cause: a national change in who is tested and a national change in care would both appear as a shared pattern. A change in the few states that hold most cases can look like a shared change, so the result is read together with the results for individual states and with the model refitted without each of the two states that report the most cases.

    deaths | trials(confirmed) ~ 1 + (1 | season) + (1 | state) + (1 | state:season)

Binomial likelihood, one row per state-season with at least one confirmed case, 2018-2026, every season cut to the last week covered in 2026 (see *Data freeze*). With one row per state-season the state-within-season term is observation-level and carries overdispersion. No within-season week term.

If CFR moves between seasons for a system-wide reason such as a change in ascertainment, variation between seasons should dominate; if it moves because of local change, variation between states within a season should dominate. The main estimand is the posterior probability that the season standard deviation exceeds the state-within-season standard deviation. It is reported as a probability, without a threshold. The state-within-season term also absorbs noise from data quality, which tilts the comparison towards a local explanation; this is stated with the result. Seasons are exchangeable in the model, so any secular trend appears in the season effects, which are reported as the reference distribution for 2026 rather than modelled. A design simulation on the real case counts (`sim/sim1_model_estimand.R`) showed that the estimand separates mainly common from mainly state-specific variation well, but that a shift confined to the few states holding most cases can be partly absorbed into the season effect. The main estimand is therefore interpreted together with the 2026 state deviations and the refits without Edo and without Ondo, not on its own.

Priors: Student-t(3, 0, 2.5) on the intercept; exponential(1) on each random-effect standard deviation. Sensitivity analyses: a beta-binomial likelihood with a gamma(2, 0.1) prior on its precision; exponential(0.5) and exponential(2) priors on the standard deviations; refits without Edo and without Ondo, which hold most confirmed cases and so carry most of the information about each season effect; and a fit on full years instead of seasons cut to the freeze week.

Estimation: brms with the CmdStan backend; 4 chains of 3,000 iterations including 1,000 warm-up; adapt_delta 0.99; 10 imputed datasets for the allocated deaths, fitted together. Convergence is judged within each imputed dataset: R-hat below 1.01, bulk effective sample size above 400, and no divergent transitions. If a criterion is not met, the number of iterations is increased; the model itself is not changed.

### A5. Isolated detection (M1), secondary

*In plain terms.* This is a secondary analysis: it is reported in full, but no conclusion rests on it alone. When a state reports a confirmed case after several weeks with none, one possible reason is that a death prompted the test. If that happens often, these first cases after a quiet period will include deaths more often than the state's usual CFR predicts. A5 counts how often they do and divides by what the state's own CFR predicts. A ratio near 1 means no sign of substantial detection prompted by death, but cannot rule out a small amount. A ratio above 1 means deaths are more common than expected in these weeks; a ratio below 1 means they are less common. A ratio above 1 cannot separate testing prompted by a death (M1) from active searches that find earlier deaths (M5).

The reports do not say how many confirmed cases were first sampled at or after death. As a proxy, an isolated detection event is a state-week with confirmed cases after k weeks in which that state confirmed none; the outcome is whether a death is reported in that week or the j following weeks. If detection were blind to severity, a window holding n confirmed cases would include at least one death with probability 1 - (1 - p)^n, where n counts the confirmed cases in the whole outcome window. The main estimand is the ratio of the observed share of events with a death to the expected share, with k = 3, j = 1 and p each state's own CFR: deaths divided by confirmed cases in the state's observed weeks outside the event windows of the same k and j, pooled over 2018-2026, without shrinkage. A state with no confirmed cases outside the windows takes the season's national CFR. Secondary nulls: the season's national CFR, and the CFR among patients admitted to a specialist treatment centre in the LASCOPE cohort (62 of 510; Duvignaud et al., *Lancet Global Health* 2021). The LASCOPE CFR is a reference point, not a bound: specialist treatment would make it lower than among untreated or late-presenting cases, while restricting to admitted patients leaves out mild cases and would make it higher.

Design simulations on the real event structure, with deaths simulated (`sim/sim2_isolated_null.R`, `sim2b`, `sim2c`), set this choice. With a single national CFR the ratio departs from 1 whenever the states that produce events differ in CFR from the rest, by about as much as when one detection in ten is triggered by death, so the two cannot be told apart. Each state's own CFR removes that confusion; shrinking it towards the national CFR brings it back, because states that report intermittently have most of their cases inside event windows. With the state's own CFR and its uncertainty carried into the interval, the ratio stays near 1 without detection bias and the interval excludes 1 at close to the nominal rate; it detects a bias of one in five detections reliably and one in ten only about four times in ten. A5 can therefore show substantial detection triggered by death but not rule out a small amount.

k and j cannot be estimated from these data, because the interval from case to death is not reported. They are taken from LASCOPE, in which the median time from admission to death was 3 days and 51 of 62 deaths occurred within 7 days of admission. These timings describe patients who reached a specialist centre; where presentation or referral is later, the interval between confirmation and death may differ, which is one reason the sensitivity analysis varies k and j. Weeks with no report, the 2022 multi-week total and weeks with allocated deaths are unobserved and break both windows. Windows run across the boundary between seasons, but not back into 2017, so the first k weeks of 2018 cannot be events. Strata: states that stand alone in any decomposition pair against all others; season weeks 1-13 against later weeks; and reporting continuity, a measure of how often a state reports suspected cases: a state-season is "frequent" if the state reported at least one suspected case in at least half of the weeks for which suspected cases are given by state, and "intermittent" otherwise. A week with no suspected cases cannot be told apart from a week in which suspected cases occurred but were not recognised or reported, so the stratum describes reporting, not the underlying capacity of surveillance. Suspected cases are not given by state in 2018-2019, so events in those seasons are left out of this stratum only. High-burden states report almost continuously and contribute few events, so the statistic speaks mainly to low- and mid-burden states; the share of weeks with suspected cases by state and season is given as a supplementary table for context.

A5 tests for detection triggered by death. Retrospective death investigation (M5) would produce the same signature, so a positive result is read as evidence for M1 or M5 without separating them.

### A6-A8. Descriptive analyses

*In plain terms.* These three analyses describe the data and test nothing. A6 shows how the share of suspected cases that are confirmed changes over time. A fall can mean wider testing or a broader use of the suspected-case definition, and a rise the reverse, so this share is a proxy for testing behaviour, not a measure of it. A7 shows how often states use the probable category, for people who died before a sample was taken. A8 checks whether each year's final cumulative figures match the sum of that year's weekly reports, which shows whether figures were revised after they were first published.

- **A6 (M1, M2).** Suspected-case positivity nationally and for the states that stand alone in any decomposition pair, from 2020, where suspected cases are given by state.
- **A7 (M4).** Use of the probable category by state and season. The case-definition box was read in every report before this plan was fixed. The confirmed-case definition ("any suspected case with laboratory confirmation (positive IgM antibody, PCR or virus isolation)") is identical from 2017 week 5 to 2026 week 36, and the suspected-case criteria are the same wherever printed, from 2017 week 7. The probable-case definition changed twice: "any suspected case ... but who died without collection of specimen for laboratory testing" (2017 week 5 to 2018 week 5); the same without "but", a change of wording only (2018 week 6 to 2019 week 24); and "who died or absconded without collection of specimen" (2019 week 25 onwards). Because the CFR uses confirmed cases only, no change in the definitions that enter it occurred in the study period. A7 therefore concerns how the probable category was applied, read against the 2019 widening.
- **A8 (M11).** Cumulative figures printed in each season's final report (for 2026, the last report before the freeze) against the sum of that season's weekly reports. The printed figures are read from the report, by machine where its text can be parsed and by hand otherwise.

Two further descriptive outputs support the whole analysis rather than one mechanism: reported CFR by season and week, cumulative and incident, 2017-2026, with week coverage; and which variables the reports break down by state, by year, which is an input to the evaluability column of the mechanisms table.

## Uncertainty

Intervals for the decomposition, the phase-matched comparison and the tipping point come from one cluster bootstrap: state-weeks resampled with replacement within season, 1,000 replicates, with the allocated deaths redrawn in each replicate. The phase points are found again in each replicate; the decomposition strata are held at their point-estimate membership, so the intervals reflect variation in the counts rather than states crossing the threshold. For the published tipping-point figures, which have no state breakdown, the bootstrap spread of the reconstructed reference CFR is centred on the published value. Resampling state-weeks independently ignores correlation between a state's consecutive weeks, so these intervals are probably somewhat too narrow; this is stated as a limitation. A design simulation (`sim/sim3_bootstrap_coverage.R`) found coverage near 95% without correlation between weeks, about 90% at moderate correlation and about 80% at strong correlation; resampling blocks of 8 or 13 weeks did not improve it consistently, so state-weeks are resampled independently. Intervals for isolated detection come from resampling state-seasons, 1,000 replicates, with each state's own CFR redrawn in every replicate from a Jeffreys beta on its deaths and cases outside the event windows. Model uncertainty is the posterior pooled across the imputed datasets. All intervals are 95%.

## Not done

Mechanisms that are not evaluated are listed, with reasons, under *What is not evaluated*. In addition:

- No exclusion or sensitivity analysis for 2020 and 2021. Every transition is reported separately, so they remain visible.
- No sensitivity analysis for transcription error; the extraction discrepancy rate is reported descriptively.
- No week-by-week measure of retrospective revision; the year-end check (A8) stands in for it.

## Reproducibility

The analysis runs from the scripts in `R/`, in numbered order, on the extraction ledger at `data-raw/lassa_state_week_ledger.csv`. Package versions are fixed with renv (`renv.lock`); models use CmdStan 2.40. Every random step is seeded (seed 20260929). Scripts default to a small test size; the full run sets the environment variable `LASSA_SMOKE=FALSE`, which gives the values above.

| Script | Role |
|---|---|
| `01_load.R` | Applies the data-handling rules; builds the state-by-week grid and season totals |
| `02_validate.R` | Duplicates, coverage, fill status, internal consistency, per-pair strata |
| `03_descriptives.R` | Reported CFR series, week coverage, variable availability |
| `04_decompose.R` | Decomposition with bootstrap; sensitivity analysis |
| `05_positivity.R` | Suspected-case positivity |
| `06_probable.R` | Probable audit |
| `07_model.R` | Hierarchical model and its sensitivity fits |
| `08_tipping.R` | Tipping point |
| `09_isolated.R` | Isolated detection |
| `10_phase.R` | Phase-matched comparison |
| `11_year_end.R` | Year-end check |
| `12_figures.R` | Figures |

## Addendum 1, 5 October 2026

This addendum adds one descriptive analysis, A9. Nothing in version 1.0 above is changed: analyses A1-A8, their estimands, their roles and the data-handling rules stand as written.

**Why it was added.** The first outputs on the final data showed that the cumulative deaths printed in the 2026 week 36 report were substantially higher than the sum of the deaths in that year's weekly reports, while cumulative confirmed cases closely matched. The year-end check (A8) compares the two only at the end of each year, and cannot show where or when such a difference arises. A9 was added after outputs on the final data had been seen, in response to them, and is reported as an analysis added after the plan was fixed.

**A9. Printed cumulative figures against summed weekly figures (M10, M11).**

*In plain terms.* Each report gives the figures for its week and a running total for the year. If deaths are added to the running total without ever appearing in a week's figures, a CFR calculated from the running total and one calculated from the weekly figures will differ. A9 measures that difference in every year, finds the states where it arises, and finds the weeks in which it is added. It does not say why the deaths were added: deaths recorded late among cases confirmed earlier, and revisions to the records, would look the same in these figures.

1. The cumulative confirmed cases and deaths printed in the summary table on page one were read by hand from every report. From the final report of each year from 2020, the first year with a state table (for 2026, the last report before the data freeze), the cumulative figures for each state were read as well.
2. Each year's final state figures were compared with the sums of the weekly figures already extracted, state by state, to identify the states in which the differences arise. Deaths reported only as a national total in the weekly reports have no state, so they are counted separately rather than through their allocation (data-handling rule 8).
3. The national cumulative figures printed in each report were compared with the running sums of the weekly figures, to find the weeks in which the differences were added.

A9 is descriptive: differences are reported as counts, by year, state and week, with no interval and no test. Years in which archive gaps or the report format prevent a like-for-like comparison (2017, which counts from the onset of the season in December 2016; 2018, which has no state table; and weeks lost to missing reports) are reported with the reason. The summary goes in the main text and the full tables in the supplement.

A9 does not change any other analysis. A1-A8 use the weekly figures, as specified; their interpretation states that the weekly figures exclude any deaths added only to the cumulative figures, and how many deaths that is in each year. The tipping point (A3) uses the published cumulative figures, as specified, and so includes them.

Data and code: the cumulative figures are in `data-raw/lassa_cumulative_national.csv` and `data-raw/lassa_cumulative_state.csv`, and A9 runs in `R/addendum1_cumulative_gap.R`.

## References

- Abdulrahim A, Gulumbe BH, Adepoju VA. Increasing mortality during Nigeria's 2026 Lassa fever outbreak calls for earlier diagnosis and treatment. *Lancet Regional Health – Africa* 2026; 100135. doi:10.1016/j.lanafr.2026.100135
- Duvignaud A, Jaspard M, Etafo IC, et al. Lassa fever outcomes and prognostic factors in Nigeria (LASCOPE): a prospective cohort study. *Lancet Global Health* 2021; 9: e469-78.
- Kitagawa EM. Components of a difference between two rates. *Journal of the American Statistical Association* 1955; 50(272): 1168-94. doi:10.1080/01621459.1955.10501299
- Musa SS, Zhao S, Abdullahi ZU, Habib AG, He D. COVID-19 and Lassa fever in Nigeria: a deadly alliance? *International Journal of Infectious Diseases* 2022; 117: 45-47. doi:10.1016/j.ijid.2022.01.058
- Reuben RC, Gyar SD, Makut MD, Adoga MP. Co-epidemics: have measures against COVID-19 helped to reduce Lassa fever cases in Nigeria? *New Microbes and New Infections* 2021; 40: 100851. doi:10.1016/j.nmni.2021.100851
