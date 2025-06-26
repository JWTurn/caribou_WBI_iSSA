
## Inferring wildlife population trends from hierarchical habitat selection: a case study with boreal caribou

[![DOI](https://zenodo.org/badge/375803485.svg)](https://doi.org/10.5281/zenodo.15750844)

- Authors:
  - [Julie W. Turner](https://www.julwturner.com)
  - Samuel Haché
  - James Hodson
  - Philip D. McLoughlin
  - Tatiane Micheletti
  - Michael J. L. Peers
  - Agnès Pelletier
  - Eliot McIntire

This repository contains the data and code accompanying the paper
“Inferring wildlife population trends from hierarchical habitat
selection: a case study with boreal caribou”.

## Data

Data processing and harmonizing occurred in scripts 1-5. Data included
here are the aspatial, calculated movement parameters (steps and turn
angles) and extracted covariates for each step used in analyses (scripts
6-9). Descriptions of the variables in the data for analysis are below.

| variable | description | units |
|:---|:---|:---|
| sl\_ | step length | m |
| ta\_ | turn angle | radians |
| id | individual animal ID |  |
| jurisdiction | province or territory where caribou data originated |  |
| pop | herd, management unit, local population as defined by jurisdiction |  |
| t1\_ | datetime at the beginning of the step |  |
| t2\_ | datetime at the end of the step |  |
| dt\_ | time difference between beginnning and end of step |  |
| case\_ | whether a step was used (1) or random (0) |  |
| step_id\_ | identifier of the the step in the track |  |
| year | year of the data point | years |
| int.year | 5 year interval that the data point occurred (2010-2014: 2010, 2015-2019: 2015) |  |
| prop_wets_start | proportion of wetlands at the start of the step within a 850m buffer of the point |  |
| prop_veg_start | proportion of other vegetation at the start of the step within a 850m buffer of the point |  |
| prop_needleleaf_start | proportion of needleleaf at the start of the step within a 850m buffer of the point |  |
| prop_mixforest_start | proportion of deciduous and mixed forest at the start of the step within a 850m buffer of the point |  |
| prop_wets_end | proportion of wetlands at the end of the step within a 850m buffer of the point |  |
| prop_veg_end | proportion of other vegetation at the end of the step within a 850m buffer of the point |  |
| prop_needleleaf_end | proportion of needleleaf at the end of the step within a 850m buffer of the point |  |
| prop_mixforest_end | proportion of deciduous and mixed forest at the end of the step within a 850m buffer of the point |  |
| ts_harv_start | time since forest harvest at the start of the step | years |
| ts_harv_end | time since forest harvest at the end of the step | years |
| ts_fires_start | time since fire at the start of the step | years |
| ts_fires_end | time since fire at the end of the step | years |
| distlf_start | distance to paved linear features at the start of the step | m |
| distlf_end | distance to paved linear features at the end of the step | m |
| distlf_other_start | distance to unpaved linear features at the start of the step | m |
| distlf_other_end | distance to unpaved linear features at the end of the step | m |
| disturbance_start | binary of if the start of the step was within a polygonal anthropogenic disturbance |  |
| disturbance_end | binary of if the end of the step was within a polygonal anthropogenic disturbance |  |
| indiv_step_id | combination of individual and step ID to identify steps by individual |  |
