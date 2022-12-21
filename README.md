# KoBoToolbox

Code to process survey data collected via KoBoToolbox for the [Play & Learning Across a Year (PLAY) Project](https://play-project.org).

## Overview

PLAY project researchers use [KoBoToolbox](https://www.kobotoolbox.org/) to collect data from participant families.
Data from all sessions at all PLAY data collection sites is pushed to a common set of files on the KoBoToolbox server.
We need to i) download, ii) parse, iii) clean, and iv) export these files into forms more suitable for data analysis.

## File organization

### Synched to GitHub

- `R/`: Sets of R functions that support workflow.
    - `_old/`: deprecated `*.R` files
    - `functions.R` active functions.
- `_targets`: Files related to this project's use of the [`{targets}` package](https://books.ropensci.org/targets/).
- `demog_screening/`: deprecated files related to processing of the demographic screening surveys.
- `docs/`: Output files for bootstrap4-styled gitbook; the output of `bookdown::render()`. See <https://PLAY-behaviorome.github.io/KoBoToolbox/> for the rendered site.
- `home_visit/`: deprecated files related to processing the home visit questionnaire data.
- `include/`: Assets to be included in rendered web book.
    - `bib/`: Reference files
    - `css/`: CSS files
    - `img/`: Image files.
- `renv/`: Files supporting the project's use of the `renv` package to support computational reproducibility.
- `visualize`: deprecated files related to visualizing cleaned survey data.
- `index.Rmd`, `01-setup.Rmd`, `02-screening.Rmd`, `03-home-visit.Rmd`, `04-data-dictionary.Rmd`, `05-post-visit-notes.Rmd`, and `06-visualization.Rmd`: Files that form the basis of the web book.
- `bookdown.yml` and `output.yml`: parameter files used by `bookdown` to render web book.
- `preamble.tex`: file used for rendering PDF version of web book.
- `_OLDtargets.R`: deprecated `_targets.R` file used in old version of targets-based workflow.
- `_targets.R`: active targets file.
- `renv.lock`: file used by `renv` to maintain list of package dependencies.


### Private, not synched to GitHub

- There are a set of data files and intermediates that are _not_ synched to GitHub for privacy reasons.
