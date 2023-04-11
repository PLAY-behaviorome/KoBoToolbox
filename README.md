# KoBoToolbox

Code to process survey data collected via KoBoToolbox for the [Play & Learning Across a Year (PLAY) Project](https://play-project.org).

## Overview

PLAY project researchers use [KoBoToolbox](https://www.kobotoolbox.org/) to collect data from participant families.
Data from all sessions at all PLAY data collection sites is pushed to a common set of files on the KoBoToolbox server.
We need to i) download, ii) parse, iii) clean, and iv) export these files into forms more suitable for data analysis.

## File organization

### Getting started

1. Clone the repository to a new RStudio project.
2. Install the `renv' package via `install.packages('renv')`.
3. Install the package dependencies via `renv::activate()` and then `renv::restore()`.
4. Create (blank) data directories via `source('R/functions.R')` and then `create_data_dirs()`.
5. Add the KoBoToolbox API key to `~/.Renviron`.
6. Restart the R environment.
7. Download the data via `targets::make()`
8. Generate the report via `bookdown::render_book('src')`.
9. View the report in `docs/index.html`.

### Synched to GitHub

- `R/`: Sets of R functions that support workflow.
    - `_old/`: deprecated `*.R` files
    - `functions.R` active functions.
- `_targets/`: Files related to this project's use of the [`{targets}` package](https://books.ropensci.org/targets/).
- `_targets.R`: active targets file.
surveys.
- `docs/`: Output files for bootstrap4-styled gitbook; the output of `bookdown::render()`. See <https://PLAY-behaviorome.github.io/KoBoToolbox/> for the rendered site.
- `renv/`: Files supporting the project's use of the `renv` package to support computational reproducibility.
- `renv.lock`: file used by `renv` to maintain list of package dependencies.
- `preamble.tex`: file used for rendering PDF version of web book.
- `src`
    - `index.Rmd`, `01-setup.Rmd`, `02-screening.Rmd`, `03-home-visit.Rmd`, `04-data-dictionary.Rmd`, `05-post-visit-notes.Rmd`, and `06-visualization.Rmd`: Files that form the basis of the web book.
    - `bookdown.yml` and `output.yml`: parameter files used by `bookdown` to render web book.
    - `include/`: Assets to be included in rendered web book.
        - `bib/`: Reference files
        - `css/`: CSS files
        - `img/`: Image files.

### Private, not synched to GitHub

- There are a set of data files and intermediates that are _not_ synched to GitHub for privacy reasons.
