# KoBoToolbox

Code to process survey data collected via KoBoToolbox for the PLAY Project.

## Overview

PLAY project researchers use [KoBoToolbox](https://www.kobotoolbox.org/) to collect data from participant families.
Data from all sessions at all PLAY data collection sites is pushed to a common set of files on the KoBoToolbox server.
We need to i) download, ii) parse, iii) clean, and iv) export these files into forms more suitable for data analysis.

## Workflows

### Release 1.0

- To download the 1.0 release from KoBoToolbox and generate both form-level and aggregate CSV files, run `rmarkdown::render('release_1.0_export_clean.Rmd', params=list(databrary_login="<default@youremail.com>"))`, substituting your actual Databrary account ID (email) for `<default@yourmail.com>`.
- To generate some summary tables and plots, run `rmarkdown::render("release_1.0_survey_report.Rmd")` with the default parameters. **Note:** Because this report contains potentially sensitive information about site performance, it is not synched to GitHub.

Preliminary work toward an automatically-generated data dictionary can be found in [`data_dictionary_workbook.html`](data_dictionary_workbook.html).

## File organization

### Synched to GitHub

- `R/`: Sets of R functions that support the R Markdown documents.
- `renv/`: Local copies of R packages used.

### Private, not synched to GitHub

-`csv/`:
    - `release_1.0/`: 
        - `raw/`: CSV files converted from the most recent KoBoToolbox exported `.xlsx` files.
        - `identifiers_removed/`: CSV files with names, addresses, birthdates, and other identifying information removed.
        - `aggregate`: CSV files with (`PLAY_non_mbcdi_all_merge.csv`) and without (`PLAY_non_mbcdi_all.csv`) the accompanying Databrary session-level info.
    - `_old/:` Files related to an initial effort to download and clean the survey data.
        - `aggregate/`: CSV files by measure for all participants in the latest export.
        - `by_form/`: CSV files by KoBoToolbox form, e.g., \{'12_English', '12_Bilingual_English', etc.\}, and measure, e.g., \{'basic_demog', 'dll_eng_long', etc.\}.
        - `by_session/`: CSV files by session (participant) for each measure.
            - The file names follow this convention: `PLAY_<Databrary_vol_id><Databrary_session_id>_<measure>.csv`
        - There is a hidden directory, `.analysis`, that contains CSV files used by the code base:
            - `form_measures.csv`: table of form stems, e.g., `12_English`, measure labels, e.g., `mbcdi_eng_short`, and the column range in the associated exported `.xlsx` file where the data for the measure can be found, e.g., `ER:HU`. This range helps make the extraction of relevant columns more efficient.
            - `sites_databrary.csv`: table of PLAY site id's, e.g. 'NYUNI, site names, e.g., 'New York University', and the PLAY Project Databrary volume ID, e.g., 899.
- `data_dict/`: Files supporting the data dictionary. This will eventually be exposed on GitHub.
- `tmp/`: Temporary directory only available locally.
- `txt/`: 
- `xlxs/`:
    - `release_1.0/`: Exported files for the 1.0 release. Not synched to GitHub.
            
## Future work

- Export questions for each measure.
- Integrate session/subject-specific data files with PLAYmate app.
- Create QA dashboard for PLAY team.
