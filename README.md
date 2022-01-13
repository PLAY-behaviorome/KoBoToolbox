# KoBoToolbox

Code to process survey data collected via KoBoToolbox for the PLAY Project.

## Overview

PLAY project researchers use [KoBoToolbox](https://www.kobotoolbox.org/) to collect data from participant families.
Data from all sessions at all PLAY data collection sites is pushed to a common set of files on the KoBoToolbox server.
We need to i) download, ii) parse, iii) clean, and iv) export these files into forms more suitable for data analysis.

## Workflows

- [Working document](http://PLAY-behaviorome.github.io/KoBoToolbox/gather-clean.html) to develop and test import, cleaning, and export procedures.
- There is an [Rmd document](export-measures-by-form.Rmd) to export all measures by form. It creates an [HTML](export-measures-by-form.html) report with useful information for conducting quality assurance (QA) reviews.
- [Workbook](gather_clean_visit.html) for gathering and cleaning survey data collected at the visit, and its accompanying [Rmd](gather_clean_visit.Rmd) document.
- A later [workbook](visit_gather_clean_export) that consolidates the lessons learned.
- [Workbook](gather_clean_previsit.html) focusing on the screening or previsit questionnaires.
- An internal (private to PLAY staff) report on the 1.0 Data Release. 

## Future work

- Export questions for each measure.
- Integrate session/subject-specific data files with PLAYmate app.
- Create QA dashboard for PLAY team.

## File organization

- Exported `.xlsx` files from KoBoToolbox.org are saved to a restricted directory on Box.
At present, this export is manual.
- Local copies of this repo contain a `csv/` directory that is private (and not synched to GitHub) because it contains identifiable data.
    - Within `csv/` there are directories for
        - `aggregate/`: CSV files by measure for all participants in the latest export.
        - `by_form/`: CSV files by KoBoToolbox form, e.g., \{'12_English', '12_Bilingual_English', etc.\}, and measure, e.g., \{'basic_demog', 'dll_eng_long', etc.\}.
        - `by_session/`: CSV files by session (participant) for each measure.
            - The file names follow this convention: `PLAY_<Databrary_vol_id><Databrary_session_id>_<measure>.csv`
        - There is a hidden directory, `.analysis`, that contains CSV files used by the code base:
            - `form_measures.csv`: table of form stems, e.g., `12_English`, measure labels, e.g., `mbcdi_eng_short`, and the column range in the associated exported `.xlsx` file where the data for the measure can be found, e.g., `ER:HU`. This range helps make the extraction of relevant columns more efficient.
            - `sites_databrary.csv`: table of PLAY site id's, e.g. 'NYUNI, site names, e.g., 'New York University', and the PLAY Project Databrary volume ID, e.g., 899.