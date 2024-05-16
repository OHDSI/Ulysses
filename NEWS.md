Ulysses 0.0.7
=============

* fix importAtlasCohorts => use RJSONIO with digits
* change all keyring fns to use only db. 
* update function to review credentials
* fix and simplify cohortManifest function #26
* fix migration bug #25

# Ulysses 0.0.6

-   correct start_study vignette
-   add fix in initConfig

# Ulysses 0.0.5

-   Add functions to import credentials
-   Add functions to import ATLAS assets
-   Write vignettes to creds and ATLAS
-   add folder cohorts/conceptSets/json

# Ulysses 0.0.4

-   Finalize Directory
-   Finalize meta fields for \_study.yml
-   Reconfigure `newOhdsiStudy()` with better meta inputs
-   Add functions to interface with \_study.yml
-   Reroute makeFiles to directory and meta fields
-   Add `makeMigrationsScript()` template
-   Update study hub website format
-   Update vignette

# Ulysses 0.0.3

-   add study hub builder
-   convert all documents to qmd
-   add functions to define study settings prior to build
-   add cohort functions
    -   `cohortManifest` lists cohorts used in study
    -   `makeCohortDetails` renders human-readable file to convey cohort definitions
    -   add versioning to cohort definitions

# Ulysses 0.0.2

-   Add keyring functions and update SetupKeyring.R template
-   Update repository request email template
-   Add git function to publish repo to remote
-   Fix bugs in HowToRun and KeyringSetup template

# Ulysses 0.0.1

-   Add initial functionality
    -   file templates for:
        -   Analysis script
        -   Cohort Details
        -   Contribution Guidelines
        -   Example Script
        -   How to run
        -   Internal function file
        -   Meeting minutes
        -   News
        -   Email templates
        -   Readme
        -   Results report
        -   Study Protocol
        -   Study SAP
        -   Config.yml
    -   initialize ohdsi study in R
-   Added a `NEWS.md` file to track changes to the package.
