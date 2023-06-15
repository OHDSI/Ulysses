# How to Run {{{ Study }}}

## Pre-Study Technical Requirements

To run this study you must setup the [HADES environment](https://ohdsi.github.io/Hades/rSetup.html). 

-   [R](https://cloud.r-project.org/) installed, (version 4.0 or greater)
-   [R Studio](https://posit.co/download/rstudio-desktop/) installed
-   On Windows: [RTools](https://cran.r-project.org/bin/windows/Rtools/) installed
-   [Java](https://www.java.com/en/) installed

You also require your site data to be mapped to the [OMOP CDM](https://ohdsi.github.io/CommonDataModel/) and administered on one of the following supported database platforms:

- Microsoft SQL Server
- Microsoft Parallel Data Warehouse
- Oracle
- PostgreSQL
- Google BigQuery
- Amazon RedShift
- Snowflake 
- Apache Hive
- Apache Spark
- Apache Impala

## Setting up the {{{ Study }}} study locally

### Via Github

  1) Go to the github url: [{{{ Url }}}]({{{ Url }}})
  2) Select the green code button revealing a dropdown menu
  3) Select download zip
  4) Unzip the folder on your local computer that is easily accessible within R Studio
  5) Open folder the folder and select `{{{ Repo }}}.Rproj`

### Via Picard

Modify and use the following code block to download the study locally. 

```
picard::downloadStudy(org = {{{ Org }}}, repo = {{{ Repo }}}, savePath = "path_to_dir")
```

## How to execute the {{{ Study }}} study

<!----Add directions on how to execute ------------>

## Additional Information for {{{ Study }}} study


<!----Add additional information or delete ------------>
