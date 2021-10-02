ELISAQC: ELISA quality control application
================
02 octobre 2021

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview:

This R package is part of the tool-kit developed for the [Mobile
Outbreak Laboratory
(MOL)](https://msf-transformation.org/wp-content/uploads/2019/07/Mobile-Lab-Project-Summary-2019.06.13.pdf)
transformative investment capacity (TIC) project.

The primary objective is to automate processing and quality control of
raw results for [EUROIMMUN antibody detection enzyme-linked
immuno-sorbent assay (ELISA)
kits](https://www.euroimmun.com/products/infection-diagnostics/id/vaccine-preventable-diseases/)
frequently used in the diagnosis of measles and other
vaccine-preventable diseases, and link these results to patient
notification data. The application takes data generated by the Biotek
800 TS [Gen5
software](https://www.biotek.com/products/software-robotics-software/gen5-microplate-reader-and-imager-software/)
(version 3.08.01). Results are calculated and interpreted according to
EUROIMMUN kit instructions and standards. External control samples are
also evaluated. Processed results, including quality control flags and
sample ID labels, are made available in a Microsoft Excel file. Finally,
the processed ELISA results are pushed into a Go.Data database which in
turn is interrogated to extract a complete case line list, where case
and laboratory results have already been linked. A summary of the
quality control assessment for each plate is provided as a HTML report.

**Note:**

This application has been designed to be as user-friendly as possible.
The intended audience are laboratory technicians with no R or
programming experience, who may also have limited prior experience with
EUROIMMUN kits. The application can accept raw ELISA data from different
plate readers and will automatically look for the expected 96-well plate
structure to extract.

## Details:

`ELISAQC` has been developed as a stand-alone R package with a Shiny
interface for user inputs, which can be launched off-line via a .bat
file on the desktop (included in this repository). Note that the file
path in the .bat file needs to be amended to where the user has
installed this repository.

### User inputs:

1.  Position of calibrators, controls and sample IDs (.csv plate
    template via file uploader)
2.  Raw optical densities (browse and upload raw ELISA reader file)
3.  EUROIMMUN kit batch reference values and acceptable thresholds (via
    interactive dialogue box)

This user-provided data may optionally be saved as a configuration file
in future versions, which can then be uploaded and re-used for
subsequent runs with the same parameters.

### Data processing:

`ELISAQC` performs the following steps to process and interpret the
data:

-   Labels raw results with sample IDs as provided by the user in the
    first input step
-   Converts data from wide to long format (“tidy” data) for further
    processing
-   For measles and rubella kits: calculates the sample:calibrator ratio
-   For pertussis kits: derives concentration from a standard curve in
    international units per mL
-   Interprets the results for controls and patient samples as negative,
    indeterminate, or positive
-   Determines if positive and negative controls are within the kit
    threshold and applies a pass/fail flag
-   Determines if in-house positive controls are within range and
    applies a pass/fail flag (if provided)
-   Determines the coefficient of variation for control well duplicates
    and applies a pass/fail flag
-   Combines the three quality control flags to indicate if the run has
    passed overall or not

### Outputs:

`ELISAQC` provides the following outputs for the user:

-   Quality control report, containing:
    -   Summary table of quality control results
    -   Heatmap of calculated results in plate layout with labels
    -   Summary statement indicating if the run has passed the quality
        control criteria or not
-   Line list of processed and interpreted ELISA results for patient
    samples (.csv)
-   Processed ELISA results automatically exported to a Go.Data database
    and linked to patient data
-   Complete case line list extracted from Go.Data, including linked
    laboratory results and patient data

An example quality control report is shown below:

**A. Summary table of internal quality control assessment:**

![](Images/QCtable_updated.png?raw=true)

**B. Heatmap of ELISA plate (calibration-normalised ratios):**

![](Images/Plate%20heatmap.png?raw=true)

**C. Summary statement for QC report:**

If the run has failed the quality control evaluation, an informative
message advising on possible causes of the failure is printed, as
follows:

    #> Ce test ELISA a échoué aux procédures internes de contrôle de la qualité.
    #> Envisagez de refaire le test de la plaque.
    #> Les problèmes suivants doivent être vérifiés et résolus lors du dépannage:
    #> 
    #> A. Si les résultats sont trop faibles:
    #> - Le temps d'incubation était trop court
    #> - La température d'incubation était trop basse
    #> - Le tampon de lavage n'a pas été complètement retiré des puits
    #> - Il y a eu une rupture de la chaîne du froid pendant le stockage des réactifs
    #> 
    #> B. Si les résultats sont trop élevés:
    #> - Le temps d'incubation était trop long
    #> - La température d'incubation (ambiante) était trop élevée
    #> - Le lavage des assiettes était insuffisant (temps trop court ou pas assez de tampon de lavage utilisé)
    #> - Une contamination croisée s'est produite (si les valeurs du contrôle négatif sont trop élevées)
    #>  
    #> C. Si les Coefficients de variation (CV) sont trop élevés:
    #> - Les broches du lecteur ELISA peuvent etre mal alignées
    #> - Les composants optiques du lecteur ELISA peuvent être contaminés par de la poussière

It is assumed that under normal circumstances, users will run alignment
calibration checks on the ELISA reader prior to running a plate and not
proceed if these checks fail. In the event that equipment is being
tested in operational conditions, however, high coefficients of
well-to-well variation between duplicate samples can be a useful
indicator of the extent of impact of any alignment problems.

## Package dependencies:

The following R packages are used for the `ELISAQC` shiny app:

-   `devtools`
-   `shiny`
-   `knitr`
-   `rmarkdown`
-   `here`
-   `data.table`
-   `dplyr`
-   `tidyr`
-   `lubridate`
-   `ggplot2`
-   `flextable`
-   `officer`
-   `openxlsx`
-   `httr`
-   `jsonlite`
-   `drc`
-   `mass`

These packages have been defined as dependencies of ELISAQC.

## Updates

You can read about updates and changes to this app in the
**[news](https://github.com/msf-ocba/MOL/blob/master/CHANGELOG.md)**
page.

## Comments and suggestions:

This application is still in development; feedback is particularly
welcome. Please report any bugs, suggestions or feature requests as new
issues **[here](https://github.com/msf-ocba/MOL/issues)**, clearly
stating the name of the function you are referring to in the subject
title and tagging with an appropriate label.

## Contributing:

Contributions and submissions are welcome. These can be submitted to the
development branch of this repository in the first instance via a merge
request. Please refer to the **[contribution
guide](https://github.com/msf-ocba/MOL/blob/master/CONTRIBUTING.md)**
for further details.

Special thanks to James Fuller and Sara Hollis who co-wrote the code for
the Go.Data interoperability packages.

## Maintainer:

This project is currently being maintained by:

[Amy Mikhail](mailto:Amy.Mikhail@barcelona.msf.org)

Mobile Outbreak Laboratory

Transformational Investment Capacity (TIC) project

Médecins Sans Frontières - OCBA