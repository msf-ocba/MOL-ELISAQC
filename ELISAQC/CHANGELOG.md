ELISAQC NEWS
================


# History

This shiny application was created to provide a user-friendly tool to automate processing, interpretation and quality control of commercial enzyme-linked immunosorbent assay (ELISA) kit results generated in a mobile outbreak investigation laboratory. The application is designed to work as a stand-alone program on a dedicated laptop without an internet connection.  The application is being developed iteratively.  This change log will indicate when the first version has been completed and is ready for beta-testing. 

# Change log: 02 October 2021

- All code chunks converted to individual functions and tested
- Functions to export to and import from Go.Data added
- Functions added to derive concentrations for pertussis ELISAs from a standard curve, using the drc package
- Functions converted to a package
- Clean-up and movement of redundant scripts to archive


# Change log: 02 August 2021

- Date time of ELISA results reformatted to match input requirements of Go.Data
- Lower and upper limits of negative and positive external controls changed to kit limits for same (ODs of reference values created in field test too low due to sample deterioration and therefore not representative).
- Additional variables added to results output (name of laboratory and sample ID) as needed for Go.Data
- Function added to extract n characters at the end of a string (used to derive sample ID from patient ID)


# Change log: 11 February 2021

- R script tidied up and minor corrections applied to resolve issues with change to French logic variables
- **New**: parameterised R markdown report created for quality control outputs - all shiny inputs working


# Change log: 01 February 2021

- `Test_script.R` completed which includes all code required to process ELISA results (to add to Shiny app)
- `Data` folder added containing example ELISA data for testing


# Change log: 04 January 2021

**First commit:**

- Basic shiny app shell with `.bat` file that can be launched from the desktop by double-clicking on the icon.

- First function included is an interactive ELISA plate template where users can type in their sample IDs and indicate the position of calibrators and controls on the plate.  This was created with the `rhandsontable` package.  The appearance (aesthetics) of the table will be tidied up once the other main features have been added.



