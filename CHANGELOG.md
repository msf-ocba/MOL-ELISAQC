ELISAQC NEWS
================


# History

This shiny application was created to provide a user-friendly tool to automate processing, interpretation and quality control of commercial enzyme-linked immunosorbent assay (ELISA) kit results generated in a mobile outbreak investigation laboratory. The application is designed to work as a stand-alone program on a dedicated laptop without an internet connection.  The application is being developed iteratively.  This change log will indicate when the first version has been completed and is ready for beta-testing. 


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



