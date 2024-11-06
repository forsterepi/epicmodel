## Resubmission
This is a resubmission. In this version I have replaced all occurrences of \dontrun{} in function documentation examples. Details: 

* The function that opens a shiny app is wrapped in if(interactive()){} (launch_steplist_creator.Rd, epicmodel_steplist.Rd)

* readRDS() in epicmodel_steplist.Rd now actually loads a file from folder inst/extdata

* In export_mechanism.Rd, the function saves a file in the temporary directory and deletes it afterwards. Saving is wrapped in if(interactive()){}.

* In mechanism.Rd, the use of export_mechanism() has been removed to make the example more concise

## R CMD check results

0 errors | 0 warnings | 2 notes

* New submission

* unable to verify current time

Note only appeared on my home computer. 
Checks with R-CMD-check GitHub Actions and win-builder service did not show it.

## revdepcheck results

There are currently no downstream dependencies for this package.
