# gcptools

## 1.0.4 (Added functionality for vignettes)
- Added function get_column_types
- Added function list_recycling_bin and recover_recycling_bin
- Added function for viewing vignettes
- Added function create_config for reading multiple yaml files
## 1.0.3 (Add wrapper functions)
- Update pull request template to include example for styler
- Added function empty_recycle_bin
- Add function gcp_combine_configs
- Added functions for reading csv files
- Added function make_artifact_list
- Added a wrapper around a system call to gsutil mv/cp which moves / copies a file from your notebook to either the WIP or the review bucket
## 1.0.2 (Add logging of outputs)
- Added object gcp_paths 
- Added function to read .rda objects into notebooks
- Added new examples to functions
- Added functions to log pipeline outputs (not tested for incidence yet)
- Added pull request template
- (Corrected name of previous release)
## 1.0.1 (Initial logging function)
- We have added functions to measure memory and cpu usage
- We have added functions to check package version and git status
- We have added functions to upload logs to GCP buckets.
- We have added a function to download logs from GCP buckets and stitch them into a report.

## 1.0 (First working release)
See link for information on the meaning of R version numbers. Where possible please try to stick to this pattern when updating the changelog.

We are now confident that the core RAP product is stable, i.e. that existing functions will not change in a way that is backwards incompatible. Future changes are likely to include patches i.e. changes to the third number or the development of new feature changes to the second number.

For example: If you find a bug a and fix it you would create a new heading above in the change log with ### 1.0.1 and then describe what the bug you fixed was in the text below. for a new feature you'd add ### 1.1.0 and describe the new feature.
