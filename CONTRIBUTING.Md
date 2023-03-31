### Contributing  
When contributing to this repository, please first discuss the change you wish to make via issue, email, or any other method with the owners before making a change. It is preferable to add new parsing function and object functions rather than any alterations to existing functions.

Pull/merge request process
Branch from the master branch. If you are implementing a feature name it feature/name_of_feature, if you are implementing a bugfix name it bug/issue_name. Update the README.md and other documentation with details of major changes to the interface, this includes new environment variables, useful file locations and container parameters. Once you are ready for review please open a pull/merge request to the master branch and tag a reviewer. You may merge the Pull/Merge Request in once it has been reviewed.

Changes should not be pushed directly to the master branch if you made changes to the code.

Do not push any files that may contain data, such as .ipynb, ipynb checkpoints. .RData files or .RHistory files. If you do, you should notify the owner of the repository. Do not push "broken" code, i.e. code that produces an error when executed.

### Code style  
The following style rules help make the code base easier to read and understand. You should stick to these rules wherever possible.

We name variables using few nouns in lowercase, e.g. mapping_names or increment. We name functions using verbs in lowercase, e.g. map_variables_to_names or change_values. We avoid full stops in column and object names, as this can be confusing to users of other programming languages such as python where full stops cannot be used in this way. With a few exceptions, we do not load whole packages by using the library() function. Instead, we reference the namespace directly, e.g. rstanarm::stan_gamm4(). The exceptions to this are packages that use custom operators, such as the use of the + operator in ggplot2, or the pipe (%>%) in magrittr. You may reference magrittr pipes and ggplot2 functions without referring to the namespace directly, within the cismrp package. In general, we also aim to follow the rules set out in the tidyverse style guide. Code that deviates from this guidance without good reason should be amended.
