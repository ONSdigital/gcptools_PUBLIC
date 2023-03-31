#### MAKE COMMIT HOOKS ####
#
# 1 commit_hooks_setup
#
#### End of contents ####

#' @title Commit hooks setup
#'
#' @description this function establishes commit hooks to ensure that ipynb
#' outputs cannot be pushed to github
#'
#' @param path_to_git_repo
#'
#' @return NULL
#'
#' @export
commit_hooks_setup <- function(path_to_git_repo) {
  setwd(path_to_git_repo)
  system("pip install pre-commit")
  system("pre-commit install")
}
