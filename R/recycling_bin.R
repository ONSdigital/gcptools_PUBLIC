##### RECYCLING BIN MODULE ####
# 1 list_recycling_bin
# 2 empty_recycling_bin
# 3 recover_recycling_bin
#### End of contents ####

#' @title list_recycling_bin
#'
#' @description This function lists the contents of the recycling bin.
#'
#' @return data.frame
#'
#' @examples
#'
#' # Call the function
#' list_recycling_bin()
#'
#' @export

list_recycling_bin <- function() {
  dir <- "/home/jupyter/.local/share/Trash/files"
  files_with_path <- list.files(dir, all.files = TRUE, full.names = TRUE, no.. = TRUE)

  if (length(files_with_path) > 0) {
    files <- list.files(dir, all.files = TRUE, full.names = FALSE, no.. = TRUE)
    type <- sapply(file.info(files_with_path)$isdir, function(x) {
      if (x) {
        "folder"
      } else {
        "file"
      }
    })

    return(data.frame(files, type))
  } else {
    print("Recycling bin is empty.")
  }

  return(invisible())
}

#' @title Delete files from "Trash" folder
#'
#' @description This function is a wrapper around a system call to rm which empties the trash folder.
#'
#' @return invisible
#'
#' @examples
#'
#' # Call the function
#' empty_recycling_bin()
#'
#' @export

empty_recycling_bin <- function() {
  dir <- "/home/jupyter/.local/share/Trash/files"

  if (length(list.files(dir)) != 0) {
    system(paste("rm -rf ", dir))
    print("Recycling bin has been emptied!")
  } else {
    print("Recycling bin is already empty!")
  }

  return(invisible)
}

#' @title Recover files from "Trash" folder
#'
#' @description This function is a wrapper around a system call to mv which transfers files from the trash folder to the user top level.
#'
#' @param recovery_dir Name of new directory to hold contents of Trash/files (defaults to "recovered").
#'
#' @return invisible
#'
#' @examples
#'
#' # Call the function
#' recover_recycling_bin()
#'
#' @export

recover_recycling_bin <- function(recovery_dir = "recovered") {
  trash_dir <- "/home/jupyter/.local/share/Trash/files"
  recovery_dir <- paste0("/home/jupyter/", recovery_dir)

  if (length(list.files(trash_dir)) != 0) {
    system(paste("mv ", trash_dir, " ", recovery_dir))
    print(paste0("Files moved to: ", recovery_dir))
  } else {
    print("Recycling bin is empty!")
  }

  return(invisible)
}
