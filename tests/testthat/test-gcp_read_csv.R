# creating a temporary directory for saving the CSVs
dir.create("temp/")
temp_dir <- "temp/"

# creating dummy test dataframes
test_gcp_read_csv1 <- data.frame(
  "characters" = c("A", "B", "C"),
  "numbers" = seq(1, 3),
  "dates" = seq(as.Date("2022/06/16"), as.Date("2022/06/18"), by = "days")
)

test_gcp_read_csv2 <- data.frame(
  "character1" = c("A", "B", "C"),
  "character2" = c("D", "E", "F"),
  "character3" = c("G", "H", "I")
)

file_name_1 <- "test_gcp_read_csv1.csv"
file_name_2 <- "test_gcp_read_csv2.csv"

# creating csvs from the test files
readr::write_csv(x = test_gcp_read_csv1, file = paste0(temp_dir, file_name_1))
readr::write_csv(x = test_gcp_read_csv2, paste0(temp_dir, file_name_2))

testthat::test_that("GCP upload for test runs successfully", {
  skip_on_ci()
  expect_silent(
    invisible({
      # Authenticate session
      authenticate_gcp()

      # since we need the files in Google Cloud Platform to actually test the function - this loop checks first
      # if these files are already on GCP in the WIP bucket - if they are not then it uploads them
      # this will prevent creating multiple uploads to the bucket every time the tests are run

      for (file in c(file_name_1, file_name_2)) {
        if (length(googleCloudStorageR::gcs_list_objects(gcp_paths$wip_bucket, prefix = file))
        == 0) {
          googleCloudStorageR::gcs_upload(
            file = file, bucket = gcp_paths$wip_bucket, name = file,
            predefinedAcl = "bucketLevel"
          )
        }
      }
    })
  )
})

testthat::test_that("function returns an error when file_paths is missing", {
  skip_on_ci()
  testthat::expect_error(
    gcp_read_csv(file_paths = NULL, bucket = "wip_bucket"),
    "You have not provided any file paths to the file_paths argument"
  )
})

testthat::test_that("function returns an error when bucket does not match one of expected arguments", {
  skip_on_ci()
  testthat::expect_error(
    gcp_read_csv(file_paths = "testing", bucket = "banana_bucket"),
    "'arg' should be one of \"wip_bucket\", \"data_bucket\", \"review_bucket\""
  )
})

testthat::test_that("function returns a dataframe if one file name is provided", {
  skip_on_ci()
  dummy_data1 <- gcp_read_csv(file_paths = file_name_1, bucket = "wip_bucket")
  dummy_data2 <- gcp_read_csv(file_paths = file_name_2, bucket = "wip_bucket")
  testthat::expect_true(is.data.frame(dummy_data1))
  testthat::expect_true(is.data.frame(dummy_data2))
})

testthat::test_that("function returns a list of dataframes with correct names if multiple file_paths provided", {
  skip_on_ci()
  data_list <- list(dummy_data1 = file_name_1, dummy_data2 = file_name_2)
  dummy_list <- gcp_read_csv(file_paths = data_list, bucket = "wip_bucket")
  testthat::expect_true(class(dummy_list) == "list")
  testthat::expect_true(is.data.frame(dummy_list$dummy_data1))
  testthat::expect_true(is.data.frame(dummy_list$dummy_data2))
})

testthat::test_that("function returns correct dataframes", {
  skip_on_ci()
  dummy_data1 <- gcp_read_csv(file_paths = file_name_1, bucket = "wip_bucket")
  dummy_data2 <- gcp_read_csv(file_paths = file_name_2, bucket = "wip_bucket")
  data_list <- list(dummy_data1 = file_name_1, dummy_data2 = file_name_2)
  dummy_list <- gcp_read_csv(file_paths = data_list, bucket = "wip_bucket")
  testthat::expect_equal(dummy_data1, test_gcp_read_csv1)
  testthat::expect_equal(dummy_data2, test_gcp_read_csv2)
  testthat::expect_equal(dummy_list$dummy_data1, test_gcp_read_csv1)
  testthat::expect_equal(dummy_list$dummy_data2, test_gcp_read_csv2)
})

testthat::test_that("supplying col_types modifies the col_types of the output", {
  skip_on_ci()
  col_types <- list(
    numbers = "c",
    dates = "c"
  )
  dummy_data3 <- gcp_read_csv(
    file_paths = file_name_1, bucket = "wip_bucket",
    col_types = col_types
  )

  testthat::expect_true(class(dummy_data3$numbers) == "character")
  testthat::expect_true(class(dummy_data3$dates) == "character")
})

# removing temporary directory and CSVs created within
unlink(temp_dir, recursive = TRUE, force = TRUE)
