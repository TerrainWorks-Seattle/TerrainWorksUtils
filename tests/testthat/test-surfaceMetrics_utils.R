test_that("get_executable_path: DEMutilities folder and files.zip exist", {
  folder <- system.file(dir = "/DEMutilities", package = "TerrainWorksUtils")
  zipfile <- file.path(folder, "files.zip")

  expect_true(dir.exists(folder))
  expect_true(file.exists(zipfile))
})

test_that("get_executable_path: no executables folder, should be created", {
  folder <- system.file(dir = "/DEMutilities", package = "TerrainWorksUtils")
  zipfile <- file.path(folder, "files.zip")

  # Delete the /DEMutilities/files folder if it exists already
  unlink(file.path(folder, "files"), recursive = TRUE)
  expect_true(dir.exists(folder))
  expect_false(dir.exists(file.path(folder, "files")))

  # Executables should now be present in a new /files folder
  get_executable_path()

  expect_true(dir.exists(folder))
  expect_true(dir.exists(file.path(folder, "files")))
  expect_true(file.exists(file.path(folder, "files/MakeGrids.exe")))
})

test_that("get_executable_path empty executables folder", {
  folder <- system.file(dir = "/DEMutilities", package = "TerrainWorksUtils")
  zipfile <- file.path(folder, "files.zip")

  # Delete the /DEMutilities/files folder if it exists already
  unlink(file.path(folder, "files/*"))
  expect_true(dir.exists(folder))
  expect_true(dir.exists(file.path(folder, "files")))
  expect_false(dir.exists(file.path(folder, "files/MakeGrids.exe")))

  # Files should be unzipped into the existing /files folder
  get_executable_path()

  expect_true(dir.exists(folder))
  expect_true(dir.exists(file.path(folder, "files")))
  expect_true(file.exists(file.path(folder, "files/MakeGrids.exe")))

})

test_that("get_executable_path executables already unzipped", {
  folder <- system.file(dir = "/DEMutilities", package = "TerrainWorksUtils")
  zipfile <- file.path(folder, "files.zip")

  # Nothing should change
  get_executable_path()

  expect_true(dir.exists(folder))
  expect_true(dir.exists(file.path(folder, "files")))
  expect_true(file.exists(file.path(folder, "files/MakeGrids.exe")))


})
