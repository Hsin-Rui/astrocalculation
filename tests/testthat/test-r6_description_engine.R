library(testthat)

write_sample_yaml <- function(path) {
  writeLines(
    c(
      "welcome_message:",
      "  zh: \"歡迎來到星盤引擎\"",
      "  en: \"Welcome to the chart engine\"",
      "  de: \"Willkommen bei der Horoskop-Engine\""
    ),
    path
  )
}

test_that("DescriptionEngine loads YAML once during initialization", {
  call_count <- 0L
  yaml_file <- tempfile(fileext = ".yaml")
  write_sample_yaml(yaml_file)

  with_mocked_bindings(
    {
      engine <- DescriptionEngine$new(yaml_path = yaml_file)

      expect_true(inherits(engine, "DescriptionEngine"))
      expect_equal(call_count, 1L)

      expect_equal(engine$localize("welcome_message", "en"), "Welcome to the chart engine")
      expect_equal(engine$localize("sun_sign_label", "de"), "Sternzeichen")
      expect_equal(call_count, 1L)
    },
    read_yaml = function(path) {
      call_count <<- call_count + 1L
      list(
        welcome_message = list(
          zh = "歡迎來到星盤引擎",
          en = "Welcome to the chart engine",
          de = "Willkommen bei der Horoskop-Engine"
        ),
        sun_sign_label = list(
          zh = "太陽星座",
          en = "Sun sign",
          de = "Sternzeichen"
        )
      )
    },
    .env = asNamespace("astrocalculation")
  )
})

test_that("DescriptionEngine uses default system.file path when yaml_path is omitted", {
  engine <- DescriptionEngine$new()

  expect_true(inherits(engine, "DescriptionEngine"))
  expect_equal(engine$localize("welcome_message", "en"), "Welcome to the chart engine")
})

test_that("DescriptionEngine returns localized values for zh, en, de", {
  yaml_file <- tempfile(fileext = ".yaml")
  write_sample_yaml(yaml_file)

  engine <- DescriptionEngine$new(yaml_path = yaml_file)

  expect_equal(engine$localize("welcome_message", "zh"), "歡迎來到星盤引擎")
  expect_equal(engine$localize("welcome_message", "en"), "Welcome to the chart engine")
  expect_equal(engine$localize("welcome_message", "de"), "Willkommen bei der Horoskop-Engine")
})

test_that("DescriptionEngine returns NA for missing keys", {
  yaml_file <- tempfile(fileext = ".yaml")
  write_sample_yaml(yaml_file)

  engine <- DescriptionEngine$new(yaml_path = yaml_file)

  expect_true(is.na(engine$localize("missing_key", "en")))
})

test_that("DescriptionEngine returns NA for invalid locale", {
  yaml_file <- tempfile(fileext = ".yaml")
  write_sample_yaml(yaml_file)

  engine <- DescriptionEngine$new(yaml_path = yaml_file)

  expect_true(is.na(engine$localize("welcome_message", "fr")))
})

test_that("DescriptionEngine handles invalid localize input deterministically", {
  yaml_file <- tempfile(fileext = ".yaml")
  write_sample_yaml(yaml_file)

  engine <- DescriptionEngine$new(yaml_path = yaml_file)

  expect_true(is.na(engine$localize(NULL, "en")))
  expect_true(is.na(engine$localize(NA_character_, "en")))
  expect_true(is.na(engine$localize("", "en")))
  expect_true(is.na(engine$localize(c("welcome_message", "sun_sign_label"), "en")))

  expect_true(is.na(engine$localize("welcome_message", NULL)))
  expect_true(is.na(engine$localize("welcome_message", NA_character_)))
  expect_true(is.na(engine$localize("welcome_message", "")))
  expect_true(is.na(engine$localize("welcome_message", c("en", "de"))))
})

test_that("DescriptionEngine errors for malformed YAML, directory path, and unnamed keys", {
  valid_yaml <- tempfile(fileext = ".yaml")
  write_sample_yaml(valid_yaml)

  with_mocked_bindings(
    {
      expect_error(
        DescriptionEngine$new(yaml_path = valid_yaml),
        "Failed to parse description YAML"
      )
    },
    read_yaml = function(path) {
      stop("syntactic problem while parsing YAML")
    },
    .env = asNamespace("astrocalculation")
  )

  yaml_dir <- tempfile(pattern = "desc-yaml-dir-")
  dir.create(yaml_dir)
  expect_error(
    DescriptionEngine$new(yaml_path = yaml_dir),
    "Description YAML file not found."
  )

  with_mocked_bindings(
    {
      expect_error(
        DescriptionEngine$new(yaml_path = valid_yaml),
        "Description YAML must be a named list of keys."
      )
    },
    read_yaml = function(path) {
      unname(list(list(en = "Welcome to the chart engine")))
    },
    .env = asNamespace("astrocalculation")
  )
})
