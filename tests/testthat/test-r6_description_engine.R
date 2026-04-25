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
    }
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

test_that("DescriptionEngine returns NA when locale value is not scalar", {
  yaml_file <- tempfile(fileext = ".yaml")
  writeLines("summary.sun: {}", yaml_file)

  with_mocked_bindings(
    {
      engine <- DescriptionEngine$new(yaml_path = yaml_file)
      expect_true(is.na(engine$localize("summary.sun", "en")))
    },
    read_yaml = function(path) {
      list(
        `summary.sun` = list(
          en = c("first", "second")
        )
      )
    }
  )
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
    }
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
    }
  )
})

# ---------------------------------------------------------------------------
# get_summary() tests
# ---------------------------------------------------------------------------

write_semantic_yaml <- function(path) {
  writeLines(
    c(
      "summary.sun:",
      "  zh: \"\u592a\u967d\u4ee3\u8868\u60a8\u7684\u6838\u5fc3\u8eab\u4efd\u3001\u751f\u547d\u76ee\u7684\u8207\u81ea\u6211\u610f\u5fd7\u3002\"",
      "  en: \"The Sun represents your core identity, life purpose, and conscious will.\"",
      "  de: \"Die Sonne repräsentiert Ihre Kernidentität, Ihren Lebensweg und Ihren bewussten Willen.\"",
      "summary.moon:",
      "  zh: \"\u6708\u4eae\u53cd\u6620\u60a8\u7684\u60c5\u611f\u4e16\u754c\u3001\u76f4\u89ba\u672c\u80fd\u8207\u5167\u5728\u9700\u6c42\u3002\"",
      "  en: \"The Moon reflects your emotional world, instinctive reactions, and inner needs.\"",
      "  de: \"Der Mond spiegelt Ihre emotionale Welt, Instinkte und innere Bedürfnisse wider.\"",
      "aspect.conjunction:",
      "  zh: \"\u5408\u76f8\uff1a\u5169\u984c\u884c\u661f\u80fd\u91cf\u878d\u5408\uff0c\u5f37\u5316\u5f7c\u6b64\u7684\u5f71\u97ff\u529b\uff0c\u5f62\u6210\u5c08\u6ce8\u8207\u96c6\u4e2d\u7684\u529b\u91cf\u3002\"",
      "  en: \"Conjunction: two planets merge their energies, intensifying and focusing their combined influence.\"",
      "  de: \"Konjunktion: Zwei Planeten verbinden ihre Energien und verstärken gemeinsam ihren Einfluss.\"",
      "aspect.square:",
      "  zh: \"\u56db\u5206\u76f8\uff1a\u5169\u984c\u884c\u661f\u5f62\u6210\u5f35\u529b\u8207\u6469\u64e6\uff0c\u4fc3\u4f7f\u900f\u904e\u885d\u7a81\u5c0b\u627e\u52d5\u529b\u8207\u7a81\u7834\u3002\"",
      "  en: \"Square: two planets create tension and friction, driving growth through challenge and conflict.\"",
      "  de: \"Quadrat: Zwei Planeten erzeugen Spannung und Reibung, die durch Herausforderungen Wachstum antreibt.\""
    ),
    path
  )
}

test_that("get_summary() returns correct payload shape for a supported planet", {
  yaml_file <- tempfile(fileext = ".yaml")
  write_semantic_yaml(yaml_file)
  engine <- DescriptionEngine$new(yaml_path = yaml_file)

  result <- engine$get_summary("sun", "en")

  expect_type(result, "list")
  expect_named(result, c("summary_id", "summary", "lang", "metadata"))
  expect_equal(result$summary_id, "summary.sun")
  expect_equal(result$lang, "en")
  expect_false(is.na(result$summary))
  expect_true(nzchar(result$summary))
  expect_type(result$metadata, "list")
})

test_that("get_summary() localizes correctly for zh, en, de", {
  yaml_file <- tempfile(fileext = ".yaml")
  write_semantic_yaml(yaml_file)
  engine <- DescriptionEngine$new(yaml_path = yaml_file)

  zh_result <- engine$get_summary("sun", "zh")
  en_result <- engine$get_summary("sun", "en")

  expect_false(is.na(zh_result$summary))
  expect_false(is.na(en_result$summary))
  expect_false(identical(zh_result$summary, en_result$summary))
})

test_that("get_summary() falls back to English when locale is absent", {
  yaml_file <- tempfile(fileext = ".yaml")
  # moon has zh, en, and de in write_semantic_yaml helper; use 'fr' to test fallback
  write_semantic_yaml(yaml_file)
  engine <- DescriptionEngine$new(yaml_path = yaml_file)

  # Request moon in 'fr' — not present in helper YAML, should fall back to EN
  result <- engine$get_summary("moon", "fr")

  expect_equal(result$lang, "fr")  # lang field preserves original request
  expect_false(is.na(result$summary))  # EN fallback provides content
  expect_equal(result$summary, "The Moon reflects your emotional world, instinctive reactions, and inner needs.")
})

test_that("get_summary() embeds caller-supplied metadata in return value", {
  yaml_file <- tempfile(fileext = ".yaml")
  write_semantic_yaml(yaml_file)
  engine <- DescriptionEngine$new(yaml_path = yaml_file)

  meta <- list(degree = 15.5, sign = "Aries")
  result <- engine$get_summary("moon", "en", metadata = meta)

  expect_equal(result$metadata$degree, 15.5)
  expect_equal(result$metadata$sign, "Aries")
})

test_that("get_summary() errors on unsupported planet_key", {
  yaml_file <- tempfile(fileext = ".yaml")
  write_semantic_yaml(yaml_file)
  engine <- DescriptionEngine$new(yaml_path = yaml_file)

  expect_error(engine$get_summary("pluto", "en"), "Unsupported planet_key")
  expect_error(engine$get_summary("uranus", "en"), "Unsupported planet_key")
  expect_error(engine$get_summary("vertex", "en"), "Unsupported planet_key")
})

test_that("get_summary() errors on invalid planet_key argument types", {
  yaml_file <- tempfile(fileext = ".yaml")
  write_semantic_yaml(yaml_file)
  engine <- DescriptionEngine$new(yaml_path = yaml_file)

  expect_error(engine$get_summary(NULL, "en"), "planet_key must be a non-empty single character string")
  expect_error(engine$get_summary(NA_character_, "en"), "planet_key must be a non-empty single character string")
  expect_error(engine$get_summary("", "en"), "planet_key must be a non-empty single character string")
  expect_error(engine$get_summary(c("sun", "moon"), "en"), "planet_key must be a non-empty single character string")
})

test_that("get_summary() errors on invalid lang argument", {
  yaml_file <- tempfile(fileext = ".yaml")
  write_semantic_yaml(yaml_file)
  engine <- DescriptionEngine$new(yaml_path = yaml_file)

  expect_error(engine$get_summary("sun", NULL), "lang must be a non-empty single character string")
  expect_error(engine$get_summary("sun", NA_character_), "lang must be a non-empty single character string")
  expect_error(engine$get_summary("sun", ""), "lang must be a non-empty single character string")
  expect_error(engine$get_summary("sun", c("en", "zh")), "lang must be a non-empty single character string")
})

test_that("get_summary() errors when metadata is not a list", {
  yaml_file <- tempfile(fileext = ".yaml")
  write_semantic_yaml(yaml_file)
  engine <- DescriptionEngine$new(yaml_path = yaml_file)

  expect_error(engine$get_summary("sun", "en", metadata = "not-a-list"), "metadata must be a list")
  expect_error(engine$get_summary("sun", "en", metadata = 42L), "metadata must be a list")
})

test_that("get_summary() errors when metadata contains non-serializable references", {
  yaml_file <- tempfile(fileext = ".yaml")
  write_semantic_yaml(yaml_file)
  engine <- DescriptionEngine$new(yaml_path = yaml_file)

  expect_error(
    engine$get_summary("sun", "en", metadata = list(handler = function() TRUE)),
    "non-serializable"
  )
  expect_error(
    engine$get_summary("sun", "en", metadata = list(ctx = new.env())),
    "non-serializable"
  )
})

test_that("get_summary() return value contains no R6 or environment references (serialization-safe)", {
  engine <- DescriptionEngine$new()

  result <- engine$get_summary("sun", "en")

  # Plain scalar types only — no R6, environments, or external references
  expect_false(inherits(result, "R6"))
  expect_false(is.environment(result))
  expect_true(is.list(result))
  expect_true(is.character(result$summary_id))
  expect_true(is.character(result$summary) || is.na(result$summary))
  expect_true(is.character(result$lang))
  expect_true(is.list(result$metadata))
})

# ---------------------------------------------------------------------------
# get_aspect_summary() tests
# ---------------------------------------------------------------------------

test_that("get_aspect_summary() returns correct payload shape for major aspects", {
  yaml_file <- tempfile(fileext = ".yaml")
  write_semantic_yaml(yaml_file)
  engine <- DescriptionEngine$new(yaml_path = yaml_file)

  for (asp in c("conjunction", "square")) {
    result <- engine$get_aspect_summary(asp, "en")

    expect_type(result, "list")
    expect_named(result, c("aspect_id", "summary", "lang"))
    expect_equal(result$aspect_id, paste0("aspect.", asp))
    expect_equal(result$lang, "en")
    expect_false(is.na(result$summary))
    expect_true(nzchar(result$summary))
  }
})

test_that("get_aspect_summary() localizes correctly across zh, en, de", {
  yaml_file <- tempfile(fileext = ".yaml")
  write_semantic_yaml(yaml_file)
  engine <- DescriptionEngine$new(yaml_path = yaml_file)

  zh_result <- engine$get_aspect_summary("conjunction", "zh")
  en_result <- engine$get_aspect_summary("conjunction", "en")

  expect_false(is.na(zh_result$summary))
  expect_false(is.na(en_result$summary))
  expect_false(identical(zh_result$summary, en_result$summary))
})

test_that("get_aspect_summary() falls back to English when locale is absent", {
  yaml_file <- tempfile(fileext = ".yaml")
  write_semantic_yaml(yaml_file)
  engine <- DescriptionEngine$new(yaml_path = yaml_file)

  # 'conjunction' has zh + en + de in write_semantic_yaml; use 'square' which has zh/en/de too.
  # Request a locale not in the helper YAML (e.g. 'fr')
  result <- engine$get_aspect_summary("square", "fr")

  expect_equal(result$lang, "fr")
  expect_false(is.na(result$summary))
  expect_equal(result$summary, "Square: two planets create tension and friction, driving growth through challenge and conflict.")
})

test_that("get_aspect_summary() errors on excluded aspects (sextile etc.)", {
  yaml_file <- tempfile(fileext = ".yaml")
  write_semantic_yaml(yaml_file)
  engine <- DescriptionEngine$new(yaml_path = yaml_file)

  expect_error(engine$get_aspect_summary("sextile", "en"), "Unsupported aspect")
  expect_error(engine$get_aspect_summary("quintile", "en"), "Unsupported aspect")
})

test_that("get_aspect_summary() errors on invalid argument types", {
  yaml_file <- tempfile(fileext = ".yaml")
  write_semantic_yaml(yaml_file)
  engine <- DescriptionEngine$new(yaml_path = yaml_file)

  expect_error(engine$get_aspect_summary(NULL, "en"), "aspect_label must be a non-empty single character string")
  expect_error(engine$get_aspect_summary("", "en"), "aspect_label must be a non-empty single character string")
  expect_error(engine$get_aspect_summary(c("conjunction", "square"), "en"), "aspect_label must be a non-empty single character string")
  expect_error(engine$get_aspect_summary("conjunction", NULL), "lang must be a non-empty single character string")
  expect_error(engine$get_aspect_summary("conjunction", ""), "lang must be a non-empty single character string")
})

test_that("get_aspect_summary() return value is serialization-safe (no R6/env)", {
  engine <- DescriptionEngine$new()

  result <- engine$get_aspect_summary("trine", "en")

  expect_false(inherits(result, "R6"))
  expect_false(is.environment(result))
  expect_true(is.list(result))
  expect_true(is.character(result$aspect_id))
  expect_true(is.character(result$summary) || is.na(result$summary))
  expect_true(is.character(result$lang))
})

# ---------------------------------------------------------------------------
# filter_major_aspects() tests
# ---------------------------------------------------------------------------

test_that("filter_major_aspects() retains only conjunction, square, trine, opposition", {
  df <- data.frame(
    planet  = c("sun", "moon", "sun", "venus", "mars"),
    planet2 = c("moon", "mars", "mars", "saturn", "saturn"),
    aspect  = c("conjunction", "sextile", "square", "trine", "opposition"),
    stringsAsFactors = FALSE
  )

  result <- filter_major_aspects(df)

  expect_equal(nrow(result), 4L)
  expect_true(all(result$aspect %in% c("conjunction", "square", "trine", "opposition")))
  expect_false("sextile" %in% result$aspect)
})

test_that("filter_major_aspects() returns empty data frame when no major aspects present", {
  df <- data.frame(
    planet  = "sun",
    planet2 = "moon",
    aspect  = "sextile",
    stringsAsFactors = FALSE
  )

  result <- filter_major_aspects(df)

  expect_equal(nrow(result), 0L)
})

test_that("filter_major_aspects() errors on non-data-frame input", {
  expect_error(filter_major_aspects(list(aspect = "conjunction")), "aspect_result must be a data frame")
  expect_error(filter_major_aspects("conjunction"), "aspect_result must be a data frame")
})

test_that("filter_major_aspects() errors when 'aspect' column is absent", {
  df <- data.frame(planet = "sun", planet2 = "moon", stringsAsFactors = FALSE)
  expect_error(filter_major_aspects(df), "must contain an 'aspect' column")
})

test_that("filter_major_aspects() preserves data.frame shape for single-column input", {
  df <- data.frame(
    aspect = c("conjunction", "sextile"),
    stringsAsFactors = FALSE
  )

  result <- filter_major_aspects(df)

  expect_true(is.data.frame(result))
  expect_named(result, "aspect")
  expect_equal(nrow(result), 1L)
  expect_equal(result$aspect[[1]], "conjunction")
})

