test_that("Story 3.1: Taiwan Gold Standard Calculation", {

  # 1. Setup Input Data (Brad Pitt)
  # Birth: Dec 18, 1963, 06:33 AM
  # Location: Shawnee, Oklahoma, USA
  t_str <- "1963-12-18 06:33:00"
  tz <- "America/Chicago"
  time_obj <- as.POSIXct(t_str, tz = tz)

  # Shawnee, OK Coordinates
  lat <- 35.3273
  lng <- -96.9253

  # 2. Run Calculation
  # We call the main wrapper function
  result <- calculate_planet_position(time_obj, tz, lng, lat)

  # Extract Data
  pos_table <- result$planetary_position

  # Ensure the ephemeris path is set correctly for the test environment
  # Replace 'yourPackageName' with your actual package name
  se_path <- system.file("se_data", package = "yourPackageName")
  if (se_path != "") {
    swephR::swe_set_ephe_path(se_path)
  }

  # 2. Run Calculation
  result <- calculate_planet_position(time_obj, tz, lng, lat)
  pos_table <- result$planetary_position

  # 3. Assertions (The "Gold Standard")
  # We convert degrees/minutes to absolute longitude: (Sign Index * 30) + Degrees + (Minutes/60)
  tol <- 0.1 # Degree tolerance

  # A. Check SUN (Sagittarius 25° 51')
  # Sagittarius starts at 240°. 240 + 25.85 = 265.85
  sun_lon <- pos_table[row.names(pos_table) == "sun", "deg"]
  expect_equal(sun_lon, 265.85, tolerance = tol, label = "Sun: Sagittarius 25°51'")

  # B. Check MOON (Capricorn 22° 49')
  # Capricorn starts at 270°. 270 + 22.81 = 292.81
  moon_lon <- pos_table[row.names(pos_table) == "moon", "deg"]
  expect_equal(moon_lon, 292.81, tolerance = tol, label = "Moon: Capricorn 22°49'")

  # C. Check JUPITER (Aries 09° 50')
  # Aries starts at 0°. 0 + 9.83 = 9.83
  jup_lon <- pos_table[row.names(pos_table) == "jupiter", "deg"]
  expect_equal(jup_lon, 9.83, tolerance = tol, label = "Jupiter: Aries 09°50'")

  # D. Check ASCENDANT (Sagittarius 11° 53')
  # 240 + 11.88 = 251.88
  asc_lon <- pos_table[row.names(pos_table) == "asc", "deg"]
  expect_equal(asc_lon, 251.88, tolerance = 0.5, label = "Asc: Sagittarius 11°53'")

  # E. Check MC (Virgo 26° 58')
  # Virgo starts at 150°. 150 + 26.96 = 176.96
  mc_lon <- pos_table[row.names(pos_table) == "mc", "deg"]
  expect_equal(mc_lon, 176.96, tolerance = 0.5, label = "MC: Virgo 26°58'")

  # F. Structure Validation
  expected_planets <- c("sun", "moon", "mercury", "venus", "mars", "jupiter",
                        "saturn", "uranus", "neptune", "pluto", "asc", "mc")
  expect_true(all(expected_planets %in% row.names(pos_table)))
  expect_false(any(is.na(pos_table$deg)))
})
