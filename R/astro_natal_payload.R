# astro_natal_payload.R
# Canonical natal chart calculation payload for story 2.5.1.
#
# All synchronous and asynchronous chart paths resolve to the same
# `calculate_natal_payload()` output before any rendering occurs.
# ---------------------------------------------------------------------------

#' Calculate the canonical natal chart payload
#'
#' Single authoritative entry point for natal chart data.  Both the
#' synchronous (\code{DataManager$update_chart}) and asynchronous
#' (\code{DataManager$update_chart_async}) paths call this function and
#' obtain the same structured result before any rendering-only file handoff.
#'
#' @param date A \code{POSIXct} datetime for the birth moment.
#' @param timezone IANA timezone string (e.g. \code{"Asia/Taipei"}).
#' @param longitude Geographic longitude of the birth location (decimal degrees).
#' @param latitude  Geographic latitude  of the birth location (decimal degrees).
#' @param selected_bodies Character vector of body names to include in the
#'   returned \code{planetary_positions} slice and aspect calculation.
#'   Defaults to all 22 recognised bodies (7 classical planets, outer planets,
#'   Chiron, mean node, ASC/MC/Vertex, and the 7 Greek Lots).
#' @param house_system One of \code{"whole_sign"}, \code{"placidus"},
#'   \code{"koch"}, or \code{"regiomontanus"}.  Default \code{"whole_sign"}.
#'
#' @return A named list with the following fields:
#'   \describe{
#'     \item{planetary_positions}{data.frame of positions filtered to
#'       \code{selected_bodies}, with columns \code{deg}, \code{speed},
#'       \code{sign}, \code{deg_in_sign}, \code{min_in_sign},
#'       \code{sec_in_sign}, \code{planet_glyphs}, \code{planet_color},
#'       \code{font_glyphs}, \code{font_size}.}
#'     \item{house_cusps}{data.frame of 12 house cusps for all five
#'       supported house systems.}
#'     \item{aspects}{data.frame of aspect results from
#'       \code{\link{calculate_aspect}}, with columns \code{planet},
#'       \code{planet2}, \code{aspect}, \code{deg_p1}, \code{deg_p2},
#'       \code{orb1}, \code{orb2}, \code{separation}, \code{draw_line}.}
#'     \item{planetary_conditions}{data.frame of sect, house placement,
#'       and essential dignities for the traditional seven planets.}
#'     \item{greek_lots}{data.frame of the seven Greek Lots with position
#'       columns and an additional \code{house} column giving each lot's
#'       house placement under \code{house_system}.}
#'     \item{selected_bodies}{Character vector echoing the
#'       \code{selected_bodies} argument.}
#'     \item{tables}{Named list of display-ready data frames:
#'       \describe{
#'         \item{aspects}{Key display columns from the aspects data frame
#'           (\code{planet}, \code{planet2}, \code{aspect}, \code{orb1},
#'           \code{separation}, \code{draw_line}).}
#'         \item{conditions}{Planetary conditions data frame enriched with
#'           a \code{sign} column from \code{planetary_positions}.}
#'       }
#'     }
#'   }
#'
#' @importFrom dplyr bind_rows left_join coalesce
#' @importFrom tibble rownames_to_column column_to_rownames
#' @export
calculate_natal_payload <- function(
    date,
    timezone,
    longitude,
    latitude,
    selected_bodies = c(
      "sun", "moon", "mercury", "venus", "mars",
      "jupiter", "saturn", "uranus", "neptune", "pluto",
      "chiron", "mean_node", "asc", "mc", "vertex",
      "spirit", "fortune", "necessity", "eros",
      "courage", "victory", "nemesis"
    ),
    house_system = c("whole_sign", "placidus", "koch", "regiomontanus")) {

  house_system <- match.arg(house_system)

  # ------------------------------------------------------------------
  # 1. Base planetary positions + house cusps
  # ------------------------------------------------------------------
  planet_pos <- calculate_planet_position(date, timezone, longitude, latitude)

  # ------------------------------------------------------------------
  # 2. Planetary conditions (traditional 7 planets; computed before
  #    Greek Lots are merged so the condition logic is clean)
  # ------------------------------------------------------------------
  conditions <- get_planetary_conditions(planet_pos, house_system)

  # ------------------------------------------------------------------
  # 3. Greek Lots — require conditions for sect determination
  # ------------------------------------------------------------------
  lots <- calculate_greek_lots(
    planet_pos$planetary_position,
    conditions
  )

  # Attach house placement for each lot
  asc_deg <- planet_pos$planetary_position$deg[
    match("asc", row.names(planet_pos$planetary_position))
  ]
  lots_norm_deg <- (lots$deg - asc_deg) %% 360
  names(lots_norm_deg) <- row.names(lots)

  norm_cusps <- normalize_degree(planet_pos)$normalized_house_cusps
  lots$house <- find_house_placement(lots_norm_deg, norm_cusps, house_system)

  # ------------------------------------------------------------------
  # 4. Merge Greek Lots into the full position data frame
  # ------------------------------------------------------------------
  all_positions <- dplyr::bind_rows(planet_pos$planetary_position, lots)

  # ------------------------------------------------------------------
  # 5. Filter to selected bodies
  # ------------------------------------------------------------------
  selected_positions <- all_positions[
    row.names(all_positions) %in% selected_bodies,
    ,
    drop = FALSE
  ]

  # ------------------------------------------------------------------
  # 6. Aspects — computed on the filtered position set
  # ------------------------------------------------------------------
  aspects_df <- calculate_aspect(selected_positions)

  # ------------------------------------------------------------------
  # 7. Display-ready tables shaped in astrocalculation (AC 6)
  # ------------------------------------------------------------------
  tables <- list(
    aspects    = shape_aspect_table(aspects_df),
    conditions = shape_conditions_table(conditions, planet_pos$planetary_position)
  )

  # ------------------------------------------------------------------
  # 8. Assemble and return canonical payload
  # ------------------------------------------------------------------
  list(
    planetary_positions  = selected_positions,
    house_cusps          = planet_pos$house_cusps,
    aspects              = aspects_df,
    planetary_conditions = conditions,
    greek_lots           = lots,
    selected_bodies      = selected_bodies,
    tables               = tables
  )
}

# ---------------------------------------------------------------------------
# Internal helpers — display table shaping (AC 6, Task 3)
# ---------------------------------------------------------------------------

#' Shape aspect data frame for display
#'
#' Selects the columns that are directly useful for UI rendering.
#' Raw intermediate columns (\code{deg_p1}, \code{deg_p2}, \code{orb2})
#' are excluded so consumers do not depend on them.
#'
#' @param aspects_df data.frame returned by \code{\link{calculate_aspect}}.
#' @return data.frame with columns \code{planet}, \code{planet2},
#'   \code{aspect}, \code{orb1}, \code{separation}, \code{draw_line}.
#'   Returns 0-row data frame (with the correct columns) when there are
#'   no aspects.
#'
shape_aspect_table <- function(aspects_df) {
  display_cols <- c("planet", "planet2", "aspect", "orb1", "separation", "draw_line")

  # Ensure all expected columns exist even when aspects_df has 0 rows.
  column_defaults <- list(
    planet = character(),
    planet2 = character(),
    aspect = character(),
    orb1 = numeric(),
    separation = character(),
    draw_line = logical()
  )
  missing_cols <- setdiff(display_cols, names(aspects_df))
  for (col in missing_cols) {
    aspects_df[[col]] <- column_defaults[[col]]
  }

  aspects_df[, display_cols, drop = FALSE]
}

#' Shape planetary conditions data frame for display
#'
#' Enriches the conditions table with a \code{sign} column taken from the
#' raw planetary positions, so UI layers do not need to join independently.
#' Greek Lots are not included here; their sign and house are available
#' directly on the \code{greek_lots} field of the payload.
#'
#' @param conditions data.frame returned by \code{\link{get_planetary_conditions}}.
#' @param planetary_position data.frame of raw planetary positions
#'   (\code{planet_pos$planetary_position} before Greek Lots are merged).
#' @return data.frame with the same rows as \code{conditions} plus a
#'   \code{sign} column.
#'
shape_conditions_table <- function(conditions, planetary_position) {
  sign_map <- data.frame(
    planet = row.names(planetary_position),
    sign   = planetary_position$sign,
    stringsAsFactors = FALSE
  )

  conditions |>
    tibble::rownames_to_column("planet") |>
    dplyr::left_join(sign_map, by = "planet") |>
    tibble::column_to_rownames("planet")
}
