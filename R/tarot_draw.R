#' Shuffle the deck
#'
#' @param number_of_cards Integer. Total number of cards in the deck. Defaults to 78.
#' @param has_reverse Logical. Whether the cards can be reversed. Defaults to TRUE.
#' @return A data.frame representing the shuffled deck, containing card IDs (id) and their reversed status (is_reversed).
#' @export
shuffle_deck <- function(number_of_cards = 78, has_reverse = TRUE) {

  # Generate a sequence from 1 to number_of_cards and shuffle it
  shuffled_ids <- sample(1:number_of_cards, size = number_of_cards, replace = FALSE)

  # Generate reversed status for the entire deck
  if (has_reverse) {
    # Randomly assign TRUE (reversed) or FALSE (upright)
    is_reversed <- sample(c(TRUE, FALSE), size = number_of_cards, replace = TRUE)
  } else {
    # If no reversed cards are allowed, set all to FALSE
    is_reversed <- rep(FALSE, number_of_cards)
  }

  # Combine the results into a data.frame for the drawing function to use
  deck <- data.frame(
    id = shuffled_ids,
    is_reversed = is_reversed
  )

  return(deck)
}

#' Draw random cards from a deck
#'
#' @param n Integer. The number of cards to draw.
#' @param deck A data.frame representing the deck of cards.
#' @return A data.frame containing the randomly drawn cards.
#' @export
draw_cards <- function(n, deck) {

  # Validate the number of cards to draw
  if (n <= 0) {
    stop("The number of cards to draw must be greater than 0.")
  }

  if (n > nrow(deck)) {
    stop("Cannot draw more cards than available in the deck.")
  }

  # Randomly draw 'n' indices from the available rows in the deck
  drawn_indices <- sample(seq_len(nrow(deck)), size = n, replace = FALSE)

  # Extract the randomly selected cards
  drawn_cards <- deck[drawn_indices, , drop = FALSE]

  return(drawn_cards)
}

