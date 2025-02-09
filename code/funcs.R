#' Add Ordinal Suffix to Day of the Month
#'
#' This function takes a numeric day of the month (1–31) and returns it as a 
#' string with an appropriate ordinal suffix ("st", "nd", "rd", "th").
#'
#' @param day An integer representing the day of the month (1–31).
#'
#' @return A character string with the day and its ordinal suffix (e.g., "1st", "2nd", "3rd", "4th").
#'
#' @examples
#' add_ordinal_suffix(1)   # Returns "1st"
#' add_ordinal_suffix(2)   # Returns "2nd"
#' add_ordinal_suffix(13)  # Returns "13th"
#' add_ordinal_suffix(21)  # Returns "21st"
#'
add_ordinal_suffix <- function(day) {
  suffixes <- c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")
  if (day %% 100 %in% c(11, 12, 13)) return(paste0(day, "th"))
  return(paste0(day, suffixes[day %% 10 + 1]))
}



#' Wrap Text with Hanging Indent and Bullet
#'
#' This function wraps a given text to a specified width and applies a hanging indent 
#' to subsequent lines after the first. A bullet ("•") is added at the beginning of the first line.
#'
#' @param text A character string to be wrapped.
#' @param width An integer specifying the maximum number of characters per line (default is 50).
#' @param indent A character string used for the hanging indent (default is two spaces `"  "`).
#'
#' @return A character string with wrapped text, a bullet at the start of the first line, 
#'         and a hanging indent for subsequent lines.
#'
#' @examples
#' wrap_text_hanging("This is a sample text that will be wrapped and indented.", width = 30)
#'
#' wrap_text_hanging("This text will have a bullet point at the start and a hanging indent for other lines.", 
#'                  width = 40, indent = "    ")
#'
wrap_text_hanging <- function(text, width = 50, indent = "  ") {
  wrapped <- strwrap(text, width = width)  # Wrap text
  wrapped[1] <- paste0("• ", wrapped[1])   # Add bullet to first line
  if (length(wrapped) > 1) {
    wrapped[-1] <- paste0(indent, wrapped[-1])  # Apply hanging indent to other lines
  }
  paste(wrapped, collapse = "\n")  # Combine wrapped lines
}



#' Create the sub-heading themes used in the epi bulletin
#'
#' This function creates a styled sub-heading as a `flextable` object with customizable appearance 
#' based on the specified level. It supports different background colors, text sizes, and row gap sizes.
#'
#' @param text A character string to be displayed as the sub-heading.
#' @param level An integer specifying the sub-heading level (default is 1). 
#'              Level 1 applies a green background, while Level 2 applies an orange background.
#' @param top_row_gap An integer specifying the font size of the top gap row (default is 4).
#' @param bottom_row_gap An integer specifying the font size of the bottom gap row (default is 4).
#'
#' @return A `flextable` object styled as a sub-heading with centered text, specified colors, and gap rows.
#'
#' @examples
#' sub_heading("Section Title", level = 1)
#' sub_heading("Sub-section Title", level = 2, top_row_gap = 6, bottom_row_gap = 6)
#'
#' @export
sub_heading <- function(text = NULL, level = 1, top_row_gap = 4, bottom_row_gap = 4) {
  out <- data.frame(
    Title = c("", text, "")
  ) %>%
    flextable() %>%
    set_table_properties(layout = "fixed") %>%
    bold(i = 2, j = 1) %>%
    fontsize(i = 1, j = 1, size = top_row_gap) %>%  # Adjust the size of the top gap row
    fontsize(i = 3, j = 1, size = bottom_row_gap) %>%  # Adjust the size of the bottom gap row
    align(i = 2, j = 1, align = "center") %>%  # Center-align the main text
    border_remove() %>%
    padding(i = 1:3, j = 1, padding.top = 0, padding.bottom = 0, padding.left = 0, padding.right = 0) %>%
    delete_part(part = "header")
  
  if (level == 1) {
    out <- out %>%
      width(j = 1, width = 7.5) %>%  # Main content
      bg(i = 2, j = 1, bg = "#92d050") %>%  # Light Green Background
      color(i = 2, j = 1, color = "black") %>%
      fontsize(i = 2, j = 1, size = 14)
  }
  
  if (level == 2) {
    out <- out %>%
      width(j = 1, width = 7.5) %>%  # Main content
      bg(i = 2, j = 1, bg = "#f79646") %>% #Orange background
      color(i = 2, j = 1, color = "white") %>%
      fontsize(i = 2, j = 1, size = 12)
  }
  
  return(out)
}


