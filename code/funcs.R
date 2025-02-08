# Function to add suffix to dates
add_ordinal_suffix <- function(day) {
  suffixes <- c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")
  if (day %% 100 %in% c(11, 12, 13)) return(paste0(day, "th"))
  return(paste0(day, suffixes[day %% 10 + 1]))
}

# Function to wrap text with hanging indent
wrap_text_hanging <- function(text, width = 50, indent = "  ") {
  wrapped <- strwrap(text, width = width)  # Wrap text
  wrapped[1] <- paste0("• ", wrapped[1])   # Add bullet to first line
  if (length(wrapped) > 1) {
    wrapped[-1] <- paste0(indent, wrapped[-1])  # Apply hanging indent to other lines
  }
  paste(wrapped, collapse = "\n")  # Combine wrapped lines
}


#Sub-heading theme
sub_heading <- function(text = NULL, level = 1, top_row_gap=4, bottom_row_gap=4) {
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
      bg(i = 2, j = 1, bg = "#f79646") %>%
      color(i = 2, j = 1, color = "white") %>%
      fontsize(i = 2, j = 1, size = 12)
  }
  
  return(out)
}

