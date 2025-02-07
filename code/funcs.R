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
