add_ordinal_suffix <- function(day) {
  suffixes <- c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")
  if (day %% 100 %in% c(11, 12, 13)) return(paste0(day, "th"))
  return(paste0(day, suffixes[day %% 10 + 1]))
}
