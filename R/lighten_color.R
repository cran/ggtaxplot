#' Lighten a Color
#'
#' This function lightens a given color by a specified factor.
#' @importFrom grDevices col2rgb rgb
#' @importFrom stats filter setNames 
#' @param color A character string representing a color in any valid format (e.g., hex code, color name).
#' @param factor A numeric value between 0 and 1 indicating the amount to lighten the color. 
#'               A factor of 0 means no change, while a factor of 1 results in white.
#' @return A character string representing the lightened color in RGB format.
#' @export
lighten_color <- function(color, factor) {
  rgb_values <- col2rgb(color)  # Convert color to RGB values
  rgb_values <- rgb_values + (255 - rgb_values) * factor  # Lighten the color
  return(rgb(rgb_values[1], rgb_values[2], rgb_values[3], maxColorValue = 255))  # Return as RGB color
}