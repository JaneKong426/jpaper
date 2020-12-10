#' BMI
#' 
#' @param weight in kg
#' @param height in m or cm. This function can automatically guess the unit.
#' 
#' @export 
BMI <- function(weight, height) {
    bmi = weight / (height)^2
    if (bmi < 1) bmi <- weight / (height/100)^2
    bmi
}
