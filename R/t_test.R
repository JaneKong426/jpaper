label_sd <- function (x, ...)
{
    x <- as.numeric(x)
    x <- x[!is.na(x)]
    y <- mean(x)
    sd <- sd(x)

    sprintf("%.2f\u00b1%.2f", y, sd) # ±
    # c(y = y, ymin = y - sd, ymax = y + sd, sd = sd)
}

length_valid <- function(.) {
    sum(!is.na(as.numeric(.)))
}

#' T test and TukeyHSD test
#'
#' @param x A numeric vector
#' @param y A numeric vector
#'
#' @seealso [stats::t.test()], [stats::TukeyHSD()]
#'
#' @import data.table
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(100)
#' y <- rnorm(100)
#' t_test(x, y)
#'
#' @importFrom stats aov t.test TukeyHSD
#' @export
t_test <- function(x, y) {
    # formula <- sprintf("%s~order", varname) %>% as.formula()
    # m <- aov(formula, df)
    # s <- TukeyHSD(m)
    d <- rbind(data.table(variable = "x", value = x), data.table(variable= "y", value = y))
    m <- aov(value ~ variable, d)
    s <- TukeyHSD(m)[[1]]

    # x <- df[order == "first"][[varname]]
    # y <- df[order == "second"][[varname]]

    r <- t.test(x, y)
    tval <- r$statistic[[1]]
    pvalue <- r$p.value

    first <- label_sd(x)
    second <- label_sd(y)

    data.table(first, second, s, tval, pvalue)
}

#' @param file The file path of xlsx
#' @param outfile The file path of xlsx for OUTPUTS
#'
#' @examples
#' \dontrun{
#' t_test_simple("a.xlsx")
#' }
#' @export
#' @importFrom openxlsx read.xlsx write.xlsx
#' @rdname t_test
t_test_simple <- function(file, outfile = NULL) {
    d <- read.xlsx(file)
    ans <- t_test(d[[1]], d[[2]])
    if (!is.null(outfile)) write.xlsx(ans, outfile)
    ans
}
