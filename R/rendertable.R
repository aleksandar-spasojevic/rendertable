
#' custom renderTable function
#'
#' can render a list of tables
#'
#' @param expr a list with named \code{data.frame} objects in it
#' @param ... everything else like in \code{\link[shiny]{renderTable}}
#'
#' @export
rendertable <- function(expr, ..., env = parent.frame(), quoted = FALSE, func = NULL)
{
  if (!is.null(func)) {
    shinyDeprecated(msg = "renderTable: argument 'func' is deprecated. Please use 'expr' instead.")
  }
  else {
    df <- .rbind_list(expr)
    installExprFunction(df, "func", env, quoted)
  }
  markRenderFunction(tableOutput, function() {

    data <- func()
    if (is.null(data) || identical(data, data.frame()))
      return("")

    return( paste(utils::capture.output(.custom_table(data)), collapse = "\n") )
  })
}
