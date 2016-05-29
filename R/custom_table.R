
.rbind_list <- function(List){
  stopifnot(is.list(List))
  is_data.frame <- sapply(List, is.data.frame)
  stopifnot(is_data.frame)

  extended <- mapply(function(df, nam){
    o <- rbind(NA, df)
    rownames(o)[1] <- nam
    return(o)
  },List, names(List))

  collapsed <- bind_rows(extended)

  rownames <- unname(as.vector(unlist(sapply(extended, rownames))))

  return(cbind(rownames = rownames, collapsed))
}

.custom_table <- function(df, caption = 'Performance',
                         class = "data table-custom", ...){

  # note: first column are rownames
  colnames <- colnames(df[,-1])
  stopifnot(!is.null(colnames))
  rownames <- df[,1]
  df <- df[,-1]

  group <- cumsum(apply(df, 1, function(row) all(is.na(row))))
  rows <- apply(suppressWarnings(cbind(group, rownames, df)), 1,
                FUN = function(row){
                  row[is.na(row)] <- ""
                  html_row <- tags$tr(tagList(lapply(row[-1], tags$td)))
                  if(all(row[-(1:2)] == "")){
                    group <- html_row$children[[1]][[1]]$rownames
                    group <- tagAppendAttributes(group, class = sprintf("group r%s", row[1]))
                    html_row$children[[1]][[1]]$rownames <- group
                  }
                  return(html_row)
                })

  table_body <- tags$tbody(tagList(rows))

  table_header <- tagList(
    mapply(function(column, id){
      tags$th(column, class = sprintf("column c%s", id))
    }, c("",colnames), seq_along(c("",colnames)), SIMPLIFY = FALSE)
  )

  table <- tags$div(tags$table(
    class = class,
    table_header,
    table_body
    ), id = "table")

  return(tags$div(tags$div(tags$caption(caption, align = "top"), id = "caption"),
                  table, id = "container"))
}
