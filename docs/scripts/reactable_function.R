reactable_function <- function(df, type = NULL){
  
  if (is.null(type)) {
    sort_col <- NULL
  } else {
    sort_col <- case_when(type == "batting" ~ "Runs", 
                          type == "bowling" ~ "Wkts", 
                          type == "fielding" ~ "Catches",
                          type == "keeping" ~ "Dismissals",
                          TRUE ~ "NULL")
  }

  reactable(df, 
            pagination = FALSE,
            #filterable = TRUE,
            defaultSortOrder = "desc",
            defaultSorted = sort_col,
            defaultColDef = colDef(
              sortNALast = TRUE,
              minWidth = 45,
              class = JS("function(rowInfo, colInfo, state) {
        // Highlight sorted columns
        for (var i = 0; i < state.sorted.length; i++) {
          if (state.sorted[i].id === colInfo.id) {
            return 'sorted'
          }
        }
      }"),
              headerClass = "box-score-header"),
            columns = list(      
              User.Id = colDef(show = FALSE),
              Name = colDef(
                name = "Name",
                defaultSortOrder = "asc",
                width = 130,
                cell = function(value, index) {
                  player_id <- df[index, "User.Id"]
                  player_url <- sprintf(
                    "https://www.lastmanstands.com/cricket-player/t20?playerid=%s", 
                    player_id)
                  tags$a(href = player_url, target = "_blank", value)
                }
              ),
              SR = colDef(format = colFormat(digits = 1)),
              Avg = colDef(format = colFormat(digits = 1)),
              Econ = colDef(format = colFormat(digits = 1)),
              #Dots = colDef(show = FALSE),
              Fours = colDef(name = "4s"),
              Sixes = colDef(name = "6s"),
              dot.perc = colDef(name = "Dot%", format = colFormat(percent = TRUE, digits = 1)),
              boundary.perc = colDef(name = "Boundary%", format = colFormat(percent = TRUE, digits = 1)),
              wide.perc = colDef(name = "Wide%", format = colFormat(percent = TRUE, digits = 1)),
              nb.perc = colDef(name = "NB%", format = colFormat(percent = TRUE, digits = 1)),
              wide.per.over = colDef(name = "Wides/Over", format = colFormat(digits = 1)),
              nb.per.over = colDef(name = "NB/Over", format = colFormat(digits = 1))
            ),
            showSortIcon = FALSE,
            highlight = TRUE,
            striped = TRUE,
            class = "box-score-tbl")
}
