load_video_game_data <- function(path="data/raw/vgsales.csv") {
 if(!file.exists(path)) stop("Dataset not found: ",path,". Put vgsales.csv in data/raw/.",call.=FALSE)
 x<-readr::read_csv(path,show_col_types=FALSE,progress=FALSE)
 cat("Rows:",nrow(x)," Columns:",ncol(x),"\nColumns:",paste(names(x),collapse=", "), "\nFirst six rows:\n"); print(utils::head(x,6)); cat("\nSummary:\n"); print(summary(x)); x
}
