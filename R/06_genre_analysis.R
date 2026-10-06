analyze_genres <- function(data) {
 x<-data|>dplyr::filter(!is.na(Genre),Genre!="Unknown",!is.na(Global_Sales))|>dplyr::group_by(Genre)|>dplyr::summarise(total_sales=sum(Global_Sales),game_count=dplyr::n(),average_sales=mean(Global_Sales),.groups="drop")|>dplyr::arrange(dplyr::desc(total_sales))
 readr::write_csv(x,"output/tables/genre_sales.csv");if(nrow(x))cat("Top sales genre:",x$Genre[1],"; most frequent:",x$Genre[which.max(x$game_count)],"\n");invisible(x)
}
