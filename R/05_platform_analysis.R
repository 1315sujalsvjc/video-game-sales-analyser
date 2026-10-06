analyze_platforms <- function(data,minimum_games=20) {
 x<-data|>dplyr::filter(!is.na(Platform),!is.na(Global_Sales))|>dplyr::group_by(Platform)|>dplyr::summarise(total_sales=sum(Global_Sales),game_count=dplyr::n(),average_sales=mean(Global_Sales),.groups="drop")|>dplyr::arrange(dplyr::desc(total_sales))
 # Restrict average leader to 20+ releases to reduce small-sample distortion.
 a<-dplyr::filter(x,game_count>=minimum_games)|>dplyr::arrange(dplyr::desc(average_sales));t<-dplyr::slice_head(x,n=10)
 readr::write_csv(x,"output/tables/platform_sales.csv");readr::write_csv(t,"output/tables/top_platforms.csv");if(nrow(x))cat("Top platform:",x$Platform[1],"\n");if(nrow(a))cat("Top average platform (at least",minimum_games,"games):",a$Platform[1],"\n");invisible(list(platform_sales=x,top_platforms=t,average_leader=utils::head(a,1)))
}
