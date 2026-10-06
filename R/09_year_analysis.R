analyze_years <- function(data) {
 x<-data|>dplyr::filter(!is.na(Year),Year>=1950,Year<=as.numeric(format(Sys.Date(),"%Y"))+2)|>dplyr::group_by(Year)|>dplyr::summarise(global_sales=sum(Global_Sales,na.rm=TRUE),game_count=dplyr::n(),.groups="drop")|>dplyr::arrange(Year)
 top_years<-dplyr::arrange(x,dplyr::desc(global_sales))|>dplyr::slice_head(n=10)
 readr::write_csv(x,"output/tables/yearly_sales.csv");readr::write_csv(dplyr::select(x,Year,game_count),"output/tables/yearly_releases.csv")
 if(nrow(x)){cat("Most releases:",x$Year[which.max(x$game_count)],"; highest sales:",x$Year[which.max(x$global_sales)],"\n");cat("Top 10 years by global sales:\n");print(top_years);if(nrow(x)>1)cat("First-to-last trend:",if(tail(x$global_sales,1)>x$global_sales[1])"upward"else"down or flat","\n")}
 invisible(list(yearly_sales=x,yearly_releases=dplyr::select(x,Year,game_count),top_years=top_years))
}