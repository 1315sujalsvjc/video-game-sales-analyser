analyze_sales <- function(data) {
 d<-dplyr::filter(data,!is.na(Global_Sales));top<-dplyr::arrange(d,dplyr::desc(Global_Sales))|>dplyr::select(dplyr::any_of(c("Rank","Name","Platform","Year","Genre","Publisher","Global_Sales")))|>dplyr::slice_head(n=10);v<-d$Global_Sales
 s<-data.frame(metric=c("total","average","median","maximum","minimum"),global_sales_millions=if(length(v))c(sum(v),mean(v),median(v),max(v),min(v))else rep(NA_real_,5))
 readr::write_csv(top,"output/tables/top_games.csv");readr::write_csv(s,"output/tables/sales_summary.csv");cat("Total global sales (millions):",sum(v),"\n");if(nrow(top))cat("Top game:",top$Name[1],"\n");invisible(list(top_games=top,summary=s))
}
