clean_video_game_data <- function(data) {
 req<-c("Name","Platform","Year","Genre","Publisher","NA_Sales","EU_Sales","JP_Sales","Other_Sales","Global_Sales"); absent<-setdiff(req,names(data)); if(length(absent)) stop("Missing columns: ",paste(absent,collapse=", "),call.=FALSE)
 cat("Missing values before cleaning:\n"); print(colSums(is.na(data))); before<-nrow(data); data<-dplyr::distinct(data)
 data$Year<-suppressWarnings(as.numeric(as.character(data$Year))); ss<-c("NA_Sales","EU_Sales","JP_Sales","Other_Sales","Global_Sales")
 for(n in ss){data[[n]]<-suppressWarnings(as.numeric(as.character(data[[n]])));data[[n]][data[[n]]<0]<-NA_real_}
 data<-dplyr::filter(data,!is.na(Name),nzchar(trimws(as.character(Name))),if_any(dplyr::all_of(ss),~!is.na(.x)))
 for(n in c("Genre","Publisher")) data[[n]][is.na(data[[n]])|!nzchar(trimws(as.character(data[[n]])))]<-"Unknown"
 dir.create("data/processed",recursive=TRUE,showWarnings=FALSE);readr::write_csv(data,"data/processed/cleaned_vgsales.csv",na="")
 cat("Rows before:",before,"after:",nrow(data),"\n");data
}
