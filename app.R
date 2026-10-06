# Video Game Analytics Dashboard: single-page Shiny application.
packages <- c("shiny", "readr", "dplyr", "tidyr", "ggplot2", "DT")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  install_command <- paste0("install.packages(c(", paste(shQuote(missing_packages), collapse = ", "), "))")
  stop("Missing package(s): ", paste(missing_packages, collapse = ", "),
       ". Install once with: ", install_command, call. = FALSE)
}

standard_columns <- c("Rank", "Name", "Platform", "Year", "Genre", "Publisher",
                      "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")
sales_columns <- c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")
regional_names <- c(NA_Sales = "North America", EU_Sales = "Europe",
                    JP_Sales = "Japan", Other_Sales = "Other")
category_labels <- c(
  all = "All Games",
  pokemon = paste0("Pok", "\u00e9", "mon"),
  cricket = "Cricket",
  football = "Football / Soccer",
  racing = "Racing",
  action_adventure = "Action / Adventure"
)
category_about <- list(
  pokemon = paste("Analyze", paste0("Pok", "\u00e9", "mon"), "games and compare their platforms, publishers, sales and release trends."),
  cricket = "Explore cricket games and examine their sales across platforms and regions.",
  football = "Analyze football and soccer games, including major football franchises.",
  racing = "Explore racing games and compare their sales across platforms and years.",
  action_adventure = "Analyze games in the Action and Adventure genres."
)

clean_uploaded_data <- function(data) {
  rows_before <- nrow(data)
  # Normalize heading punctuation and capitalization, e.g. Global Sales -> Global_Sales.
  keys <- tolower(gsub("[^[:alnum:]]", "", names(data)))
  for (column in standard_columns) {
    key <- tolower(gsub("[^[:alnum:]]", "", column))
    if (!column %in% names(data) && key %in% keys) {
      names(data)[match(key, keys)] <- column
      keys <- tolower(gsub("[^[:alnum:]]", "", names(data)))
    }
  }
  if (!"Global_Sales" %in% names(data)) {
    alias <- match(TRUE, keys %in% c("sales", "totalsales", "worldwidesales", "worldsales", "unitssold"), nomatch = 0L)
    if (alias > 0L) names(data)[alias] <- "Global_Sales"
  }
  actual_regions <- intersect(names(regional_names), names(data))
  if (!"Global_Sales" %in% names(data) && !length(actual_regions))
    stop("Include Global_Sales, Sales, or at least one regional sales field.", call. = FALSE)

  original_fields_missing <- setdiff(standard_columns, names(data))
  data <- dplyr::distinct(data)
  duplicates_removed <- rows_before - nrow(data)
  if (!"Rank" %in% names(data)) data$Rank <- seq_len(nrow(data))
  if (!"Name" %in% names(data)) data$Name <- paste("Game", seq_len(nrow(data)))
  for (column in c("Platform", "Genre", "Publisher"))
    if (!column %in% names(data)) data[[column]] <- "Unknown"
  if (!"Year" %in% names(data)) data$Year <- NA_real_
  for (column in sales_columns) if (!column %in% names(data)) data[[column]] <- NA_real_

  data$Year <- suppressWarnings(as.numeric(as.character(data$Year)))
  for (column in sales_columns) {
    data[[column]] <- suppressWarnings(as.numeric(as.character(data[[column]])))
    data[[column]][data[[column]] < 0] <- NA_real_
  }
  if (length(actual_regions)) {
    regional_sum <- rowSums(data[actual_regions], na.rm = TRUE)
    has_regional_value <- rowSums(!is.na(data[actual_regions])) > 0
    derive <- is.na(data$Global_Sales) & has_regional_value
    data$Global_Sales[derive] <- regional_sum[derive]
  }
  data$Name <- trimws(as.character(data$Name))
  data <- dplyr::filter(data, !is.na(Name), nzchar(Name),
                        if_any(dplyr::all_of(sales_columns), ~ !is.na(.x)))
  missing_categories <- 0L
  for (column in c("Genre", "Publisher")) {
    blank <- is.na(data[[column]]) | !nzchar(trimws(as.character(data[[column]])))
    missing_categories <- missing_categories + sum(blank)
    data[[column]][blank] <- "Unknown"
  }
  if (!nrow(data)) stop("No usable rows remain after cleaning.")
  list(data = data, rows_before = rows_before, rows_after = nrow(data),
       duplicates_removed = duplicates_removed, category_values_filled = missing_categories,
       optional_missing = original_fields_missing, regional_columns = actual_regions)
}

filter_by_category <- function(data, category) {
  if (is.null(data) || category == "all") return(data)
  text_columns <- intersect(c("Name", "Genre", "Platform", "Publisher"), names(data))
  searchable <- if (length(text_columns)) {
    do.call(paste, c(lapply(data[text_columns], function(x) ifelse(is.na(x), "", as.character(x))), sep = " "))
  } else rep("", nrow(data))
  matches <- rep(FALSE, nrow(data))
  if (category == "action_adventure") {
    genre <- tolower(ifelse(is.na(data$Genre), "", as.character(data$Genre)))
    matches <- grepl("action|adventure", genre, perl = TRUE)
  } else {
    pattern <- switch(category,
      pokemon = paste0("pokemon|pok", "\u00e9", "mon"),
      cricket = "cricket",
      football = "football|soccer|fifa|pro[[:space:]]*evolution[[:space:]]*soccer|(^|[^[:alnum:]])pes([^[:alnum:]]|$)",
      racing = "racing|racer|speed|formula|nascar|gran[[:space:]]*turismo|mario[[:space:]]*kart",
      "")
    matches <- grepl(pattern, searchable, ignore.case = TRUE, perl = TRUE)
  }
  data[matches, , drop = FALSE]
}

summarize_filtered <- function(data, top_n, regional_columns) {
  empty <- !nrow(data)
  top_games <- data |>
    dplyr::filter(!is.na(Global_Sales)) |>
    dplyr::arrange(dplyr::desc(Global_Sales)) |>
    dplyr::select(dplyr::any_of(c("Rank", "Name", "Platform", "Year", "Genre", "Publisher", "Global_Sales"))) |>
    dplyr::slice_head(n = top_n)
  platforms <- data |>
    dplyr::filter(Platform != "Unknown", !is.na(Global_Sales)) |>
    dplyr::group_by(Platform) |>
    dplyr::summarise(Game_Count = dplyr::n(), Total_Sales = sum(Global_Sales),
                     Average_Sales = mean(Global_Sales), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(Total_Sales)) |>
    dplyr::slice_head(n = top_n)
  genres <- data |>
    dplyr::filter(Genre != "Unknown", !is.na(Global_Sales)) |>
    dplyr::group_by(Genre) |>
    dplyr::summarise(Game_Count = dplyr::n(), Total_Sales = sum(Global_Sales),
                     Average_Sales = mean(Global_Sales), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(Total_Sales)) |>
    dplyr::slice_head(n = top_n)
  publishers <- data |>
    dplyr::filter(Publisher != "Unknown", !is.na(Global_Sales)) |>
    dplyr::group_by(Publisher) |>
    dplyr::summarise(Game_Count = dplyr::n(), Total_Sales = sum(Global_Sales),
                     Average_Sales = mean(Global_Sales), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(Total_Sales)) |>
    dplyr::slice_head(n = top_n)

  if (length(regional_columns)) {
    regional <- data.frame(Region = unname(regional_names[regional_columns]),
      Sales = vapply(regional_columns, function(column) sum(data[[column]], na.rm = TRUE), numeric(1)))
    regional_games <- data |>
      dplyr::filter(rowSums(!is.na(dplyr::pick(dplyr::all_of(regional_columns)))) > 0) |>
      dplyr::mutate(Regional_Sales = rowSums(dplyr::pick(dplyr::all_of(regional_columns)), na.rm = TRUE)) |>
      dplyr::arrange(dplyr::desc(Regional_Sales)) |>
      dplyr::select(dplyr::any_of(c("Rank", "Name", "Platform", "Year", "Genre", "Publisher")), Regional_Sales) |>
      dplyr::slice_head(n = top_n)
  } else {
    regional <- data.frame(Region = "Regional data unavailable", Sales = 0)
    regional_games <- data.frame()
  }
  yearly <- data |>
    dplyr::filter(!is.na(Year), Year >= 1950, Year <= as.numeric(format(Sys.Date(), "%Y")) + 2) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(Game_Count = dplyr::n(), Global_Sales = sum(Global_Sales, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(Year)
  eligible_platform <- dplyr::filter(platforms, Game_Count >= 20) |>
    dplyr::arrange(dplyr::desc(Average_Sales))
  best_game <- if (nrow(top_games)) paste(top_games$Name[1], "(", top_games$Platform[1], ")") else "No global sales available"
  insights <- list(
    game = best_game,
    platform = if (nrow(platforms)) platforms$Platform[1] else "Platform data unavailable",
    genre = if (nrow(genres)) genres$Genre[1] else "Genre data unavailable",
    publisher = if (nrow(publishers)) publishers$Publisher[1] else "Publisher data unavailable",
    avg_platform = if (nrow(eligible_platform)) eligible_platform$Platform[1] else "No platform has 20+ games",
    region = if (length(regional_columns) && nrow(regional)) regional$Region[which.max(regional$Sales)] else "Regional data unavailable",
    year = if (nrow(yearly)) yearly$Year[which.max(yearly$Global_Sales)] else "Year data unavailable"
  )
  list(empty = empty, top_games = top_games, platforms = platforms, genres = genres,
       publishers = publishers, regional = regional, regional_games = regional_games,
       yearly = yearly, insights = insights)
}

dark_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#151b23", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#151b23", colour = NA),
      text = ggplot2::element_text(colour = "#e8edf2"),
      plot.title = ggplot2::element_text(face = "bold", colour = "#f4f7fa", size = 14),
      axis.title = ggplot2::element_text(colour = "#c6d0da"),
      axis.text = ggplot2::element_text(colour = "#c6d0da"),
      panel.grid.major = ggplot2::element_line(colour = "#303a46"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = "#151b23", colour = NA),
      legend.key = ggplot2::element_rect(fill = "#151b23", colour = NA),
      legend.text = ggplot2::element_text(colour = "#d9e1e8"),
      legend.title = ggplot2::element_text(colour = "#f4f7fa")
    )
}

build_plots <- function(data, result, top_n, regional_columns) {
  theme <- dark_theme()
  plot_or_null <- function(plot_data, plot) if (nrow(plot_data)) plot else NULL
  top_games_plot <- result$top_games
  top_platform_plot <- result$platforms
  top_genre_plot <- result$genres
  top_publisher_plot <- result$publishers
  plots <- list(
    games = plot_or_null(top_games_plot, ggplot2::ggplot(top_games_plot,
      ggplot2::aes(reorder(Name, Global_Sales), Global_Sales)) +
      ggplot2::geom_col(fill = "#47b8a5") + ggplot2::coord_flip() +
      ggplot2::labs(title = paste("Top", top_n, "Games by Global Sales"), x = NULL, y = "Global sales (millions)") + theme),
    platforms = plot_or_null(top_platform_plot, ggplot2::ggplot(top_platform_plot,
      ggplot2::aes(reorder(Platform, Total_Sales), Total_Sales)) +
      ggplot2::geom_col(fill = "#778df5") + ggplot2::coord_flip() +
      ggplot2::labs(title = paste("Top", top_n, "Platforms"), x = NULL, y = "Global sales (millions)") + theme),
    genres = plot_or_null(top_genre_plot, ggplot2::ggplot(top_genre_plot,
      ggplot2::aes(reorder(Genre, Total_Sales), Total_Sales)) +
      ggplot2::geom_col(fill = "#e4a752") + ggplot2::coord_flip() +
      ggplot2::labs(title = paste("Top", top_n, "Genres"), x = NULL, y = "Global sales (millions)") + theme),
    publishers = plot_or_null(top_publisher_plot, ggplot2::ggplot(top_publisher_plot,
      ggplot2::aes(reorder(Publisher, Total_Sales), Total_Sales)) +
      ggplot2::geom_col(fill = "#c47bea") + ggplot2::coord_flip() +
      ggplot2::labs(title = paste("Top", top_n, "Publishers"), x = NULL, y = "Global sales (millions)") + theme),
    regions = plot_or_null(result$regional, ggplot2::ggplot(result$regional,
      ggplot2::aes(Region, Sales, fill = Region)) + ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::labs(title = "Regional Sales Comparison", x = NULL, y = "Sales (millions)") + theme),
    years = plot_or_null(result$yearly, ggplot2::ggplot(result$yearly,
      ggplot2::aes(Year, Global_Sales)) + ggplot2::geom_line(colour = "#47b8a5", linewidth = 1) +
      ggplot2::geom_point(colour = "#47b8a5") + ggplot2::labs(title = "Yearly Global Sales", x = "Year", y = "Sales (millions)") + theme),
    releases = plot_or_null(result$yearly, ggplot2::ggplot(result$yearly,
      ggplot2::aes(Year, Game_Count)) + ggplot2::geom_line(colour = "#e4a752", linewidth = 1) +
      ggplot2::geom_point(colour = "#e4a752") + ggplot2::labs(title = "Number of Releases by Year", x = "Year", y = "Game count") + theme),
    scatter = if (length(regional_columns) && nrow(data) && any(!is.na(data[[regional_columns[1]]])) && any(!is.na(data$Global_Sales))) {
      ggplot2::ggplot(data, ggplot2::aes(.data[[regional_columns[1]]], Global_Sales)) +
        ggplot2::geom_point(colour = "#47b8a5", alpha = 0.55) +
        ggplot2::labs(title = paste(regional_names[[regional_columns[1]]], "vs Global Sales"),
          x = paste(regional_names[[regional_columns[1]]], "sales (millions)"), y = "Global sales (millions)") + theme
    } else NULL
  )
  plots
}

empty_table <- function(message = "No matching games in this category.") {
  data.frame(Message = message)
}
table_panel <- function(title_id, table_id) shiny::div(class = "panel",
  shiny::h3(shiny::textOutput(title_id, inline = TRUE)), DT::DTOutput(table_id))
plot_panel <- function(title, plot_id, height = "350px") shiny::div(class = "panel",
  shiny::h3(title), shiny::plotOutput(plot_id, height = height))

ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$title("Video Game Analytics Dashboard"),
    shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    shiny::tags$style(shiny::HTML("
      html { scroll-behavior:smooth; scroll-padding-top:76px; }
      body { background:#0e131a; color:#e7edf3; font-family:'Segoe UI',Arial,sans-serif; }
      .container-fluid { max-width:1500px; padding:20px 28px 50px; }
      .top-nav { position:sticky; top:0; z-index:1000; display:flex; gap:18px; flex-wrap:wrap;
        background:#121923; border:1px solid #273343; padding:12px 18px; border-radius:13px; margin-bottom:18px; }
      .top-nav a { color:#a9bacb; font-weight:650; text-decoration:none; }
      .top-nav a:hover { color:#57d4bd; }
      .hero { background:radial-gradient(circle at 85% 15%,#30476b,#1b2735 52%,#141c26);
        border:1px solid #344354; border-radius:22px; color:#f2f6fa; padding:32px; margin-bottom:20px;
        box-shadow:0 16px 40px #0005; }
      .hero h1 { margin:0 0 10px; font-size:clamp(28px,4vw,42px); font-weight:760; }
      .hero p { color:#b3c2d0; font-size:16px; margin:0; }
      .section-title { color:#f2f6fa; font-size:23px; font-weight:720; margin:30px 0 14px;
        border-left:4px solid #47b8a5; padding-left:12px; }
      .panel,.control-card,.upload-card,.step-card,.about-card { background:#171f29; border:1px solid #293645;
        border-radius:16px; padding:19px; margin-bottom:16px; box-shadow:0 9px 24px #0003; }
      .panel h3 { color:#dfe8f0; font-size:16px; font-weight:700; margin:0 0 15px; }
      .metric-card { background:#18232e; border:1px solid #304252; border-radius:15px; padding:17px;
        min-height:108px; margin-bottom:14px; }
      .metric-label,.insight-label,.muted { color:#91a1b1; font-size:11px; font-weight:700; letter-spacing:.07em;
        text-transform:uppercase; margin-bottom:9px; }
      .metric-value { color:#62d2bd; font-size:24px; font-weight:760; }
      .insight-card { background:#18232e; border:1px solid #304252; border-radius:14px; padding:16px;
        margin-bottom:14px; min-height:88px; }
      .insight-value { color:#e5edf4; font-weight:650; }
      .control-card { border-color:#417d80; background:linear-gradient(130deg,#17232d,#1b2c35); }
      .control-card label { color:#dce7ef; }
      .form-control,.selectize-input { background:#101720!important; border-color:#3a4a5b!important; color:#e6edf4!important; }
      .selectize-dropdown,.selectize-dropdown-content { background:#17212c!important; color:#e6edf4!important; }
      .selectize-dropdown .active { background:#2b4652!important; color:#fff!important; }
      .btn { border-radius:9px; font-weight:650; }
      .btn-primary { background:#287f79; border-color:#399b90; color:#fff; }
      .btn-primary:hover { background:#369e91; border-color:#5bc5b6; color:#fff; }
      .btn-default,.btn-secondary { background:#273442; border-color:#405164; color:#e3eaf0; }
      .btn-default:hover,.btn-secondary:hover { background:#344558; color:white; }
      .welcome,.empty-state { background:#17212b; border:1px solid #344557; border-radius:15px; padding:20px; color:#b6c6d5; }
      .alert { background:#33261b; border-color:#614927; color:#f2d6a9; }
      .dataTables_wrapper,.dataTables_info,.dataTables_length,.dataTables_filter,.dataTables_paginate { color:#bfccd7!important; }
      table.dataTable { background:#171f29!important; color:#e1e8ef!important; border-color:#344353!important; }
      table.dataTable thead th { background:#202c38!important; color:#f0f4f7!important; border-color:#39495a!important; }
      table.dataTable tbody tr,table.dataTable tbody td { background:#171f29!important; color:#dbe4eb!important; border-color:#2e3c4a!important; }
      table.dataTable tbody tr:hover td { background:#22313e!important; }
      .dataTables_wrapper input,.dataTables_wrapper select { background:#101720!important; color:#e6edf4!important; border:1px solid #3a4a5b!important; }
      .footer { border-top:1px solid #293645; margin-top:35px; padding:22px 4px; color:#8999a9; text-align:center; }
      .how-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(210px,1fr)); gap:14px; }
      @media(max-width:700px) { .container-fluid{padding:12px}.hero{padding:23px}.top-nav{gap:11px} }
    "))
  ),
  shiny::div(class = "top-nav",
    shiny::tags$a(href = "#dashboard", "Dashboard"),
    shiny::tags$a(href = "#how-it-works", "How It Works"),
    shiny::tags$a(href = "#category-analysis", "Category Analysis"),
    shiny::tags$a(href = "#charts", "Charts"),
    shiny::tags$a(href = "#tables", "Tables"),
    shiny::tags$a(href = "#about", "About")),
  shiny::div(id = "dashboard", class = "hero",
    shiny::h1(paste0("Video Game Analytics Dashboard")),
    shiny::p("Explore, filter and analyze video game sales data using interactive visual analytics.")),
  shiny::div(class = "upload-card",
    shiny::h2(class = "section-title", "Upload Dataset"),
    shiny::fileInput("file", "Choose a video game sales CSV", accept = c(".csv", "text/csv")),
    shiny::uiOutput("upload_status"), shiny::uiOutput("upload_meta"),
    shiny::actionButton("analyze", "Analyze Dataset", class = "btn-primary"),
    shiny::uiOutput("error_message"),
    shiny::tags$div(style = "display:none", shiny::textOutput("is_ready"), shiny::textOutput("has_games"))),
  shiny::conditionalPanel("input.file && output.is_ready != '1'",
    shiny::div(class = "panel", shiny::h3("Dataset Preview"), DT::DTOutput("upload_preview"))),
  shiny::div(id = "how-it-works", class = "section-title", "How It Works"),
  shiny::div(class = "how-grid",
    shiny::div(class = "step-card", shiny::div(class = "metric-label", "01 / UPLOAD"),
      shiny::p("Upload a CSV containing video game sales data.")),
    shiny::div(class = "step-card", shiny::div(class = "metric-label", "02 / CHOOSE"),
      shiny::p("Select a category and the number of results to explore.")),
    shiny::div(class = "step-card", shiny::div(class = "metric-label", "03 / ANALYZE"),
      shiny::p("The dashboard cleans and filters your data, then recalculates the results.")),
    shiny::div(class = "step-card", shiny::div(class = "metric-label", "04 / EXPLORE"),
      shiny::p("Compare insights, charts, tables, regions and years."))),
  shiny::div(id = "category-analysis", class = "section-title", "Analysis Controls"),
  shiny::div(class = "control-card",
    shiny::fluidRow(
      shiny::column(5, shiny::selectInput("category", "Explore by Category",
        choices = stats::setNames(names(category_labels), category_labels), selected = "all")),
      shiny::column(4, shiny::selectInput("topn_mode", "Number of Results",
        choices = c("5" = "5", "10" = "10", "15" = "15", "20" = "20",
                    "25" = "25", "50" = "50", "100" = "100", "Custom" = "custom"),
        selected = "10")),
      shiny::column(3, shiny::conditionalPanel("input.topn_mode === 'custom'",
        shiny::numericInput("custom_top_n", "Enter number of results", value = 10, min = 1, max = 1000)))),
    shiny::uiOutput("filter_status"),
    shiny::actionButton("reset", "Reset Filters", class = "btn-secondary")),
  shiny::uiOutput("welcome"),
  shiny::uiOutput("no_games"),
  shiny::conditionalPanel("output.is_ready == '1' && output.has_games == '1'",
    shiny::div(class = "section-title", "Dataset Overview"), shiny::uiOutput("summary_cards"),
    shiny::div(class = "section-title", "Key Insights"), shiny::uiOutput("insight_cards"),
    shiny::div(id = "charts", class = "section-title", "Charts"),
    shiny::fluidRow(shiny::column(6, plot_panel("top_games_title", "plot_games")),
      shiny::column(6, plot_panel("top_platforms_title", "plot_platforms")),
      shiny::column(6, plot_panel("top_genres_title", "plot_genres")),
      shiny::column(6, plot_panel("top_publishers_title", "plot_publishers")),
      shiny::column(6, plot_panel("Regional Sales Comparison", "plot_regions")),
      shiny::column(6, plot_panel("Yearly Global Sales", "plot_years")),
      shiny::column(6, plot_panel("Number of Releases by Year", "plot_releases")),
      shiny::column(6, plot_panel("Regional vs Global Sales", "plot_scatter"))),
    shiny::div(id = "tables", class = "section-title", "Analysis Tables"),
    shiny::fluidRow(shiny::column(12, table_panel("top_games_table_title", "table_games")),
      shiny::column(12, table_panel("top_regional_games_title", "table_regional_games")),
      shiny::column(6, table_panel("top_platforms_table_title", "table_platforms")),
      shiny::column(6, table_panel("top_genres_table_title", "table_genres")),
      shiny::column(6, table_panel("top_publishers_table_title", "table_publishers")),
      shiny::column(6, table_panel("regional_table_title", "table_regions")),
      shiny::column(12, table_panel("year_table_title", "table_years"))),
    shiny::div(class = "panel", shiny::h3("Dataset Preview"), DT::DTOutput("filtered_preview")),
    shiny::div(class = "panel", shiny::h3("Downloads"),
      shiny::downloadButton("download_filtered", "Download Filtered Dataset"),
      shiny::downloadButton("download_analysis", "Download Analysis Results"))),
  shiny::div(id = "about", class = "section-title", "About Category Analysis"),
  shiny::div(class = "about-card",
    shiny::p("This dashboard lets you explore types of video games in a larger sales dataset. Select a category to compare sales, platforms, publishers, regions and yearly performance."),
    shiny::tags$ul(lapply(names(category_about), function(key)
      shiny::tags$li(shiny::tags$strong(paste0(category_labels[[key]], ": ")), category_about[[key]])))),
  shiny::div(class = "footer",
    shiny::div("Video Game Analytics Dashboard"),
    shiny::div("Built with R, Shiny, dplyr and ggplot2"),
    shiny::div("Interactive data analysis for educational and analytical purposes."))
)

server <- function(input, output, session) {
  raw_upload <- shiny::reactive({
    file <- input$file
    shiny::req(file)
    tryCatch({
      data <- readr::read_csv(file$datapath, show_col_types = FALSE, progress = FALSE)
      keys <- tolower(gsub("[^[:alnum:]]", "", names(data)))
      has_sales <- any(keys %in% c("globalsales", "sales", "totalsales", "worldwidesales",
        "worldsales", "unitssold", "nasales", "eusales", "jpsales", "othersales"))
      list(data = data, file = file$name, has_sales = has_sales, error = NULL)
    }, error = function(e) list(data = NULL, file = file$name, has_sales = FALSE, error = conditionMessage(e)))
  })
  prepared <- shiny::reactiveVal(NULL)
  failure <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$file, {
    prepared(NULL); failure(NULL)
    shiny::updateSelectInput(session, "category", selected = "all")
    shiny::updateSelectInput(session, "topn_mode", selected = "10")
    shiny::updateNumericInput(session, "custom_top_n", value = 10)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$reset, {
    shiny::updateSelectInput(session, "category", selected = "all")
    shiny::updateSelectInput(session, "topn_mode", selected = "10")
    shiny::updateNumericInput(session, "custom_top_n", value = 10)
  })
  shiny::observeEvent(input$analyze, {
    upload <- raw_upload()
    if (!is.null(upload$error)) { failure(paste("Could not read CSV:", upload$error)); return() }
    if (!upload$has_sales) {
      failure("No usable sales column found. Include Global_Sales, Sales, or at least one regional sales field.")
      return()
    }
    tryCatch({ prepared(clean_uploaded_data(upload$data)); failure(NULL) },
      error = function(e) { prepared(NULL); failure(conditionMessage(e)) })
  }, ignoreInit = TRUE)

  selected_category <- shiny::reactive({ value <- input$category; if (is.null(value) || !value %in% names(category_labels)) "all" else value })
  category_name <- shiny::reactive(category_labels[[selected_category()]])
  top_n <- shiny::reactive({
    value <- if (identical(input$topn_mode, "custom")) input$custom_top_n else input$topn_mode
    value <- suppressWarnings(as.integer(value))
    if (length(value) != 1L || is.na(value)) value <- 10L
    max(1L, min(1000L, value))
  })
  filtered_data <- shiny::reactive({
    p <- prepared()
    if (is.null(p)) return(NULL)
    filter_by_category(p$data, selected_category())
  })
  current_analysis <- shiny::reactive({
    p <- prepared(); d <- filtered_data()
    if (is.null(p) || is.null(d) || !nrow(d)) return(NULL)
    n <- top_n()
    summary <- summarize_filtered(d, n, p$regional_columns)
    list(data = d, summary = summary, plots = build_plots(d, summary, n, p$regional_columns),
         top_n = n, prepared = p)
  })
  shiny::observeEvent(input$file, {
    if (!is.null(input$file)) failure(NULL)
  }, ignoreInit = TRUE)

  output$is_ready <- shiny::renderText(if (!is.null(prepared())) "1" else "0")
  output$has_games <- shiny::renderText(if (!is.null(filtered_data()) && nrow(filtered_data())) "1" else "0")
  shiny::outputOptions(output, "is_ready", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "has_games", suspendWhenHidden = FALSE)
  output$upload_status <- shiny::renderUI({
    if (is.null(input$file)) return(NULL)
    upload <- raw_upload()
    if (!is.null(upload$error)) return(shiny::div(class = "alert alert-danger", upload$error))
    if (!upload$has_sales) return(shiny::div(class = "alert alert-danger",
      "No usable sales field found. Other standard columns are optional."))
    shiny::div(class = "alert alert-success", paste("Dataset ready:", upload$file))
  })
  output$upload_meta <- shiny::renderUI({
    if (is.null(input$file)) return(NULL)
    upload <- raw_upload()
    if (!is.null(upload$error) || !upload$has_sales) return(NULL)
    shiny::p(class = "muted", paste(nrow(upload$data), "original rows |", ncol(upload$data),
      "columns. Only a sales field is required; other standard fields are optional."))
  })
  output$error_message <- shiny::renderUI(if (!is.null(failure()))
    shiny::div(class = "alert alert-danger", failure()))
  output$welcome <- shiny::renderUI(if (is.null(input$file)) shiny::div(class = "welcome",
    shiny::h3("Welcome to Video Game Analytics"),
    shiny::p("Upload a sales CSV, then click Analyze Dataset to begin.")) else NULL)
  output$upload_preview <- DT::renderDT({
    upload <- raw_upload()
    shiny::req(!is.null(upload$data))
    DT::datatable(utils::head(upload$data, 10), rownames = FALSE,
      options = list(pageLength = 5, scrollX = TRUE))
  })
  output$filter_status <- shiny::renderUI({
    p <- prepared()
    count <- if (is.null(p)) 0L else nrow(filtered_data())
    shiny::tagList(
      shiny::p(shiny::tags$strong("Current Filter: "), category_name(),
        shiny::span(class = "muted", paste("| Showing", count, "games"))),
      if (!is.null(p)) shiny::p(class = "muted", paste("Top N:", top_n(),
        "| Original rows:", p$rows_before, "| Cleaned rows:", p$rows_after,
        "| Duplicate rows removed:", p$duplicates_removed)) else NULL
    )
  })
  output$no_games <- shiny::renderUI({
    p <- prepared()
    if (is.null(p) || nrow(filtered_data())) return(NULL)
    shiny::div(class = "empty-state",
      paste("No games found for this category in the uploaded dataset. Showing 0", category_name(), "games."))
  })

  output$summary_cards <- shiny::renderUI({
    p <- prepared(); d <- filtered_data(); a <- current_analysis()
    shiny::req(!is.null(p), !is.null(d), nrow(d))
    summary <- if (is.null(a)) NULL else a$summary
    global_sales <- sum(d$Global_Sales, na.rm = TRUE)
    years <- d$Year[!is.na(d$Year)]
    items <- list(
      "Total Games" = format(nrow(d), big.mark = ","),
      "Total Global Sales" = paste0(format(round(global_sales, 2), big.mark = ",", nsmall = 2), " M"),
      "Platforms" = as.character(dplyr::n_distinct(d$Platform[d$Platform != "Unknown"])),
      "Genres" = as.character(dplyr::n_distinct(d$Genre[d$Genre != "Unknown"])),
      "Publishers" = as.character(dplyr::n_distinct(d$Publisher[d$Publisher != "Unknown"])),
      "Year Range" = if (length(years)) paste(min(years), "-", max(years)) else "Unavailable"
    )
    shiny::tagList(shiny::fluidRow(lapply(names(items), function(label)
      shiny::column(4, shiny::div(class = "metric-card",
        shiny::div(class = "metric-label", label), shiny::div(class = "metric-value", items[[label]]))))),
      shiny::p(class = "muted", paste("Missing Genre/Publisher values labeled Unknown:",
        p$category_values_filled, "| optional columns absent:", if (length(p$optional_missing))
          paste(p$optional_missing, collapse = ", ") else "none")))
  })
  output$insight_cards <- shiny::renderUI({
    a <- current_analysis(); shiny::req(!is.null(a))
    insights <- a$summary$insights
    labels <- c("Best-Selling Game" = "game", "Best Platform" = "platform",
      "Best Genre" = "genre", "Top Publisher" = "publisher",
      "Highest Average Platform (20+ games)" = "avg_platform",
      "Highest-Sales Year" = "year", "Highest-Sales Region" = "region")
    shiny::fluidRow(lapply(names(labels), function(label)
      shiny::column(4, shiny::div(class = "insight-card",
        shiny::div(class = "insight-label", label),
        shiny::div(class = "insight-value", insights[[labels[[label]]]])))))
  })

  table_value <- function(data, message = "No matching data for this analysis.") {
    if (is.null(data) || !nrow(data)) return(empty_table(message))
    data
  }
  dt_render <- function(id, getter) output[[id]] <- DT::renderDT({
    a <- current_analysis(); shiny::req(!is.null(a))
    DT::datatable(table_value(getter(a)), rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE))
  })
  title_render <- function(id, text) output[[id]] <- shiny::renderText({
    shiny::req(prepared()); paste("Top", top_n(), text)
  })
  title_render("top_games_title", "Games by Global Sales")
  title_render("top_platforms_title", "Platforms by Global Sales")
  title_render("top_genres_title", "Genres by Global Sales")
  title_render("top_publishers_title", "Publishers by Global Sales")
  title_render("top_games_table_title", "Games by Global Sales")
  title_render("top_regional_games_title", "Games by Regional Sales")
  title_render("top_platforms_table_title", "Platforms by Global Sales")
  title_render("top_genres_table_title", "Genres by Global Sales")
  title_render("top_publishers_table_title", "Publishers by Global Sales")
  output$regional_table_title <- shiny::renderText("Regional Sales")
  output$year_table_title <- shiny::renderText("Year Analysis")
  dt_render("table_games", function(a) a$summary$top_games)
  dt_render("table_regional_games", function(a) a$summary$regional_games)
  dt_render("table_platforms", function(a) a$summary$platforms)
  dt_render("table_genres", function(a) a$summary$genres)
  dt_render("table_publishers", function(a) a$summary$publishers)
  dt_render("table_regions", function(a) a$summary$regional)
  dt_render("table_years", function(a) a$summary$yearly)

  plot_render <- function(id, key) output[[id]] <- shiny::renderPlot({
    a <- current_analysis(); shiny::req(!is.null(a))
    plot <- a$plots[[key]]
    shiny::validate(shiny::need(!is.null(plot), "Not enough matching data to draw this chart."))
    plot
  }, res = 96)
  plot_render("plot_games", "games")
  plot_render("plot_platforms", "platforms")
  plot_render("plot_genres", "genres")
  plot_render("plot_publishers", "publishers")
  plot_render("plot_regions", "regions")
  plot_render("plot_years", "years")
  plot_render("plot_releases", "releases")
  plot_render("plot_scatter", "scatter")
  output$filtered_preview <- DT::renderDT({
    a <- current_analysis(); shiny::req(!is.null(a))
    DT::datatable(utils::head(a$data, 20), rownames = FALSE, filter = "top",
      options = list(pageLength = 10, scrollX = TRUE, searchHighlight = TRUE))
  })

  category_slug <- shiny::reactive(switch(selected_category(), pokemon = "pokemon", cricket = "cricket", football = "football_soccer", racing = "racing", action_adventure = "action_adventure", "all_games"))
  output$download_filtered <- shiny::downloadHandler(
    filename = function() paste0(category_slug(), "_filtered.csv"),
    content = function(file) readr::write_csv(filtered_data(), file, na = ""))
  output$download_analysis <- shiny::downloadHandler(
    filename = function() paste0(category_slug(),
      "_top_", top_n(), "_analysis.csv"),
    content = function(file) {
      a <- current_analysis(); shiny::req(!is.null(a)); t <- a$summary
      games <- t$top_games |>
        dplyr::transmute(Analysis = "Games", Rank = dplyr::row_number(), Item = Name,
          Game_Count = NA_integer_, Total_Sales = Global_Sales, Average_Sales = NA_real_)
      groups <- dplyr::bind_rows(
        t$platform |> dplyr::transmute(Analysis = "Platforms", Rank = dplyr::row_number(), Item = Platform,
          Game_Count, Total_Sales, Average_Sales),
        t$genres |> dplyr::transmute(Analysis = "Genres", Rank = dplyr::row_number(), Item = Genre,
          Game_Count, Total_Sales, Average_Sales),
        t$publishers |> dplyr::transmute(Analysis = "Publishers", Rank = dplyr::row_number(), Item = Publisher,
          Game_Count, Total_Sales, Average_Sales)
      )
      readr::write_csv(dplyr::bind_rows(games, groups), file, na = "")
    })
}

shiny::shinyApp(ui, server)
