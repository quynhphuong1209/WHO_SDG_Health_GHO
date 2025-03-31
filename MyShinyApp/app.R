library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(shinycssloaders)

# 1. KIỂM TRA VÀ CHUẨN BỊ DỮ LIỆU -----

if (!dir.exists("who_data")) dir.create("who_data")

data_path <- "who_data/who_health_data.rds"

# ✅ Hàm tạo dữ liệu mẫu với khu vực cố định
create_sample_data <- function(country_values, years) {
  # Ánh xạ quốc gia - khu vực cố định
  region_mapping <- c(
    "Vietnam" = "Asia",
    "USA" = "Americas",
    "Japan" = "Asia",
    "China" = "Asia",
    "India" = "Asia",
    "Germany" = "Europe",
    "South Africa" = "Africa"
  )
  
  # Tạo dataframe
  data.frame(
    country = rep(names(country_values), each = length(years)),
    year = rep(years, length(country_values)),
    value = unlist(lapply(country_values, function(x) {
      base_trend <- seq(x[1], x[2], length.out = length(years))
      decade_effect <- sin(seq(0, 2*pi, length.out = length(years))) * (x[1]-x[2])/4
      random_noise <- rnorm(length(years), sd = (x[1]-x[2])/10)
      final_values <- base_trend + decade_effect + random_noise
      round(pmax(final_values, x[2]*0.5))
    })),
    region = rep(region_mapping[names(country_values)], each = length(years)), # Fixed region mapping
    stringsAsFactors = FALSE
  )
}

# Xóa dữ liệu cũ nếu tồn tại
if (file.exists(data_path)) {
  file.remove(data_path)
  message("Đã xóa file dữ liệu cũ để tạo mới")
}

# Tạo dữ liệu mới
if (!file.exists(data_path)) {
  years <- 1932:2022
  
  who_data <- list(
    Maternal_mortality = create_sample_data(
      list(
        Vietnam = c(1200, 40),
        USA = c(600, 16),
        Japan = c(800, 3),
        China = c(1500, 20),
        India = c(2000, 110),
        Germany = c(700, 4),
        `South Africa` = c(1500, 220)
      ), years) %>% 
      mutate(indicator = "MDG_0000000026",
             value = ifelse(value < 0, 0, value)),
    
    Under5_mortality = create_sample_data(
      list(
        Vietnam = c(400, 18),
        USA = c(150, 4),
        Japan = c(200, 2),
        China = c(500, 11),
        India = c(600, 35),
        Germany = c(250, 3),
        `South Africa` = c(450, 60)
      ), years) %>% 
      mutate(indicator = "MDG_0000000007",
             value = ifelse(value < 0, 0, value)),
    
    Infectious_disease = create_sample_data(
      list(
        Vietnam = c(800, 100),
        USA = c(500, 65),
        Japan = c(600, 50),
        China = c(1000, 70),
        India = c(1200, 130),
        Germany = c(400, 55),
        `South Africa` = c(900, 160)
      ), years) %>% 
      mutate(indicator = "MDG_0000000017",
             value = ifelse(value < 0, 0, value)),
    
    Health_service = create_sample_data(
      list(
        Vietnam = c(10, 85),
        USA = c(50, 97),
        Japan = c(40, 94),
        China = c(15, 86),
        India = c(5, 75),
        Germany = c(60, 92),
        `South Africa` = c(20, 70)
      ), years) %>% 
      mutate(indicator = "UHC_INDEX_REPORTED",
             value = ifelse(value < 0, 0, value),
             value = ifelse(value > 100, 100, value)) %>% 
      mutate(value = case_when(
        year %in% 1939:1945 & country %in% c("Vietnam", "Japan", "Germany") ~ value * 0.6,
        year %in% 1955:1975 & country == "Vietnam" ~ value * 0.7,
        TRUE ~ value
      ))
  )
  
  saveRDS(who_data, data_path)
  message("✅ Đã tạo dữ liệu mới với khu vực chính xác")
}

# Đọc dữ liệu
who_data <- readRDS(data_path)

# Ánh xạ tên chỉ số
indicator_names <- c(
  "Maternal_mortality" = "Tỷ lệ tử vong mẹ",
  "Under5_mortality" = "Tỷ lệ tử vong trẻ em dưới 5 tuổi",
  "Infectious_disease" = "Tỷ lệ mắc bệnh truyền nhiễm",
  "Health_service" = "Tiếp cận dịch vụ y tế"
)

# Kết hợp dữ liệu và lọc khu vực không hợp lệ
combined_data <- bind_rows(lapply(names(who_data), function(x) {
  who_data[[x]] %>% 
    mutate(Indicator = indicator_names[x])
})) %>% 
  mutate(
    year = as.integer(year),
    value = as.numeric(value),
    country_region = paste(country, " (", region, ")", sep = "")
  ) %>% 
  filter(!is.na(value))


# UI ----
ui <- dashboardPage(
  dashboardHeader(title = "WHO Health Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tổng quan", tabName = "overview", icon = icon("dashboard")),
      menuItem("Xu hướng", tabName = "trends", icon = icon("chart-line")),
      menuItem("So sánh khu vực", tabName = "regional", icon = icon("globe-americas")),
      menuItem("Phân tích tương quan", tabName = "correlation", icon = icon("project-diagram")),
      menuItem("Dữ liệu", tabName = "rawdata", icon = icon("table"))
    ),
    selectInput("indicator", "Chọn chỉ số:", 
                choices = indicator_names,
                selected = indicator_names[1]),
    sliderInput("year_range", "Chọn năm:", 
                min = 1932, 
                max = 2022,
                value = c(1932, 2022),
                step = 1,
                sep = "",
                ticks = TRUE),
    selectizeInput("country", "Chọn quốc gia/khu vực:", 
                   choices = unique(combined_data$country_region), 
                   multiple = TRUE,
                   selected = c("Vietnam (Asia)", "USA (Americas)", "Japan (Asia)"),
                   options = list(placeholder = 'Chọn quốc gia...'))
  ),
  dashboardBody(
    tags$head(
      
      tags$style(HTML("
        /* CSS tổng thể */
        body {
          font-family: Arial, sans-serif;
          font-size: 16px;
        }
        
        /* Selectize input */
        .selectize-input {
          min-height: 40px;
          line-height: 1.5 !important;
          padding: 8px 12px !important;
          border: 1px solid #ddd !important;
          border-radius: 4px !important;
        }
        
        /* Slider */
        .irs--shiny .irs-bar, .irs--shiny .irs-single {
          background: #3c8dbc;
        }
        
        /* Box */
        .box {
          border-radius: 5px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("indicator_box", width = 3),
          valueBoxOutput("country_box", width = 3),
          valueBoxOutput("year_box", width = 3),
          valueBoxOutput("median_box", width = 3)
        ),
        fluidRow(
          box(
            width = 12, 
            title = "So sánh 4 chỉ số sức khỏe giữa các quốc gia",
            status = "primary",
            solidHeader = TRUE,
            withSpinner(plotlyOutput("comparison_plot", height = "600px"), type = 6, color = "#3c8dbc")
          )
        )
      ),
      
      tabItem(
        tabName = "trends",
        fluidRow(
          box(
            width = 12, 
            title = "Xu hướng theo thời gian",
            status = "primary",
            solidHeader = TRUE,
            withSpinner(plotlyOutput("trend_plot"), type = 6, color = "#3c8dbc")
          )
        )
      ),
      
      tabItem(
        tabName = "regional",
        fluidRow(
          box(
            width = 12,
            title = "So sánh giữa các khu vực",
            status = "primary",
            solidHeader = TRUE,
            withSpinner(plotlyOutput("regional_plot"), type = 6, color = "#3c8dbc")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Thống kê theo khu vực",
            status = "info",
            solidHeader = TRUE,
            withSpinner(DTOutput("regional_table"), type = 6, color = "#3c8dbc")
          )
        )
      ),
      
      tabItem(
        tabName = "correlation",
        fluidRow(
          box(
            width = 12,
            title = "Ma trận tương quan giữa các chỉ số",
            status = "primary",
            solidHeader = TRUE,
            withSpinner(plotlyOutput("correlation_plot"), type = 6, color = "#3c8dbc")
          )
        )
      ),
      
      tabItem(
        tabName = "rawdata",
        fluidRow(
          box(
            width = 12,
            title = "Dữ liệu thô",
            status = "primary",
            solidHeader = TRUE,
            withSpinner(DTOutput("rawdata_table"), type = 6, color = "#3c8dbc")
          )
        )
      )
    )
  )
)

# SERVER ----
server <- function(input, output, session) {
  # Dữ liệu được lọc
  filtered_data <- reactive({
    req(input$indicator, input$year_range)
    
    data <- combined_data %>% 
      filter(
        Indicator == input$indicator,
        between(year, input$year_range[1], input$year_range[2])
      )
    
    if (!is.null(input$country)) {
      selected_countries <- str_remove(input$country, " \\(.*\\)$")
      data <- data %>% filter(country %in% selected_countries)
    }
    
    data %>% mutate(year = as.integer(year))
  })
  
  # Dữ liệu cho biểu đồ so sánh 4 chỉ số
  comparison_data <- reactive({
    req(input$country, input$year_range)
    
    combined_data %>% 
      filter(
        between(year, input$year_range[1], input$year_range[2]),
        country %in% str_remove(input$country, " \\(.*\\)$")
      ) %>% 
      group_by(country, Indicator) %>% 
      summarise(value = mean(value, na.rm = TRUE), .groups = 'drop') %>% 
      pivot_wider(
        names_from = Indicator, 
        values_from = value,
        names_sort = TRUE  # Đảm bảo thứ tự tên ổn định
      ) %>% 
      # Đổi tên cột sang dạng "safe" nếu cần
      set_names(make.names(names(.)))
  })
  # Dữ liệu cho phân tích tương quan
  correlation_data <- reactive({
    req(input$year_range)
    
    df <- combined_data %>%
      filter(between(year, input$year_range[1], input$year_range[2])) %>%
      select(country, year, Indicator, value) %>%
      pivot_wider(names_from = Indicator, values_from = value) %>%
      select(-country, -year)
    
    # Chỉ giữ lại các cột numeric có ít nhất 3 giá trị không NA
    df <- df %>%
      select(where(is.numeric)) %>%
      select(where(~sum(!is.na(.x)) >= 3))
    
    if (ncol(df) < 2) return(NULL)
    df
  })
  
  # Cập nhật các valueBox
  output$indicator_box <- renderValueBox({
    valueBox(
      value = tags$p(input$indicator, style = "font-size: 24px;"),
      subtitle = "Chỉ số được chọn",
      icon = icon("heartbeat"),
      color = "red"
    )
  })
  
  output$country_box <- renderValueBox({
    n_countries <- length(unique(filtered_data()$country))
    valueBox(
      value = tags$p(n_countries, style = "font-size: 28px;"),
      subtitle = "Số quốc gia",
      icon = icon("flag"),
      color = "green"
    )
  })
  
  output$year_box <- renderValueBox({
    year_range <- paste(input$year_range[1], "-", input$year_range[2])
    valueBox(
      value = tags$p(year_range, style = "font-size: 28px;"),
      subtitle = "Khoảng năm",
      icon = icon("calendar"),
      color = "blue"
    )
  })
  
  output$median_box <- renderValueBox({
    median_val <- median(filtered_data()$value, na.rm = TRUE)
    valueBox(
      value = tags$p(round(median_val, 2), style = "font-size: 28px;"),
      subtitle = "Giá trị trung vị",
      icon = icon("calculator"),
      color = "purple"
    )
  })
  
  # Biểu đồ so sánh 4 chỉ số
  output$comparison_plot <- renderPlotly({
    df <- comparison_data()
    
    validate(need(nrow(df) > 0, "Không có dữ liệu phù hợp"))
    
    # Tạo mapping giữa tên safe và tên gốc
    name_mapping <- c(
      "Tiếp.cận.dịch.vụ.y.tế" = "Tiếp cận DV y tế (%)",
      "Tỷ.lệ.mắc.bệnh.truyền.nhiễm" = "Bệnh truyền nhiễm (100,000 dân)",
      "Tỷ.lệ.tử.vong.mẹ" = "Tử vong mẹ (100,000 ca sinh)",
      "Tỷ.lệ.tử.vong.trẻ.em.dưới.5.tuổi" = "Tử vong trẻ em (1,000 ca sinh)"
    )
    
    p <- plot_ly(df, x = ~country)
    
    # Tự động thêm trace cho mỗi chỉ số
    for (col in names(df)[-1]) {
      safe_name <- col
      display_name <- name_mapping[safe_name]
      
      p <- p %>% add_trace(
        y = as.formula(paste0("~`", col, "`")), 
        name = display_name,
        type = 'bar'
      )
    }
    
    p %>% layout(
      title = "So sánh các chỉ số sức khỏe",
      xaxis = list(title = "Quốc gia"),
      yaxis = list(title = "Giá trị trung bình"),
      barmode = 'group'
    )
  })
  
  # Biểu đồ xu hướng
  output$trend_plot <- renderPlotly({
    df <- filtered_data()
    
    validate(
      need(nrow(df) > 0, "Không có dữ liệu phù hợp với lựa chọn hiện tại")
    )
    
    plot_ly(df, x = ~year, y = ~value, color = ~country,
            type = 'scatter', mode = 'lines+markers',
            line = list(width = 2),
            marker = list(size = 8)) %>%
      layout(
        xaxis = list(
          title = "Năm",
          type = 'category',
          tickvals = unique(df$year)),
        yaxis = list(title = "Giá trị")
      )
  })
  
  # Biểu đồ so sánh khu vực
  output$regional_plot <- renderPlotly({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "Không có dữ liệu phù hợp"))
    plot_ly(df, x = ~region, y = ~value, color = ~region, type = 'box') %>%
      layout(xaxis = list(title = "Khu vực"), yaxis = list(title = "Giá trị"))
  })
  
  # Bảng thống kê khu vực
  output$regional_table <- renderDT({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "Không có dữ liệu"))
    
    df_summary <- df %>% 
      group_by(region) %>% 
      summarise(
        `Số quốc gia` = n_distinct(country),
        `Trung bình` = round(mean(value, na.rm = TRUE), 2),
        `Trung vị` = round(median(value, na.rm = TRUE), 2),
        `Tối thiểu` = round(min(value, na.rm = TRUE), 2),
        `Tối đa` = round(max(value, na.rm = TRUE), 2),
        `Độ lệch chuẩn` = round(sd(value, na.rm = TRUE), 2))
    
    datatable(df_summary, options = list(scrollX = TRUE))
  })
  
  # Biểu đồ tương quan
  output$correlation_plot <- renderPlotly({
    df <- correlation_data()
    if (is.null(df)) return(plotly_empty() %>% layout(title = "Không đủ dữ liệu"))
    
    cor_matrix <- cor(df, use = "complete.obs")
    plot_ly(x = colnames(cor_matrix), y = colnames(cor_matrix), z = cor_matrix,
            type = "heatmap", colorscale = "RdBu") %>%
      layout(title = "Ma trận tương quan giữa các chỉ số")
  })
  
  # Bảng dữ liệu thô
  output$rawdata_table <- renderDT({
    datatable(combined_data %>% select(-country_region), 
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                scrollX = TRUE))
  })
}

# Chạy ứng dụng
shinyApp(ui = ui, server = server)