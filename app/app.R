#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(shinycssloaders)

# 1. KIỂM TRA VÀ CHUẨN BỊ DỮ LIỆU -----
# Tạo thư mục nếu chưa có
if (!dir.exists("who_data")) dir.create("who_data")

# Đường dẫn đến file dữ liệu
data_path <- "who_data/who_final_data.rds"

# 2. TẠO DỮ LIỆU MẪU NẾU FILE KHÔNG TỒN TẠI -----
if (!file.exists(data_path)) {
  warning("⚠️ Không tìm thấy file dữ liệu. Đang tạo dữ liệu mẫu...")
  
  # Tạo dữ liệu mẫu
  who_data <- list(
    Maternal_mortality = data.frame(
      country = rep(c("Vietnam", "USA", "Japan"), each = 5),
      year = rep(2015:2019, 3),
      value = c(50, 48, 45, 43, 40,  # Vietnam
                20, 19, 18, 17, 16,  # USA
                5, 5, 4, 4, 3),      # Japan
      region = rep(c("Asia", "Americas", "Asia"), each = 5),
      indicator = "MMR"
    )
  )
  
  # Kiểm tra thư mục lưu file
  dir_path <- dirname(data_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)  # Tạo thư mục nếu chưa có
  }
  
  # Lưu dữ liệu mẫu vào file
  tryCatch({
    saveRDS(who_data, data_path)
    message("✅ Đã tạo file dữ liệu mẫu tại: ", data_path)
  }, error = function(e) {
    stop("❌ Lỗi khi lưu dữ liệu mẫu: ", e$message)
  })
  
} else {
  # Tải dữ liệu từ file
  tryCatch({
    who_data <- readRDS(data_path)
    message("✅ Đã tải file dữ liệu từ: ", data_path)
  }, error = function(e) {
    stop("❌ Lỗi khi đọc file dữ liệu: ", e$message)
  })
}


# 3. KIỂM TRA DỮ LIỆU -----
# In ra thông tin để debug
message("\nTHÔNG TIN DỮ LIỆU:")
message("Số lượng chỉ số: ", length(who_data))
message("Tên các chỉ số: ", paste(names(who_data), collapse = ", "))
message("Cấu trúc dữ liệu:")
str(who_data, max.level = 1)

# 4. CHUẨN BỊ DỮ LIỆU CHO SHINY -----
combined_data <- tryCatch({
  bind_rows(who_data, .id = "Indicator") %>% 
    mutate(
      Indicator = str_replace_all(Indicator, "_", " "),
      year = as.integer(year),
      value = as.numeric(value)
    ) %>% 
    filter(!is.na(value))
}, error = function(e) {
  message("Lỗi khi chuẩn bị dữ liệu: ", e$message)
  NULL
})

# Kiểm tra dữ liệu cuối cùng
if (is.null(combined_data)) {
  stop("KHÔNG THỂ KHỞI TẠO DỮ LIỆU. Vui lòng kiểm tra file who_final_data.rds")
}

# 3. Giao diện người dùng
ui <- dashboardPage(
  dashboardHeader(title = "WHO Health Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tổng quan", tabName = "overview", icon = icon("dashboard")),
      menuItem("Xu hướng", tabName = "trends", icon = icon("chart-line")),
      menuItem("Khu vực", tabName = "regional", icon = icon("map")),
      menuItem("Dữ liệu", tabName = "rawdata", icon = icon("table"))
    ),
    
    selectInput(
      "indicator", 
      "Chọn chỉ số:", 
      choices = unique(combined_data$Indicator),
      selected = unique(combined_data$Indicator)[1]
    ),
    
    sliderInput(
      "year_range", 
      "Chọn năm:",
      min = min(combined_data$year, na.rm = TRUE),
      max = max(combined_data$year, na.rm = TRUE),
      value = range(combined_data$year, na.rm = TRUE),
      step = 1
    ),
    
    actionButton("update", "Cập nhật", icon = icon("refresh"))
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .shiny-output-error { color: red; }
        .box-body { overflow-x: auto; }
        .selectize-input { font-size: 14px; }
      "))
    ),
    
    tabItems(
      # Tab tổng quan
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("indicator_box", width = 4),
          valueBoxOutput("country_box", width = 4),
          valueBoxOutput("year_box", width = 4)
        ),
        fluidRow(
          box(
            width = 12,
            withSpinner(plotlyOutput("overview_plot"))
          )
        )
      ),
      
      # Tab xu hướng
      tabItem(
        tabName = "trends",
        fluidRow(
          box(
            width = 12,
            withSpinner(plotlyOutput("trend_plot"))
          )
        ),
        fluidRow(
          box(
            width = 12,
            selectizeInput(
              "country", 
              "Chọn quốc gia:", 
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = 'Chọn ít nhất 1 quốc gia')
            )
          )
        )
      ),
      
      # Tab khu vực
      tabItem(
        tabName = "regional",
        fluidRow(
          box(
            width = 12,
            withSpinner(plotlyOutput("regional_plot"))
          )
        ),
        fluidRow(
          box(
            width = 12,
            withSpinner(DTOutput("regional_table"))
          )
        )
      ),
      
      # Tab dữ liệu
      tabItem(
        tabName = "rawdata",
        fluidRow(
          box(
            width = 12,
            withSpinner(DTOutput("rawdata_table"))
          )
        )
      )
    )
  )
)

# 4. Server logic
server <- function(input, output, session) {
  
  # In thông tin dữ liệu một lần duy nhất
  isolate({
    message("\n==== THÔNG TIN DỮ LIỆU ====")
    message("Chỉ số có sẵn: ", paste(unique(combined_data$Indicator), collapse = ", "))
    message("Số quốc gia: ", length(unique(combined_data$country)))
    message("Khoảng năm: ", min(combined_data$year), " đến ", max(combined_data$year))
  })
  
  
  # Dữ liệu đã lọc
  filtered_data <- eventReactive(input$update, {
    req(input$indicator)
    
    data <- combined_data %>%
      filter(
        Indicator == input$indicator,
        between(year, input$year_range[1], input$year_range[2])
      )
    
    validate(
      need(nrow(data) > 0, "Không có dữ liệu phù hợp với tiêu chí đã chọn!")
    )
    
    return(data)
  })
  
  # Cập nhật danh sách quốc gia
  observeEvent(filtered_data(), {
    choices <- unique(filtered_data()$country)
    if (length(choices) > 0) {
      updateSelectizeInput(
        session, 
        "country",
        choices = choices,
        selected = choices[1],
        server = TRUE
      )
    }
  })
  
  
  # Value boxes
  output$indicator_box <- renderValueBox({
    valueBox(
      value = input$indicator,
      subtitle = "Chỉ số được chọn",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$country_box <- renderValueBox({
    n_countries <- length(unique(filtered_data()$country))
    valueBox(
      value = n_countries,
      subtitle = "Số quốc gia",
      icon = icon("globe"),
      color = "green"
    )
  })
  
  output$year_box <- renderValueBox({
    years <- paste(input$year_range[1], "-", input$year_range[2])
    valueBox(
      value = years,
      subtitle = "Khoảng năm",
      icon = icon("calendar"),
      color = "red"
    )
  })
  
  # Biểu đồ tổng quan
  output$overview_plot <- renderPlotly({
    df <- filtered_data() %>%
      group_by(year) %>%
      summarise(median_value = median(value, na.rm = TRUE))
    
    ggplotly(
      ggplot(df, aes(x = year, y = median_value)) +
        geom_line(color = "#1f77b4", size = 1) +
        geom_point(color = "#ff7f0e", size = 2) +
        labs(
          title = paste("Xu hướng:", input$indicator),
          x = "Năm", 
          y = "Giá trị trung vị"
        ) +
        theme_minimal()
    ) %>% config(displayModeBar = FALSE)
  })
  
  # Biểu đồ xu hướng
  output$trend_plot <- renderPlotly({
    req(input$country)
    
    df <- filtered_data() %>%
      filter(country %in% input$country)
    
    validate(
      need(nrow(df) > 0, "Vui lòng chọn ít nhất 1 quốc gia hợp lệ")
    )
    
    p <- ggplot(df, aes(x = year, y = value, color = country)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        title = paste(input$indicator, "theo quốc gia"),
        x = "Năm", 
        y = "Giá trị",
        color = "Quốc gia"
      ) +
      theme_minimal()
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Biểu đồ khu vực
  output$regional_plot <- renderPlotly({
    df <- filtered_data() %>%
      filter(!is.na(region)) %>%
      group_by(region) %>%
      summarise(median_value = median(value, na.rm = TRUE))
    
    validate(
      need(nrow(df) > 0, "Không có dữ liệu khu vực để hiển thị")
    )
    
    p <- ggplot(df, aes(x = reorder(region, median_value), y = median_value,
                        text = paste("Khu vực:", region, "<br>Giá trị:", round(median_value, 2)))) +
      geom_bar(stat = "identity", fill = "#2ca02c") +
      coord_flip() +
      labs(
        title = "So sánh theo khu vực", 
        x = "", 
        y = "Giá trị trung vị"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  # Bảng khu vực
  output$regional_table <- renderDT({
    df <- filtered_data() %>%
      filter(!is.na(region)) %>%
      group_by(region) %>%
      summarise(
        Trung_bình = mean(value, na.rm = TRUE),
        Trung_vị = median(value, na.rm = TRUE),
        `Độ lệch chuẩn` = sd(value, na.rm = TRUE),
        Min = min(value, na.rm = TRUE),
        Max = max(value, na.rm = TRUE),
        `Số lượng` = n()
      ) %>%
      mutate(across(where(is.numeric), ~ round(., 2)))
    
    validate(
      need(nrow(df) > 0, "Không có dữ liệu khu vực để hiển thị")
    )
    
    datatable(
      df,
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        dom = 'tip'
      ),
      rownames = FALSE
    )
  })
  
  # Bảng dữ liệu thô
  output$rawdata_table <- renderDT({
    df <- filtered_data() %>% 
      select(Quốc_gia = country, Năm = year, Khu_vực = region, Giá_trị = value)
    
    validate(
      need(nrow(df) > 0, "Không có dữ liệu để hiển thị")
    )
    
    datatable(
      df,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        scrollX = TRUE,
        pageLength = 10,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Vietnamese.json'
        )
      ),
      rownames = FALSE
    )
  })
}

# 5. Chạy ứng dụng
shinyApp(
  ui = ui, 
  server = server,
  onStart = function() {
    cat("Ứng dụng WHO Dashboard đang khởi chạy...\n")
  },
  options = list(
    launch.browser = TRUE,  # Mở trong trình duyệt mặc định
    width = "100%",
    height = 800
  )
)

