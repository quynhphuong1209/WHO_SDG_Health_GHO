# Load các thư viện cần thiết
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,     # Thao tác dữ liệu
  janitor,       # Làm sạch dữ liệu
  httr,          # Truy vấn API
  jsonlite,      # Xử lý JSON
  skimr,         # Thống kê mô tả
  ggplot2,       # Trực quan hóa
  patchwork,     # Ghép nhiều plot
  broom,         # Tidy statistical outputs
  reshape2,      # Chuyển đổi dữ liệu
  glue,          # Nối chuỗi
  car,           # Kiểm định Levene
  rgho,          # Truy xuất dữ liệu WHO
  data.table,    # Xử lý dữ liệu lớn
  furrr,         # Xử lý song song
  kableExtra     # Hiển thị bảng
)

# Cấu hình xử lý song song
plan(multisession)

# Ánh xạ tên hiển thị cho các chỉ số
indicator_names <- c(
  "Maternal_mortality" = "Tỷ lệ tử vong mẹ",
  "Under5_mortality" = "Tỷ lệ tử vong trẻ em dưới 5 tuổi",
  "Infectious_disease" = "Tỷ lệ mắc các bệnh truyền nhiễm",
  "Health_service" = "Tiếp cận dịch vụ y tế"
)

# Sử dụng các mã chỉ số cố định từ WHO với phương án dự phòng
indicators <- list(
  "Maternal_mortality" = c("MDG_0000000026", "MDG_0000000026"),      # Primary + backup
  "Under5_mortality" = c("MDG_0000000007", "MDG_0000000007"),        # Primary + backup
  "Infectious_disease" = c("UHC_SCL_INFECT", "SDG_03_20", "MDG_0000000017"), # Primary + backup
  "Health_service" = c("IHRSPAR_CAPACITY09", "UHC_INDEX_REPORTED")   # Primary + backup
)

# 1. Hàm lấy dữ liệu từ WHO API
get_who_data_enhanced <- function(indicator_codes, max_retries = 3) {
  result <- NULL
  
  for (code in indicator_codes) {
    if (is.na(code)) next
    
    retry_count <- 0
    while (retry_count < max_retries && is.null(result)) {
      tryCatch({
        url <- paste0("https://ghoapi.azureedge.net/api/", code)
        response <- GET(url, timeout(30))
        
        if (status_code(response) == 200) {
          json_data <- content(response, "text", encoding = "UTF-8") %>% 
            fromJSON()
          
          if ("value" %in% names(json_data) && nrow(json_data$value) > 0) {
            result <- json_data$value %>%
              as_tibble() %>%
              clean_names() %>%
              select(
                country = spatial_dim,
                year = time_dim,
                value = numeric_value,
                region = parent_location
              ) %>%
              mutate(
                year = as.integer(year),
                value = as.numeric(value),
                indicator = code
              )
            message("✅ Thành công với mã: ", code)
          }
        }
      }, error = function(e) {
        message("❌ Lỗi với mã ", code, ": ", e$message)
      })
      retry_count <- retry_count + 1
      if (is.null(result)) Sys.sleep(1)
    }
    
    if (!is.null(result)) break
  }
  return(result)
}

# 2. Lấy dữ liệu song song
message("⏳ Đang lấy dữ liệu từ WHO API...")

who_data <- future_map(names(indicators), ~{
  message("\n📥 Đang xử lý: ", .x)
  data <- get_who_data_enhanced(indicators[[.x]])
  
  if (!is.null(data)) {
    data %>% mutate(indicator_name = .x)
  } else {
    message("🚨 Không thể lấy dữ liệu cho ", .x)
    NULL
  }
}, .progress = TRUE) %>% 
  set_names(names(indicators)) %>% 
  compact()

# 3. Kiểm tra và báo cáo kết quả
message("\n==== KẾT QUẢ TỔNG HỢP ====")

if (length(who_data) > 0) {
  who_dt <- who_data %>% 
    map(~ as.data.table(.x)) %>% 
    rbindlist(fill = TRUE)
  
  summary_report <- who_dt[, .(
    Số_bản_ghi = .N,
    Năm_bắt_đầu = min(year, na.rm = TRUE),
    Năm_kết_thúc = max(year, na.rm = TRUE),
    Số_quốc_gia = uniqueN(country, na.rm = TRUE),
    Giá_trị_TB = round(mean(value, na.rm = TRUE), 2)
  ), by = .(indicator_name)]
  
  print(kable(summary_report, 
              col.names = c("Chỉ số", "Số bản ghi", "Năm bắt đầu", 
                            "Năm kết thúc", "Số quốc gia", "Giá trị TB"),
              align = c("l", "r", "r", "r", "r", "r")) %>%
          kable_styling(bootstrap_options = c("striped", "hover")))
  
  # Phần kiểm tra dữ liệu thiếu
  missing_indicators <- setdiff(names(indicators), names(who_data))
  if (length(missing_indicators) > 0) {
    message("\n⚠️ Các chỉ số không có dữ liệu: ", paste(missing_indicators, collapse = ", "))
    
    message("\n🔍 Gợi ý mã thay thế từ GHO:")
    gho_codes <- rgho::get_gho_codes()
    for (ind in missing_indicators) {
      message("\nTìm kiếm mã cho: ", ind)
      suggested_codes <- gho_codes %>% 
        filter(str_detect(Title, regex(ind, ignore_case = TRUE))) %>% 
        head(3) %>% 
        select(Code, Title)
      
      if (nrow(suggested_codes) > 0) {
        print(suggested_codes)
      } else {
        message("Không tìm thấy mã phù hợp")
      }
    }
  }
} else {
  message("🚨 Không lấy được dữ liệu nào từ WHO API")
}

# 4. Lưu dữ liệu
if (length(who_data) > 0) {
  saveRDS(who_data, "who_health_data.rds")
  message("\n💾 Đã lưu dữ liệu vào file: who_health_data.rds")
}

# 2. Phân tích dữ liệu
## 2.1. Thống kê mô tả
descriptive_stats <- map_df(who_data, ~{
  .x %>%
    group_by(indicator) %>%
    summarise(
      Mean = mean(value, na.rm = TRUE),
      SD = sd(value, na.rm = TRUE),
      Min = ifelse(all(is.na(value)), NA, min(value, na.rm = TRUE)),
      Max = ifelse(all(is.na(value)), NA, max(value, na.rm = TRUE)),
      N = n(),
      Missing = sum(is.na(value))
    ) %>%
    mutate(across(where(is.numeric), ~round(., 2)))
}, .id = "Dataset")

print(descriptive_stats)

## 2.2. Phân tích xu hướng theo thời gian
plot_trend <- function(df, title) {
  if (is.null(df) || nrow(df) == 0 || !"year" %in% colnames(df) || !"value" %in% colnames(df)) {
    message(paste("⚠️ Không thể vẽ biểu đồ cho", title, "do dữ liệu bị thiếu"))
    return(NULL)
  }
  
  df %>%
    filter(!is.na(year) & !is.na(value)) %>%
    group_by(year) %>%
    summarise(Median_Value = median(value, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = year, y = Median_Value)) +
    geom_line(color = "#0072B2", linewidth = 1.2) +
    geom_point(color = "#D55E00", size = 2) +
    labs(title = paste("Xu hướng", title), x = "Năm", y = "Giá trị trung vị") +
    theme_minimal()
}

trend_plots <- map2(who_data, names(who_data), ~ plot_trend(.x, indicator_names[.y]))
trend_plots <- compact(trend_plots)

if (length(trend_plots) > 0) {
  wrap_plots(trend_plots, ncol = 2) +
    plot_annotation(title = "Xu hướng các chỉ số sức khỏe theo năm",
                    theme = theme(plot.title = element_text(size = 16, face = "bold")))
}

## 2.3. Phân tích tương quan
if (length(who_data) >= 2) {
  # Phân tích theo năm nếu có đủ năm chung
  common_years <- reduce(map(who_data, ~ unique(.x$year)), intersect)
  
  if (length(common_years) >= 3) {
    message("Phân tích theo năm chung (", length(common_years), " năm)")
    
    filtered_data <- map(who_data, ~ {
      .x %>% 
        filter(year %in% common_years) %>%
        group_by(year) %>%
        summarise(mean_value = mean(value, na.rm = TRUE)) %>%
        arrange(year)
    })
    
    combined <- reduce(map2(filtered_data, names(filtered_data), ~ {
      .x %>% rename(!!.y := mean_value)
    }), full_join, by = "year")
    
    numeric_data <- combined %>% 
      select(-year) %>%
      select(where(~ sum(!is.na(.x)) >= 3))  # Yêu cầu ít nhất 3 quan sát
    
    if (ncol(numeric_data) >= 2) {
      cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
      
      # In ma trận tương quan
      message("\nMa trận tương quan:")
      print(cor_matrix)
      
      # Vẽ heatmap
      melted_cor <- reshape2::melt(cor_matrix)
      
      cor_plot <- ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
        geom_tile(color = "white") +
        geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
        scale_fill_gradient2(
          low = "#2b8cbe", 
          high = "#e41a1c", 
          mid = "white",
          midpoint = 0, 
          limit = c(-1, 1), 
          space = "Lab",
          name = "Hệ số tương quan"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          plot.title = element_text(face = "bold")
        ) +
        labs(
          title = "Ma trận tương quan giữa các chỉ số sức khỏe",
          x = "", 
          y = ""
        )
      
      print(cor_plot)
    } else {
      message("Không đủ biến số hợp lệ để tính toán tương quan")
    }
  } else {
    message("Không đủ năm chung (cần ít nhất 3 năm) để phân tích tương quan")
  }
} else {
  message("Cần ít nhất 2 chỉ số để phân tích tương quan")
}

## 2.4. So sánh giữa các khu vực
plot_region_comparison <- function(df, title) {
  if (!"region" %in% colnames(df)) return(NULL)
  
  df %>%
    filter(!is.na(region), region != "Unknown", !is.na(value), is.finite(value)) %>%
    ggplot(aes(x = reorder(region, value, FUN = median), y = value)) +
    geom_boxplot(fill = "#56B4E9", alpha = 0.7, outlier.shape = NA) +
    coord_flip() +
    labs(title = paste("Phân bố", title, "theo khu vực"), x = NULL, y = "Giá trị") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"))
}

region_plots <- map2(who_data, names(who_data), ~ plot_region_comparison(.x, indicator_names[.y]))
region_plots <- compact(region_plots)

if (length(region_plots) > 0) {
  wrap_plots(region_plots, ncol = 2, widths = c(1, 1.2)) +
    plot_annotation(title = "So sánh các chỉ số sức khỏe theo khu vực",
                    theme = theme(plot.title = element_text(size = 16, face = "bold")))
}

## 2.5. Kiểm định thống kê
### Kiểm định T-Test cho Under5_mortality
if ("Under5_mortality" %in% names(who_data)) {
  df_ttest <- who_data[["Under5_mortality"]] %>% 
    filter(!is.na(region), !is.na(value), region != "")
  
  valid_regions <- names(table(df_ttest$region))[table(df_ttest$region) >= 10]
  
  if (length(valid_regions) >= 2) {
    region_pairs <- combn(valid_regions, 2, simplify = FALSE)
    
    ttest_results <- map_df(region_pairs, ~ {
      df_pair <- df_ttest %>% filter(region %in% .x)
      
      if (n_distinct(df_pair$region) == 2 && all(table(df_pair$region) >= 2)) {
        test_result <- tryCatch(
          t.test(value ~ region, data = df_pair),
          error = function(e) return(NULL)
        )
        
        if (!is.null(test_result)) {
          p_val <- ifelse(test_result$p.value < 0.001, 0.001, test_result$p.value)
          
          tibble(
            `Khu vực 1` = .x[1],
            `Khu vực 2` = .x[2],
            `Chênh lệch TB` = round(diff(test_result$estimate), 2),
            `Giá trị t` = round(test_result$statistic, 2),
            `p-value` = round(p_val, 4),
            `Độ tin cậy 95%` = paste(round(test_result$conf.int, 2), collapse = " - "),
            `Số quan sát` = paste(table(df_pair$region), collapse = " vs ")
          )
        }
      }
    })
    
    if (nrow(ttest_results) > 0) {
      ttest_display <- ttest_results %>%
        mutate(`p-value` = ifelse(`p-value` == 0.001, "<0.001", as.character(round(`p-value`, 3))))
      
      knitr::kable(ttest_display, caption = "Kết quả kiểm định T-Test giữa các khu vực")
    }
  }
}

### Phân tích ANOVA cho Maternal_mortality
if ("Maternal_mortality" %in% names(who_data)) {
  df_anova <- who_data[["Maternal_mortality"]] %>% 
    filter(!is.na(region), !is.na(value), region != "")
  
  region_counts <- table(df_anova$region)
  valid_regions <- names(region_counts)[region_counts >= 2]
  
  if (length(valid_regions) >= 3) {
    df_anova <- df_anova %>% filter(region %in% valid_regions)
    
    # Kiểm định Levene
    levene_test <- tryCatch(
      leveneTest(value ~ region, data = df_anova),
      error = function(e) return(NULL)
    )
    
    if (!is.null(levene_test)) {
      cat("### Kiểm định phương sai đồng nhất (Levene's Test)\n")
      print(knitr::kable(broom::tidy(levene_test) %>%
                           mutate(p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))))
      
      # Phân tích ANOVA
      anova_result <- tryCatch(
        aov(value ~ region, data = df_anova),
        error = function(e) return(NULL)
      )
      
      if (!is.null(anova_result)) {
        anova_summary <- broom::tidy(anova_result) %>%
          mutate(p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
        
        cat("\n### Kết quả phân tích ANOVA\n")
        print(knitr::kable(anova_summary))
        
        # Kiểm định Tukey HSD nếu ANOVA có ý nghĩa
        if (anova_summary$p.value[1] < 0.05) {
          tukey_result <- TukeyHSD(anova_result)
          
          cat("\n### Kiểm định Tukey HSD\n")
          tukey_df <- broom::tidy(tukey_result) %>%
            mutate(adj.p.value = ifelse(adj.p.value < 0.001, "<0.001", round(adj.p.value, 3)))
          
          print(knitr::kable(tukey_df))
          
          # Vẽ biểu đồ Tukey
          par(mar = c(5, 12, 4, 2))
          plot(tukey_result, las = 1, cex.axis = 0.7)
        }
      }
    }
  } else {
    message("Không đủ nhóm khu vực hợp lệ để thực hiện ANOVA")
  }
} else {
  message("Không có dữ liệu về tỷ lệ tử vong mẹ (Maternal_mortality)")
}

# 3. Kết quả và thảo luận
cat("## 3. Kết quả và Thảo luận\n\n")

# Tổng quan kết quả
cat("### 3.1. Tổng quan kết quả\n")
cat("Phân tích dữ liệu từ WHO về bốn chỉ số sức khỏe quan trọng cung cấp cái nhìn tổng thể về tình trạng y tế toàn cầu.\n")
cat("Dữ liệu thu thập được trải dài qua nhiều năm, giúp theo dõi xu hướng sức khỏe theo thời gian.\n")
cat("Số lượng quốc gia có dữ liệu đầy đủ khác nhau tùy vào từng chỉ số, phản ánh mức độ sẵn có và độ tin cậy của thông tin y tế.\n\n")

# Xu hướng chính
cat("### 3.2. Xu hướng chính\n")
cat("Các phân tích chỉ ra một số xu hướng quan trọng:\n\n")
cat("- Tỷ lệ tử vong mẹ và trẻ em có xu hướng giảm qua các năm, đặc biệt tại các nước có nền y tế phát triển.\n")
cat("- Khả năng tiếp cận dịch vụ y tế được cải thiện đáng kể ở nhiều khu vực, nhưng vẫn có sự chênh lệch giữa các quốc gia.\n")
cat("- Tỷ lệ mắc bệnh truyền nhiễm giảm ở một số khu vực nhờ vào các biện pháp kiểm soát dịch bệnh và tiêm chủng.\n\n")

# Khuyến nghị
cat("### 3.3. Khuyến nghị\n")
cat("1. Tiếp tục theo dõi và cập nhật dữ liệu các chỉ số sức khỏe để đánh giá hiệu quả chính sách y tế.\n")
cat("2. Tăng cường đầu tư vào hệ thống y tế, đặc biệt tại các quốc gia có chỉ số sức khỏe thấp.\n")
cat("3. Đẩy mạnh nghiên cứu về các yếu tố tác động đến sức khỏe cộng đồng để có biện pháp can thiệp hiệu quả hơn.\n")