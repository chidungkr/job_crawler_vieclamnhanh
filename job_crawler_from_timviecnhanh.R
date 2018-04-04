

#=================================================
#    Phần 1: Lấy ra tất cả link
#    Đầu vào: loại công việc + số trang
#    Data Source: https://www.timviecnhanh.com
#=================================================

library(rvest)
library(tidyverse)
library(magrittr)
library(stringr)
library(httr)



# Hàm lấy ra link về việc làm của từng trang đơn lẻ: 
get_link <- function(your_link) {
  
  k <- http_status(GET(your_link))
  
  if (k$category == "Success") {
    m <- readLines(your_link)
    m <- m[m %>% str_detect("https://www.timviecnhanh.com/")]
    m <- m[m %>% str_detect("title=")]
    m <- m[m %>% str_detect("class")]
    
    u1 <- str_locate(m, "https://") %>% as.data.frame() %>% pull(start)
    u2 <- str_locate(m, "html") %>% as.data.frame() %>% pull(end)
    
    link <- str_sub(m, start = u1, end = u2)
    link <- link[!is.na(link)]
    link <- link[!str_detect(link, "dang-nhap")]
    link <- link[!duplicated(link)] %>% as.character()
    
  }
  return(data.frame(link_source = link))
  
}


# Hàm lấy ra tất cả các link của một nhóm công việc được tuyển dụng: 


get_link_job_group <- function(base_url, n_pages) {
  
  all_pages <- paste0(base_url, 1:n_pages)
  job_list <- vector("list", length = n_pages)
  for (i in seq_along(all_pages)) {
    job_list[[i]] <- get_link(all_pages[i])
    Sys.sleep(2)
  }
  
  all_pages_df <- do.call("bind_rows", job_list)
  return(all_pages_df %>% filter(!duplicated(link_source)))
  
}


#=========================================================
# Phần thứ hai: Lấy ra thông tin về công việc ở mỗi link
#=========================================================



job_crawler <- function(job_link) {
  
  k <- http_status(GET(job_link))
  
  if (k$category == "Success") {
    read_html(job_link) -> k
    # Mảng thông tin thứ nhất (mức lương, kinh nghiệm...): 
    html_nodes(k, xpath = '//*[@id="left-content"]/article/div[5]/div[1]/ul') %>% 
      html_text() %>% 
      str_replace_all("\n", "") %>% 
      str_trim() -> p
    
    
    str_split(p, "   ", simplify = TRUE) %>% as.vector() -> p
    p <- p[str_count(p) != 0]
    p <- p[!str_detect(p, "%")]
    
    p %>% matrix(nrow = 5, byrow = TRUE) %>% as.data.frame() -> u
    u$V2 %>% as.character() -> thong_tin1
    
    # Mảng thông tin thứ hai: 
    html_nodes(k, xpath = '//*[@id="left-content"]/article/div[5]/div[2]/ul') %>% 
      html_text() %>% 
      str_replace_all("\n", "") %>% 
      str_trim() -> p
    
    str_split(p, "   ", simplify = TRUE) %>% as.vector() -> p
    p <- p[str_count(p) != 0]
    
    p[1:8] %>% matrix(nrow = 4, byrow = TRUE) %>% as.data.frame() -> u
    u$V2 %>% as.character() -> thong_tin2
    
    # Hạn nộp HS: 
    deadline <- html_nodes(k, xpath = '//*[@id="left-content"]/article/table/tbody/tr[4]/td[2]/b') %>% 
      html_text() %>% 
      str_replace_all("\n", "") %>% 
      str_trim()
    
    # Ngày đăng: 
    start_date <- html_nodes(k, xpath = '//*[@id="left-content"]/article/div[1]/div[1]/time') %>% 
      html_text() %>% 
      str_replace_all("\n", "") %>% 
      str_trim()
    
    # Tên CV: 
    job_name <- html_nodes(k, xpath = '//*[@id="left-content"]/header/h1/span') %>% 
      html_text() %>% 
      str_replace_all("\n", "") %>% 
      str_trim()
    
    # Tên công ti: 
    company_name <- html_nodes(k, xpath = '//*[@id="left-content"]/article/div[2]/h3') %>% 
      html_text() %>% 
      str_replace_all("\n", "") %>% 
      str_trim()
    
    # Địa chỉ công ti: 
    company_add <- html_nodes(k, xpath = '//*[@id="left-content"]/article/table/tbody/tr[1]/td[2]/p') %>% 
      html_text() %>% 
      str_replace_all("\n", "") %>% 
      str_trim()
    
    # Mô tả công việc: 
    
    html_nodes(k, xpath = '//*[@id="left-content"]/article/table/tbody/tr[1]/td[2]/p') %>% 
      html_text() %>% 
      str_replace_all("\n", "") %>% 
      str_trim() -> mo_ta
    
    # Yêu cầu: 
    html_nodes(k, xpath = '//*[@id="left-content"]/article/table/tbody/tr[2]/td[2]/p') %>% 
      html_text() %>% 
      str_replace_all("\n", "") %>% 
      str_trim() -> yeu_cau
    
    # Quyền lợi khác: 
    
    html_nodes(k, xpath = '//*[@id="left-content"]/article/table/tbody/tr[3]/td[2]') %>% 
      html_text() %>% 
      str_replace_all("\n", "") %>% 
      str_trim() -> quyen_loi
    
  }
  all_info <- c(thong_tin1, thong_tin2, mo_ta, yeu_cau, quyen_loi, 
                company_add, company_name, start_date, deadline)
  
  all_info <- matrix(all_info, ncol = 16) %>% as.data.frame()
  names(all_info) <- c("luong", "kinh_nghiem", "trinh_do", "tinh", 
                       "nhom_cv", "so_luong", "gioi_tinh", "tinh_chat", 
                       "hinh_thuc", "nhiem_vu", "yeu_cau", "quyen_loi", 
                       "dia_chi_ct", "ten_ct", "ngay_dang", "het_han")
  return(all_info %>% mutate(link_source = job_link))
  
}



# Sử dụng hàm cho nhóm công việc kinh doanh. Trước hết lấy các công việc về
# kinh doanh đăng trên 100 pages đầu tiên (cho đỡ mất thời gian). Nếu test 
# hàm thì thay 100 bằng 10: 

kinh_doanh_link <- get_link_job_group("https://www.timviecnhanh.com/viec-lam-kinh-doanh-c32.html?page=", 100)



# Viết hàm lấy ra các thông tin về công việc được tuyển 
# với đầu vào là một danh sách các link công việc: 

get_information_job <- function(list_job_link) {
  n <- length(list_job_link)
  job_inf <- vector("list", length = n)
  for (i in seq_along(list_job_link)) {
    job_inf[[i]] <- job_crawler(list_job_link[i])
    Sys.sleep(2)
  }
  job_inf <- do.call("bind_rows", job_inf)
  return(job_inf)
}


# Sử dụng hàm: 

kinh_doanh_link$link_source %>% get_information_job() -> df1

# Các thông tin có được từ nhóm công việc kinh doanh (xem qua): 
df1[, 1:4] %>% head()








