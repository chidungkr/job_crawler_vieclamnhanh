

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
      str_trim() # Cái này có thể không có. 
    
    # Ngày đăng: 
    start_date <- html_nodes(k, xpath = '//*[@id="left-content"]/article/div[1]/div[1]/time') %>% 
      html_text() %>% 
      str_replace_all("\n", "") %>% 
      str_trim() # Có thể không có. 
    
    # Tên CV: 
    job_name <- html_nodes(k, xpath = '//*[@id="left-content"]/header/h1/span') %>% 
      html_text() %>% 
      str_replace_all("\n", "") %>% 
      str_trim()
    
    # Tên công ti: 
    # company_name <- html_nodes(k, xpath = '//*[@id="left-content"]/article/div[2]/h3') %>% 
    #   html_text() %>% 
    #   str_replace_all("\n", "") %>% 
    #   str_trim()
    
    # Địa chỉ công ti: 
    # company_add <- html_nodes(k, xpath = '//*[@id="left-content"]/article/table/tbody/tr[1]/td[2]/p') %>% 
    #   html_text() %>% 
    #   str_replace_all("\n", "") %>% 
    #   str_trim() # Có thể không chuẩn. 
    
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
  all_info <- c(thong_tin1, 
                thong_tin2)
  
  all_info <- matrix(all_info, ncol = 9) %>% as.data.frame()
  names(all_info) <- c("luong", "kinh_nghiem", "trinh_do", "tinh", 
                       "nhom_cv", "so_luong", "gioi_tinh", "tinh_chat", "hinh_thuc")
  all_info %<>% mutate(link_source = job_link)
  
  if (length(job_name) != 0) {
    all_info %<>% mutate(ten_cong_viec = job_name)
  } else {
    all_info %<>% mutate(ten_cong_viec = NA)
  }
  
  if (length(yeu_cau) != 0) {
    all_info %<>% mutate(yeu_cau = yeu_cau)
  } else {
    all_info %<>% mutate(yeu_cau = NA)
  }
  
  if (length(quyen_loi) != 0) {
    all_info %<>% mutate(quyen_loi = quyen_loi)
  } else {
    all_info %<>% mutate(quyen_loi = NA)
  }
  
  if (length(mo_ta) != 0) {
    all_info %<>% mutate(nhiem_vu = mo_ta)
  } else {
    all_info %<>% mutate(nhiem_vu = NA)
  }
  
  if (length(start_date) != 0) {
    all_info %<>% mutate(ngay_dang = start_date)
  } else {
    all_info %<>% mutate(ngay_dang = NA)
  }
  

  if (length(deadline) != 0) {
    all_info %<>% mutate(het_han = deadline)
  } else {
    all_info %<>% mutate(het_han = NA)
  }
  
  return(all_info)
  
}





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




# Hàm lấy ra thông tin của nhóm công việc quan tâm với thông tin đầu vào là: 
# (1) Base url của nhóm công việc, và (2) Số pages muốn lấy: 

job_crawler_type <- function(base.url, n.pages) {
  link_group <- get_link_job_group(base.url, n.pages)
  link_group$link_source %>% 
    get_information_job() %>% 
    return()
  
}

#=========================================
#  Sử dụng các hàm đã có để lấy dữ liệu
#=========================================


# Nhóm việc kinh doanh: 
kinh_doanh_url <- "https://www.timviecnhanh.com/viec-lam-kinh-doanh-c32.html?page="
n1 <- 240
kinh_doanh_df <- job_crawler_type(base.url = kinh_doanh_url, n.pages = n1)

# Nhóm việc bán hàng: 

ban_hang_url <- "https://www.timviecnhanh.com/viec-lam-ban-hang-c10.html?page="
n2 <- 230
ban_hang_df <- job_crawler_type(base.url = ban_hang_url, n.pages = n2)

# Nhóm việc chăm sóc khách hàng 
cham_soc_url <- "https://www.timviecnhanh.com/viec-lam-cham-soc-khach-hang-c21.html?page="
n3 <- 148
cham_soc_df <- job_crawler_type(cham_soc_url, n3)

# Nhóm việc lao động phổ thông: 
lao_dong_url <- "https://www.timviecnhanh.com/viec-lam-lao-dong-pho-thong-c33.html?page="
n4 <- 120
lao_dong_df <- job_crawler_type(lao_dong_url, n4)

# Nhóm việc tài chính - kế toán - kiểm toán: 

tai_chinh_url <- "https://www.timviecnhanh.com/viec-lam-tai-chinh-ke-toan-kiem-toan-c47.html?page="
n5 <- 115
tai_chinh_df <- job_crawler_type(tai_chinh_url, n5)

# Nhóm công việc cho SV mới ra trường: 
moi_ra_truong_url <- "https://www.timviecnhanh.com/viec-lam-sinh-vien-moi-tot-nghiep-thuc-tap-c35.html?page="
n6 <- 104
ra_truong_df <- job_crawler_type(moi_ra_truong_url, n6)
# Nhóm công việc hành chính - trợ lí: 

hanh_chinh_url <- "https://www.timviecnhanh.com/viec-lam-hanh-chinh-thu-ky-tro-ly-c29.html?page="
n7 <- 104
hanh_chinh_df <- job_crawler_type(hanh_chinh_url, n7)

# Nhóm công việc quảng cáo - marketing: 

marketing_url <- "https://www.timviecnhanh.com/viec-lam-quang-cao-marketing-pr-c45.html?page="
n8 <- 80
marketing_df <- job_crawler_type(marketing_url, n8)


# Nhóm công việc cơ khí: 

co_khi_url <- "https://www.timviecnhanh.com/viec-lam-co-khi-ki-thuat-ung-dung-c16.html?page="
n9 <- 69
co_khi_df <- job_crawler_type(co_khi_url, n9)

# Nhóm công việc bất động sản: 

bat_dong_san_url <- "https://www.timviecnhanh.com/viec-lam-bat-dong-san-c13.html?page="
n10 <- 58
bat_dong_san_df <- job_crawler_type(bat_dong_san_url, n10)

# Nhóm công việc liên quan đến điện: 

dien_url <- "https://www.timviecnhanh.com/viec-lam-dien-dien-tu-dien-lanh-c22.html?page="
n11 <- 58
dien_df <- job_crawler_type(dien_url, n11)

# Nhóm công việc IT: 

it_url <- "https://www.timviecnhanh.com/viec-lam-cong-nghe-thong-tin-c17.html?page="
n12 <- 58
it_df <- job_crawler_type(it_url, n12)

# NHóm công việc du lịch - nhà hàng - khách sạn: 

du_lich_url <- "https://www.timviecnhanh.com/viec-lam-du-lich-nha-hang-khach-san-c23.html?page="
n13 <- 57
du_lich_df <- job_crawler_type(du_lich_url, n13)


# Nhóm công việc xây dựng: 

xay_dung_url <- "https://www.timviecnhanh.com/viec-lam-xay-dung-c52.html?page="
n14 <- 48
xay_dung_df <- job_crawler_type(xay_dung_url, n14)

# Nhóm ngân hàng - đầu tư - chứng khoán: 

nga_hang_url <- "https://www.timviecnhanh.com/viec-lam-ngan-hang-chung-khoan-dau-tu-c38.html?page="
n15 <- 44
ngan_hang_df <- job_crawler_type(nga_hang_url, n15)


# Nhóm quản lí: 

quan_li_url <- "https://www.timviecnhanh.com/viec-lam-quan-ly-dieu-hanh-c44.html?page="
n16 <- 40
quan_li_df <- job_crawler_type(quan_li_url, n16)

# Nhóm việc nhân sự: 

nhan_su_url <- "https://www.timviecnhanh.com/viec-lam-nhan-su-c40.html?page="
n17 <- 38
nhan_su_df <- job_crawler_type(nhan_su_df, n17)


# Nhóm phát triển thị trường: 

thi_truong_url <- "https://www.timviecnhanh.com/viec-lam-phat-trien-thi-truong-c65.html?page="
n18 <- 38
thi_truong_df <- job_crawler_type(thi_truong_url, n18)


# Bảo hiểm: 
bao_hiem_url <- "https://www.timviecnhanh.com/viec-lam-tu-van-bao-hiem-c11.html?page="
n19 <- 38
bao_hiem_df <- job_crawler_type(bao_hiem_url, n19)

# Bảo vệ - an ninh: 
bao_ve_url <- "https://www.timviecnhanh.com/viec-lam-bao-ve-ve-si-an-ninh-c20.html?page="
n20 <- 34
bao_ve_df <- job_crawler_type(bao_ve_url, n20)



# Kho vận - vật tư: 
kho_van_url <- "https://www.timviecnhanh.com/viec-lam-kho-van-vat-tu-thu-mua-c30.html?page="
n21 <- 32
kho_van_df <- job_crawler_type(kho_van_url, n21)

# Kiến trúc - nội thất: 
kien_truc_url <- "https://www.timviecnhanh.com/viec-lam-kien-truc-noi-that-c31.html?page="
n22 <- 30
kien_truc_df <- job_crawler_type(kien_truc_url, n22)


# Thiết kế - Mĩ thuật: 

thiet_ke_url <- "https://www.timviecnhanh.com/viec-lam-thiet-ke-my-thuat-c49.html?page="
n23 <- 28
thiet_ke_df <- job_crawler_type(thiet_ke_url, n23)

# Nhóm tài xế - vận tải: 
tai_xe_url <- "https://www.timviecnhanh.com/viec-lam-tai-xe-lai-xe-giao-nhan-c60.html?page="
n24 <- 28
tai_xe_df <- job_crawler_type(tai_xe_url, n24)


