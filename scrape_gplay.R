# Import packages -----------------------------------------------------------

pacman::p_load(tidyverse, httr, lubridate, xml2, rvest, webdriver)


# Get basic information --------------------------------------------------

get_app_info <- function(url){
  
  app_h <- httr::GET(url)
  
  # Get description
  app_info <- app_h %>%
    read_html()
  
  
  app_title <- app_info %>%
    xml2::xml_find_first("//h1[starts-with(@class, 'Fd93Bb')]") %>%
    xml2::xml_text()
  
  app_score <- app_info %>%
    xml2::xml_find_first("//div[@class = 'TT9eCd']") %>%
    xml2::xml_text() %>%
    str_remove("star") %>%
    as.numeric()
  
  app_description <- app_info %>%
    xml2::xml_find_first("//div[@class = 'bARER']") %>%
    xml2::xml_text()
  
  last_update <- app_info %>%
    xml2::xml_find_first("//div[@class = 'xg1aie']") %>%
    xml2::xml_text()
  
  tags <- app_info %>%
    xml2::xml_find_all("//span[@jsname = 'V67aGc']") %>%
    xml_text()
  
  prod_link <- app_info %>%
    xml2::xml_find_first("//div[@class = 'Vbfug auoIOc']/a[contains(@href, 'store')]") %>%
    html_attr('href')
  
  prod_name <- app_info %>%
    xml2::xml_find_first("//div[@class = 'Vbfug auoIOc']") %>%
    xml_text()
  
  app_producer <- tibble(link = prod_link,
                         producer = prod_name) %>%
    mutate(link = str_c('https://play.google.com', link))
  
  
  
  app_overview <- tibble(name = app_title,
                         description = app_description,
                         updated = last_update,
                         tags = list(tags),
                         evaluation = app_score) %>%
    bind_cols(app_producer)
  
  return(app_overview)
}
get_app_info("https://play.google.com/store/apps/details?id=com.roblox.client&gl=US")


# Create network of relations  --------------------------------------------------

get_app_relations <- function(url){
  
  app_h <- httr::GET(url)
  
  # Get description
  app_info <- app_h %>%
    read_html()
  
  related_apps <- app_info %>%
    xml2::xml_find_all("//div[@class = 'ubGTjb']") %>%
    xml_text() %>%
    .[1:18]
  
  related_apps_link <- app_info %>%
    xml2::xml_find_all("//a[@class = 'Si6A0c nT2RTe']") %>%
    html_attr('href') %>%
    .[1:6]
  
  get_trios <- function(.x){
    related_apps[seq(.x, length(related_apps), 3)]
  }
  relations <- tibble(name = get_trios(1),
                      producer = get_trios(2),
                      evaluation = get_trios(3),
                      links = related_apps_link) %>%
    mutate(evaluation = str_extract(evaluation, "[0-9.]+") %>%
             as.numeric(),
           links = str_c("https://play.google.com", links))
  
  return(relations)
}
get_app_relations(relations$links[2])


# Get safe information -----------------------------------------------------

get_app_privacy <- function(url){
  link_id <- str_extract(url, "id=.*")
  
  u_safe <- glue::glue("https://play.google.com/store/apps/datasafety?{link_id}")
  
  l_safe <- GET(u_safe)
  
  safe_r <- l_safe %>%
    read_html()
  
  privacy_item <- safe_r %>%
    xml2::xml_find_all("//h3[@class = 'aFEzEb']") %>%
    html_text()
  
  privacy_accept <- safe_r %>%
    xml2::xml_find_all("//div[@class = 'fozKzd']") %>%
    html_text()
  
  app_privacy <- tibble(topic = privacy_item,
                        requirements = privacy_accept) %>%
    separate_rows(requirements, sep = "( and |, and |, )")
  return(app_privacy)
}
get_app_privacy(relations$links[4])


get_app_evaluations <- function(url){
  
  # webdriver::install_phantomjs()
  pjs <- run_phantomjs()
  
  # Fazendo isso, o phantomJS já está rodando
  
  # A nossa porta de acesso já está rodando
  ses <- Session$new(port = pjs$port) # o pacote session tem o método new, e a gente coloca a porta certa
  # como se a gente tivesse clicado no botão para abrir o navegador
  
  ses$go(u)
  Sys.sleep(2)
  ses$takeScreenshot()
  
  botao <- ses$findElement(xpath = '//*[@id="yDmH0d"]/c-wiz[2]/div/div/div[1]/div[2]/div/div[1]/c-wiz[4]/section/header/div/div[2]/button')
  botao$click()
  
  
  html <- ses$getSource()
  
  ses$delete()
  
  
  # readr::write_file(html, "output/all_comments.html")
  
  comment_text <- xml2::read_html(html) %>%
    xml2::xml_find_all("//div[@class='h3YV2d']") %>%
    xml2::xml_text()
  comment_useful <- xml2::read_html(html) %>%
    xml2::xml_find_all("//div[@class='AJTPZc']") %>%
    xml2::xml_text()
  stars <- xml2::read_html(html) %>%
    xml2::xml_find_all("//div[@role='img']") %>%
    html_attr("aria-label")
  
  overall_star <- stars[2:6] %>%
    str_extract("[0-9.]+") %>%
    str_remove_all("\\.") %>%
    as.numeric()
  
  general_eval <- tibble(stars = 5:1,
                         evaluations = overall_star)

  all_comments <- tibble(comment = comment_text,
                         stars = eval_star) %>%
    mutate(stars = str_extract(stars, "[0-9]"))
  
  final <- list("avarage_star" = general_eval,
                "comments" = all_comments) 
  return(final)
}

get_app_evaluations(relations$links[4])
