# Import packages ----------------------------------------------------------

pacman::p_load(tidyverse, httr, lubridate, xml2, rvest, webdriver)

# Choose link to scrape ---------------------------------------------------

# a few exemples to use
u <- "https://play.google.com/store/apps/details?id=com.roblox.client&gl=US"
u <- "https://play.google.com/store/apps/details?id=br.com.brainweb.ifood"
u <- "https://play.google.com/store/apps/details?id=com.mercadolibre&gl=US"
# Get html ----------------------------------------------------------------


app_h <- httr::GET(u)

# Get description
app_info <- app_h %>%
  read_html()

# Get basic information ---------------------------------------------------

app_title <- app_info %>%
  xml2::xml_find_first("//h1[@class = 'Fd93Bb ynrBgc xwcR9d']") %>%
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
relations <- tibble(app_name = get_trios(1),
       app_producer = get_trios(2),
       app_evaluation = get_trios(3),
       links = related_apps_link) %>%
  mutate(app_evaluation = str_extract(app_evaluation, "[0-9.]+") %>%
           as.numeric(),
         links = str_c("https://play.google.com", links))

# Get safe information -----------------------------------------------------
link_id <- str_extract(u, "id=.*")

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

# Teste abrir a página --------------------------------------------------------


# webdriver::install_phantomjs()
pjs <- run_phantomjs()

# Fazendo isso, o phantomJS já está rodando

pjs$port # Aqui ele está usando uma porta aleatória para rodar

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
eval_fin <- general_eval %>%
  mutate(total_stars = stars*evaluations) %>%
  summarise(avarage = sum(total_stars)/sum(evaluations))

all_comments <- tibble(comment = comment_text,
                       stars = eval_star) %>%
  mutate(stars = str_extract(stars, "[0-9]"))


