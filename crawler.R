# Google play crawler --------------------------------------------------------
pacman::p_load(tidygraph)


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
                         evaluation = app_score,
                         links = url) %>%
    bind_cols(app_producer)
  
  return(app_overview)
}

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


connect_apps <- function(url, graph = F){
  original_info <- get_app_info(u)
  relate <- get_app_relations(u)
  relation_info <- map_df(relate$links, get_app_info)
  final_nodes <- relate %>%
    left_join(relation_info) %>%
    bind_rows(original_info) %>%
    select(name:links)
  
  final_edges <- relate %>%
    select(to = name) %>%
    mutate(from = original_info$name)
  if(graph == F){
  return(list("app_nodes" = final_nodes,
              "app_edges" = final_edges))
  } else{
  final_graph <- tidygraph::as_tbl_graph(final_edges) %N>%
    left_join(final_nodes)
  return(final_graph)
  }
}
followers <- crawl_for_apps("https://play.google.com/store/apps/details?id=com.roblox.client&gl=US")
follow_graph <- crawl_for_apps("https://play.google.com/store/apps/details?id=com.roblox.client&gl=US", T)


