# Google play crawler --------------------------------------------------------
pacman::p_load(tidyverse, httr, lubridate, xml2, rvest, webdriver, tidygraph)


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
  original_info <- get_app_info(url)
  relate <- get_app_relations(url)
  relation_info <- map_df(relate$links, get_app_info)
  final_nodes <- relate %>%
    left_join(relation_info) %>%
    bind_rows(original_info) %>%
    select(name:links)
  
  final_edges <- relate %>%
    select(to = links) %>%
    mutate(from = original_info$links)
  if(graph == F){
  return(list("app_nodes" = final_nodes,
              "app_edges" = final_edges))
  } else{
  final_graph <- tidygraph::as_tbl_graph(final_edges) %N>%
    left_join(final_nodes)
  return(final_graph)
  }
}
followers <- connect_apps("https://play.google.com/store/apps/details?id=com.roblox.client&gl=US")
follow_graph <- connect_apps("https://play.google.com/store/apps/details?id=com.roblox.client&gl=US", T)




crawl_apps <- function(url, depth = 1, graph = F){
    followers <- connect_apps(url)
    to_scrape <- followers$app_edges$to %>%
      unique()
    scraped <- followers$app_edges$from %>%
      unique()
    clean_scrape <- to_scrape[!to_scrape %in% scraped]
    
    new_depth <- map(clean_scrape, connect_apps)
    
    if(depth == 1){
    
    all_apps <- c(to_scrape, scraped)
    
    depth_1_edge <- new_depth %>%
      map(~.$app_edges) %>%
      map_df(filter, to %in% all_apps) %>%
      bind_rows(followers$app_edges) %>%
      unique()
    depth_1_node <- new_depth %>%
      map(~.$app_nodes) %>%
      map_df(filter, links %in% all_apps) %>%
      bind_rows(followers$app_nodes) %>%
      unique()
    
    depth_1_final <- list(app_nodes = depth_1_node,
                          app_edges = depth_1_edge)
    return(depth_1_final)
    }
    
    
    
    targets <- new_depth %>%
      map(~.$app_edges$to) %>%
      unlist() %>%
      unique()
    
    origin_apps <- new_depth %>%
      map(~.$app_edges$from) %>%
      unlist() %>%
      unique() %>%
      c(., scraped)
    
    new_clean_scrape <- targets[!targets %in% origin_apps]
    
    new_followers <- map(new_clean_scrape, connect_apps)

}















