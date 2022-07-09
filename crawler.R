# Google play crawler --------------------------------------------------------
pacman::p_load(tidyverse, httr, lubridate, xml2, rvest, webdriver, tidygraph, ggraph)


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

u <- "https://play.google.com/store/apps/details?id=com.roblox.client&gl=US"

connect_apps(clean_scrape[1])
crawl_app <- function(url, depth = 1, graph = F){
  
  try_connect_apps <- possibly(connect_apps, otherwise = NULL)
  
    followers <- try_connect_app(url)
    if(is.null(followers)){
      usethis::ui_stop("This app can't bee the seed")}
    to_scrape <- followers$app_edges$to %>%
      unique()
    scraped <- followers$app_edges$from %>%
      unique()
    clean_scrape <- to_scrape[!to_scrape %in% scraped]
    
    usethis::ui_info("Start scrapping at depth 1")
    
    progressr::with_progress({
    p <- progressr::progressor(length(clean_scrape)) # O parâmetro é a quantidade de passos
      
    new_depth <- map(clean_scrape, 
                     ~{p()
                       try_connect_apps(.)}) %>%
      keep(~!is.null(.))
    })
    
    split_edge_node <- function(.x, limit){
      final_edge <- .x %>%
        map(~.$app_edges) %>%
        map_df(filter, to %in% limit) %>%
        bind_rows(followers$app_edges) %>%
        unique()
      final_node <- .x %>%
        map(~.$app_nodes) %>%
        map_df(filter, links %in% limit) %>%
        bind_rows(followers$app_nodes) %>%
        unique()
      final <- list(app_nodes = final_node,
                    app_edges = final_edge)
      return(final)
    }
    
    if(depth == 1){
    
    all_apps <- c(to_scrape, scraped)
    
    final <- split_edge_node(new_depth, all_apps)
    
    
    } else if(depth == 2){
    
    
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

    usethis::ui_info("Start scrapping at depth 2")
    progressr::with_progress({
    p <- progressr::progressor(length(new_clean_scrape)) # O parâmetro é a quantidade de passos
      
    new_followers <- map(new_clean_scrape, ~{
      p()
      try_connect_apps(.) %>%
        keep(~!is.null(.))
      })
    })
    
    all_depth <- c(new_depth, new_followers)
    all_apps <- c(targets, origin_apps)
    
    final <- split_edge_node(all_depth, all_apps)
    }
    if(graph == T){
      final_graph <- tbl_graph(nodes = rename(final$app_nodes, name = links, app_name = name), edges = final$app_edges, directed = FALSE)
      
      return(final_graph)
    } else{
      return(final)
    }
}

ifood <- "https://play.google.com/store/apps/details?id=br.com.brainweb.ifood"
melivre <- "https://play.google.com/store/apps/details?id=com.mercadolibre&gl=US"

teste_depth <- crawl_app(melivre, depth = 2, graph = T)
teste_2 <- crawl_app(clean_scrape[1], depth = 2)


teste_depth %N>%
  mutate(degree = centrality_degree(),
         big_label = ifelse(degree < 5, "", app_name)) %>%
  ggraph(layout = 'kk') +
  geom_edge_link() +
  geom_node_point(aes(size = degree, color = degree)) +
  geom_node_text(aes(label = big_label))





