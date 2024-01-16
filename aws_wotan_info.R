pacman::p_load(gs4load)
get_lib()

df = read.csv('clipboard', sep = '\t') %>% as_tibble()

info_s3 = get_insert(df,'`wotan_delabs`.`info_s3_cdc`')
info_wotan = get_insert(df,'`wotan_delabs`.`info_wotan_delabs`')


info_s3
info_wotan



# scrapper  ---------------------------------------------------------------

rs = function(){

  temp = readLines('clipboard')
  temp = tibble(txt = temp)

  if(sum(str_detect(temp$txt, 'TBLPROPERTIES')) == 0){

    temp %>% filter(str_detect(txt, 'TABLE')) %>%
      mutate(txt = gsub('.+ game_s3\\.(.+) \\($','\\1', txt)) %>%
      pull(txt) -> rs_name
    temp %>% filter(str_detect(txt, '^        ')) %>%
      mutate(txt = trimws(txt) %>% str_remove(',$')) %>%
      mutate(txt = str_replace(txt, ', ', ',')) %>%
      separate(txt, c('col','type2'), sep = '\\s') %>%
      mutate(type1 = '', .before = type2)-> res

  } else {

    temp = temp %>% mutate(txt = str_remove_all(txt, '`')) %>%
      mutate(num = row_number(), .before = txt)

    max_num = which(str_detect(temp$txt, 'PARTITIONED'))

    temp = temp %>% filter(num < max_num)

    temp %>% filter(str_detect(txt, 'TABLE')) %>%
      mutate(txt = gsub('^CREATE EXTERNAL TABLE\\s(.+)\\s?\\($','\\1', txt)) %>%
      pull(txt) -> rs_name

    temp %>% filter(str_detect(txt, '^  ')) %>%
      mutate(txt = trimws(txt) %>% str_remove(',$')) %>%
      mutate(txt = str_remove(txt, '^, ')) %>%
      mutate(txt = str_replace(txt, ', ', ',')) %>%
      separate(txt, c('col','type1'), sep = '\\s{1,}') %>%
      filter(!is.na(type1)) %>%
      select(-num) -> res
  }

  print(res)
  suppressWarnings(
    write.table(res, 'clipboard', sep = '\t', row.names = F, col.names = F)
  )
  cat(green('√', red(rs_name), 'coppied to clipbaord.\n'))
  # return(res)

}

rs()


# create schema 생성기  ------------------------------------------------------

make_schema = function(type_is = 's3', table_is){

  type_is = 's3'
  table_is = 'log_login'


  # 작업 정보
  work_s3 = gs_loader('s3_url','META') %>% filter(table_name == table_is)
  work_rs = gs_loader('redshift','META')  %>% filter(table_name == table_is)
  work_col = gs_loader('column_str','META')  %>% filter(table_name == table_is)
  work_create  = gs_loader('create', 'META') %>% filter(action == type_is)

  # 테이블 정보
  table_name = table_is
  s3_url = work_s3$s3_path

  # Columns 정보 생성
  work_col %>%
    filter(table_name == table_is) %>%
    select(col_name, s3_type, rs_type) %>%
    pivot_longer(2:ncol(.), names_to = 'type', values_to = 'col_type') %>%
    filter(str_detect(type, type_is)) %>%
    mutate(fit = map2_chr(col_name, col_type,
                          ~{
                            sprintf("`%s` %s\n", .x, .y)
                          })) %>%
    pull(fit) %>% paste0(collapse = ', ') -> columns_are

  query_create = glue(work_create$query_is)

  return(query_create)

}


df = gs_loader('s3_url','META')
df %>% select(table_name) %>%
  mutate(query = map2(table_name, 's3', create_schema))
