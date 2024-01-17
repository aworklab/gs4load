# remotes::install_github('aworklab/gs4load', upgrade = c('never'), force = T)
pacman::p_load(gs4load)
get_lib()

# 스키마 코드 읽기  --------------------------------------------------------------

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

create_schema = function(type_is = 's3', table_is){

  # 작업 정보
  loading_sheet = tibble(
    sheet_is = c('s3_url','redshift','column_str','create'),
    assign_is = c('work_s3','work_rs','work_col','work_create')
  )

  # code_gs Env 생성 확인

  if(!('code_gs' %in% ls(envir = .GlobalEnv))){

    assign('code_gs', new.env(), envir = .GlobalEnv)

  }

  # code_gs에 존재 하지 않을 때만 작업값을 불러옴

  for(i in 1:nrow(loading_sheet)){

    load_info = loading_sheet[i, ]


    if(sum(ls(code_gs) == load_info$assign_is) == 0){

      temp_load = gs_loader(load_info$sheet_is, 'META')
      assign(load_info$assign_is, temp_load, envir = code_gs)

    } # if 문 종료

  } # for 문 종료

  target_s3 = code_gs$work_s3 %>% filter(table_name == table_is)
  target_rs = code_gs$work_rs  %>% filter(table_name == table_is)
  target_col = code_gs$work_col  %>% filter(table_name == table_is)
  target_create  = code_gs$work_create %>% filter(action == type_is)

  # 테이블 정보
  table_name = table_is
  s3_url = target_s3$s3_path

  # 테이블 생성 일 경우

  if(type_is %in% c('s3','rs')){

    # Columns 정보 생성
    target_col %>%
      filter(table_name == table_is) %>%
      select(col_name, s3_type, rs_type) %>%
      pivot_longer(2:ncol(.), names_to = 'type', values_to = 'col_type') %>%
      filter(str_detect(type, type_is)) %>%
      mutate(fit = map2_chr(col_name, col_type,
                            ~{
                              sprintf("`%s` %s\n", .x, .y)
                            })) %>%
      pull(fit) %>% paste0(collapse = ', ') -> columns_are

  } else if(type_is %in% c('add_part','drop_part')) {

    year = year(today())
    month = month(today())


  } else if(type_is %in% c('select_part')) {


  }

  # 쿼리 생성
  query_create = glue(target_create$query_is)

  return(query_create)

}


df = gs_loader('s3_url','META')
df %>% select(table_name) %>%
  mutate(type_is = 's3') %>%
  mutate(query = map2_chr(type_is,table_name,
                      ~{create_schema(.x,.y)})) -> ox

create_schema('s3','log_login')
create_schema('rs','log_login')
create_schema('add_part','log_login')
create_schema('drop_part','log_login')
create_schema('select_part','log_login')


