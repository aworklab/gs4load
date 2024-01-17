#' 기본 함수를 가져 옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A mat
#' @export
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
  target_create = code_gs$work_create %>% filter(action == type_is)
  # target_create = gs_loader('create','META') %>% filter(action == type_is)

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
      mutate(num = row_number(),.before = col_name) %>%
      mutate(fit = case_when(
        num == 1 ~ sprintf("\t`%s` %s\n", col_name, col_type),
        num == max(num) ~ sprintf("`%s` %s", col_name, col_type),
        T ~ sprintf("`%s` %s\n", col_name, col_type)
      )) %>%
      pull(fit) %>% paste0(collapse = '\t, ') -> columns_are

  } else if(type_is %in% c('add_part','drop_part')) {

    year = year(today())
    month = month(today())


  } else if(type_is %in% c('select_part')) {


  } else if(type_is %in% c('cdc_select')) {

    target_col %>%
      filter(table_name == table_is) %>%
      select(col_name) %>%
      bind_rows(
        tibble(col_name = c('year','month','day'))
      ) %>%
      mutate(num = row_number(), .before = col_name) %>%
      mutate(col = case_when(
        col_name == 'division_date' ~ '\tDATE(DATE_ADD(reg_date, INTERVAL {info.time_difference} hour)) division_date\n',
        col_name == 'reg_dt' ~ 'DATE_ADD(reg_date, INTERVAL {info.time_difference} hour) reg_dt\n',
        col_name == 'mig_dt' ~ 'DATE_ADD(now(), INTERVAL {info.time_difference} hour) mig_dt\n',
        col_name == 'year' ~ 'YEAR(DATE_ADD(reg_date, INTERVAL {info.time_difference} hour)) year\n',
        col_name == 'month' ~ 'MONTH(DATE_ADD(reg_date, INTERVAL {info.time_difference} hour)) month\n',
        col_name == 'day' ~ 'DAY(DATE_ADD(reg_date, INTERVAL {info.time_difference} hour)) day',
        T ~ sprintf('%s\n',col_name)
      )) %>% pull(col) %>% paste0(collapse = '\t, ') -> columns_are

    ts_limit_is = target_s3$ts_limit

  }

  # 쿼리 생성
  query_create = glue(target_create$query_is, .open = '<<',.close = '>>')


  # Clipbaord로 붙여 넣기
  suppressWarnings(
    write.table(query_create, 'clipboard', sep = '\t', row.names = F, col.names = F, quote = F)
  )
  cat(green('√', blue(type_is),red(table_is), 'coppied to clipbaord.\n'))

  return(query_create)

}
