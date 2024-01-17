#' 기본 함수를 가져 옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A mat
#' @export
get_wotan_insert = function(df, target_name){

  # 테이블 구조
  list_df_type = do.call(c, lapply(df, function(x) typeof(x))) # DF의 colums 구함
  tbl_df = tibble::tibble(col = names(list_df_type), col_type = list_df_type) # 변환 Schema 구조

  # TS, Char 구하기
  col_ts = gs4load::get_ts_name(df)
  col_chr = gs4load::get_char_name(df)

  # 전체 구조
  col_info = bind_rows(col_chr, col_ts)
  col_info = left_join(tbl_df, col_info, 'col') %>%
    mutate(flag = coalesce(flag, col_type, flag)) %>%
    mutate(flag = case_when(
      flag == 'ts' ~ 'timestamp',
      flag == 'dt' ~ 'date',
      T ~ flag
    ))

  # Columns 명단
  col_names = paste0(col_info$col, collapse = ',')

  # Insert Value
  df %>%
    mutate(num = row_number(), .before = col_info$col[1]) %>%
    group_nest(num) %>%
    mutate(lines = map_chr(data,
                           ~{
                             # .x = xx$data[[1]]

                             # 처리 원본
                             tibble(col = names(.x),
                                    value = do.call(c, .x)) %>%
                               left_join(col_info, 'col') %>%
                               select(-flag) %>%
                               mutate(value = ifelse(col_type %in% c('character','timestamp','date'),
                                                     sprintf('"%s"',value),
                                                     value
                               )) %>%
                               mutate(keys = sprintf('%s=%s',col, value))-> raw

                             values = paste0(raw$value, collapse = ',')
                             keys = paste0(raw$keys,collapse = ',')

                             txt = glue("
                              INSERT {target_name}
                                ({col_names})
                              VALUES
                                ({values})
                              ON DUPLICATE KEY UPDATE
                              {keys};
                              ")
                             return(txt)


                           })) %>%
    pull(lines) %>%
    paste0(collapse = '\n') -> query

  glue("
       DELETE FROM {target_name};
       {query}
       ") -> query

  # Clipbaord로 붙여 넣기
  suppressWarnings(
    write.table(query, 'clipboard', sep = '\t', row.names = F, col.names = F, quote = F)
  )
  cat(green('√', red(target_name), 'coppied to clipbaord.\n'))

  return(query)

}
