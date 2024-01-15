#' 기본 함수를 가져 옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A mat
#' @export
get_mysql_schema = function(df, table_is){

  # df = temp
  # 기본 정보 구하기
  list_df_type = do.call(c, lapply(df, function(x) typeof(x))) # DF의 colums 구함

  tbl_df = tibble(col = names(list_df_type), col_type = list_df_type) # 변환 Schema 구조

  col_chr = gs4load::get_char_name(df)

  # TS
  col_ts = gs4load::get_ts_name(df) # TS 구함

  # TS + Varchar
  col_info = bind_rows(col_chr, col_ts)
  col_info = left_join(tbl_df, col_info, 'col') %>%
    mutate(flag = coalesce(flag, col_type, flag)) %>%
    mutate(flag = case_when(
      flag == 'ts' ~ 'timestamp',
      flag == 'dt' ~ 'date',
      T ~ flag
    ))

  # table_is = table_name

  col_info %>%
    mutate(fit = map2_chr(col, flag, ~{sprintf('`%s` %s\n',.x,.y)})) %>%
    pull(fit) %>% paste0(collapse = '\t,') -> query_cols

  glue('create table {table_is} (
       \t{query_cols})
       ENGINE = InnoDB
       DEFAULT CHARACTER SET = utf8mb4
       COLLATE = utf8mb4_0900_ai_ci;
       ') -> query

  return(list(col_info = col_info, query = query))

}
