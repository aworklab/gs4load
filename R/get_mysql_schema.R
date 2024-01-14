#' 기본 함수를 가져 옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A mat
#' @export
get_mysql_schema = function(df){

  df = temp
  # 기본 정보 구하기
  list_df_type = do.call(c, lapply(df, function(x) typeof(x))) # DF의 colums 구함

  tbl_df = tibble(col = names(list_df_type),col_type = list_df_type) # 변환 Schema 구조

  # Chr -> Varchgr
  list_chr = tbl_df %>% filter(col_type == 'character')
  col_chr = df[, list_chr$col]

  min_row = min(nrow(temp_chr),1e3)
  col_chr = col_chr[sample(1:min_row, min_row), ]



  col_chr %>%
    mutate(num = 1, .before = list_chr$col[1]) %>%
    pivot_longer(2:ncol(.)) %>%
    group_nest(name) %>%
    mutate(fit = map_int(data,
                     ~{
                       max_len = .x %>%
                         mutate(len = map_int(value,
                                              ~{
                                                coalesce(.x, '', .x) %>% nchar()
                                              })) %>%
                         pull(len) %>%
                         max(na.rm = T)
                       max_len

                     })) %>%
    unnest(fit) -> col_chr

  # cut

  bind_cols(
    col_chr,
    tibble(size = cut(col_chr$fit,
                      c(-1,10,20,40,60,100,200,1000,4000,999999),
                      c(10,20,40,60,100,200,1000,4000,10000), right = T))
  ) %>%
    mutate(size = sprintf('varchar(%s)', size)) %>%
    select(col = name, flag = size) -> col_chr

  # TS
  col_ts = gs4load::get_ts_name(df) # TS 구함

  # TS + Varchar
  col_inf = bind_rows(temp_chr, col_ts)
  col_inf = left_join(tbl_df, col_inf, 'col') %>%
    mutate(flag = coalesce(flag, col_type, flag))

  return(col_inf)

}
