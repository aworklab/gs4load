#' 기본 함수를 가져 옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A mat
#' @export
str2dt = function(df){

  # Text Filter

  dt_str = '^\\d{4}-\\d{2}-\\d{2}$'
  ts_str = '^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$'


  # 무작위 샘플링 (NA 피하기)

  idx_cur = min(nrow(df), 1000)
  chk = df[sample(1:idx_cur, idx_cur, replace = T), ] %>% as_tibble()

  lapply(chk, function(x){sum(is.na(x))} ) -> na_chk

  tibble(col = names(na_chk), na_cnt = as.integer(na_chk)) %>%
    filter(na_cnt > 0 & na_cnt != idx_cur)-> na_chk

  if(nrow(na_chk) != 0){ # na_chk가 비어 있지 않을 경우

    for(i in 1:nrow(na_chk)){

      col_is = na_chk$col[i]
      txt = glue('chk${col_is} = zoo::na.locf(chk${col_is}, na.rm = F, fromLast=T)')
      eval(parse(text = txt))
      txt = glue('chk${col_is} = zoo::na.locf(chk${col_is}, na.rm = F, fromLast=F)')
      eval(parse(text = txt))

    }

  }


  # Date, TS를 자동 변환

  chk = tibble(col = names(chk[1, ]), value = c(chk[1, ]))

  chk %>%
    mutate(flag = map_chr(value,
                          ~{
                            case_when(
                              str_detect(as.character(.x), dt_str) ~ 'dt',
                              str_detect(.x, ts_str) ~ 'ts',
                              T ~ NA_character_
                            ) -> res
                            return(res)
                          })) %>%
    select(-value) %>%
    na.omit() -> chk

  if(nrow(chk)!=0){

    for(i in 1:nrow(chk)){

      work_info = chk[i, ]
      work_col = work_info$col
      work_flag = work_info$flag
      converter = ifelse(work_flag == 'ts', 'ymd_hms','ymd')
      txt = glue('df${work_col} = {converter}(df${work_col})')
      eval(parse(text = txt))

      cat(magenta('>')
          ,yellow(work_col),'is converted to',blue(work_flag),'\n')

    } # for 문 종료

  } # if문 종료

  str(df)
  return(df)

}
