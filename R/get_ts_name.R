#' DataFrame에서 Date, TimeStamp 형식의 Columns 이름을 찾는 함수
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A mat
#' @export
get_ts_name = function(df){

  # Text Filter

  dt_str = '^\\d{4}-\\d{2}-\\d{2}$'
  ts_str = '^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$'


  # 무작위 샘플링 (NA 피하기)

  idx_cur = min(nrow(df), 1000)
  chk = df[sample(1:idx_cur, idx_cur, replace = T), ] %>% as_tibble()

  lapply(chk, function(x){sum(is.na(x))} ) -> na_chk

  tibble(col = names(na_chk), na_cnt = as.integer(na_chk)) %>%
    filter(na_cnt > 0 & na_cnt != idx_cur)-> na_chk

  if(nrow(na_chk)!=0){

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


  return(chk)

}
