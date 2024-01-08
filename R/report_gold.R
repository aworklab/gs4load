#' 기본 함수를 가져 옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A mat
#' @export
report_gold = function(){

  # 기본 함수 호출

  tibble_exist = sum((.packages()) == 'tibble')

  if(tibble_exist == 0){

    gs4load::get_lib()

  }

  # clipboard 데이터 불러오기
  suppressWarnings(
    tibble(txt = readLines('clipboard'))
  ) %>% mutate(txt = trimws(txt)) -> temp


  # 데이터 세트 수 확인

  temp %>%
    mutate(flag = ifelse(str_detect(txt, 'KST'), 1, 0), .before = txt)  %>%
    mutate(flag = cumsum(flag)) %>%
    filter(flag != 0) -> temp2

  n_set = max(temp2$flag) # 데이터 세트 수
  range_date = seq.Date(today() - days(n_set), today() - 1, 'day')

  # 세트별로 정리

  txt_filter = 'KST.+(\\d{4}\\/\\d{2}\\/\\d{2}).+(\\d{2}\\:\\d{2}).+(\\d{4}\\/\\d{2}\\/\\d{2}).+(\\d{2}\\:\\d{2})$'

  temp2 %>% group_nest(flag) %>%
    mutate(fit = map2(data, flag,
                      ~{
                        # i =1
                        # .x = xx$data[[i]]
                        # .y = xx$flag[[i]]

                        .x %>% filter(str_detect(txt, 'KST')) -> raw_period
                        .x %>% filter(str_detect(txt, '\\d{4}') & !str_detect(txt,'KST|UTC')) -> raw_id

                        suppressWarnings(
                          raw_period %>%
                            separate(txt, letters[1:10], sep = '\\s') %>%
                            mutate(flag = 1, .before = a) %>%
                            pivot_longer(2:ncol(.)) %>%
                            na.omit() %>%
                            filter(str_detect(value, '\\d{4}/\\d{1,}/\\d{1,}')) %>%
                            mutate(value = parse_date_time2(value, order = '%Y/%m/%d') %>% ymd()) %>%
                            arrange(value) %>%
                            mutate(name = row_number()) %>%
                            mutate(name = case_when(
                              name == 1 ~ 'date_start',
                              name == 2 ~ 'date_end'
                            )) %>%
                            pivot_wider(names_from = name, values_from = value) %>%
                            select(-flag) %>%
                            mutate(across(c(date_start,date_end),
                                          ~{
                                            str_remove_all(.x,'-') %>% as.integer()
                                          }))
                        ) -> raw_period

                        # 프로덕트 정보
                        raw_id %>%
                          mutate(txt = str_remove_all(txt, '\t|\n|GroupID\\: ')) %>%
                          mutate(txt = str_replace_all(txt, '\\/|\\=\\>', '-')) %>%
                          separate(txt, letters[1:4], sep= ' \\- ') %>%
                          mutate(product_type = case_when(
                            str_detect(b, '골드') ~ 'gold',
                            T ~ 'coin'
                          )) %>%
                          mutate(product_price = parse_number(b)) %>%
                          select(product_type, product_id = d,
                                 product_name = a, product_price) %>%
                          mutate(date_report = str_remove_all(range_date[.y], '-') %>% as.integer(),
                                 date_start = raw_period$date_start,
                                 date_end = raw_period$date_end,
                                 .before = product_type
                          ) %>%  -> raw_id

                        # 골드 상품만인지 확인
                        gold_exclusive = !sum(raw_id$product_type == 'coin')

                        raw_id %>%
                          mutate(gold_exclusive = case_when(
                            gold_exclusive == T ~ 1,
                            T ~ 0
                          ), .after = date_end) -> raw_id

                        return(raw_id)

                      })) -> temp3

  # 그룹별  처리

  temp3 = temp3 %>% select(fit) %>% unnest(fit)

  # Sheet에 업데이트
  ##gs_update(temp3, 'report_gold','BS')

  return(temp3)

}









