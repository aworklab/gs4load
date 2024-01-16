#' 기본 함수를 가져 옴
#'
#' 테스트 해볼 것
#'
#' @param infile Path to the input file
#' @return A mat
#' @export
create_schema = function(type_is = 's3', table_is){

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
