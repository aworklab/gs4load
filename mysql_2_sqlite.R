# remotes::install_github('aworklab/gs4load', upgrade = c('never'), force = T)
# remotes::install_github('aworklab/gs4load')

pacman::p_load(gs4load)
get_lib()

# 0. db_info --------------------------------------------------------------

db_info = gs_loader('bs_db','META') %>% filter(com_is == get_com())

# 0. work range -----------------------------------------------------------

work_prefix = c('bk_common','bk_cstool','bk_game','bk_live')
tbl_prefix = tibble(prefix = work_prefix)


# 1. mysql 5 to sqlite ----------------------------------------------------
# ** db-schema ------------------------------------------------------------

tbl_prefix %>%
  mutate(tbl = map(prefix,
                  ~{
                    # .x = xx$prefix[[1]]

                    con_my = get_con(.x,'my')
                    list_tbl = dbListTables(con_my)
                    dbDisconnect(con_my)
                    return(list_tbl)
                  })) -> list_tbl

list_tbl = list_tbl %>% unnest(tbl)

# ** Import from mysql ----------------------------------------------------

list_tbl %>%
  mutate(fit = map2(prefix, tbl,
                    ~{
                      # i = 3
                      # .x = list_tbl$prefix[[i]]
                      # .y = list_tbl$tbl[[i]]

                      # DB 정보
                      temp_info = db_info %>% filter(prefix == .x)
                      sqlite_name = temp_info$dbname
                      schema_name = temp_info$t_name
                      table_name = .y
                      # Mysql
                      con_my = get_con(.x, 'my')
                      query = glue('select * from {schema_name}.{table_name}')
                      temp = dbGetQuery(con_my, query) %>% as.data.table()
                      dbDisconnect(con_my)
                      temp = dt2str(temp)
                      return(temp)
                    })) -> list_tbl_my


# ** Export to SQLite -----------------------------------------------------

list_tbl %>%
  mutate(export = pmap(list(.x=prefix,.y=tbl,.z=fit),
                       function(.x,.y,.z){
                         # i = 1
                         # .x = list_tbl$prefix[[i]]
                         # .y = list_tbl$tbl[[i]]
                         # .z = list_tbl$fit[[i]]

                         .x
                         .y
                         .z

                         temp_info = db_info %>% filter(prefix == .x)
                         con_q = get_con(.x, 'sql')

                         dbWriteTable(con_q, .y, .z, append = T, row.names = F)


                       }))


# 3. sqlite 2 mysql8 ------------------------------------------------------

# ** db-schema ------------------------------------------------------------

tbl_prefix %>%
  mutate(tbl = map(prefix,
                   ~{
                     .x = xx$prefix[[1]]

                     con_q = get_con(.x,'sql')
                     list_tbl = dbListTables(con_q)
                     dbDisconnect(con_q)
                     return(list_tbl)
                   })) -> list_tbl

list_tbl = list_tbl %>% unnest(tbl)

# ** Import from mysql ----------------------------------------------------

list_tbl %>%
  mutate(fit = map2(prefix, tbl,
                    ~{
                      i = 1
                      .x = list_tbl$prefix[[i]]
                      .y = list_tbl$tbl[[i]]

                      .x
                      .y

                      # DB 정보
                      temp_info = db_info %>% filter(prefix == .x)
                      sqlite_name = temp_info$dbname
                      schema_name = temp_info$t_name
                      table_name = .y
                      # Mysql
                      con_q = get_con(.x, 'sql')
                      con_my = get_con(.x, 'my')

                      query = glue('select * from {table_name}')
                      temp = dbGetQuery(con_q, query) %>% as_tibble()
                      temp = str2dt(temp)
                      temp
                      names(temp)
                      str(temp)
                      dbWriteTable(con_my,
                                   table_name,
                                   temp,
                                   field.types = list(bill_start_date = "DATETIME"),
                                   overwrite = T, row.names = F)

                      dbDisconnect(con_my)

                      return(temp)
                    })) -> list_tbl



