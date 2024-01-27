# remotes::install_github('aworklab/gs4load', upgrade = c('never'), force = T)
pacman::p_load(gs4load)
get_lib()
type_is = 'cdc_select';table_is = 'rrs_user'
type_is = 'rs';table_is = 'rrs_user'
# 스키마 코드 읽기  --------------------------------------------------------------

create_schema('cdc_select','account')

create_schema('rs','log_login')
create_schema('add_part','log_login')
create_schema('drop_part','log_login')
create_schema('select_part','log_star')

create_schema('cdc_select','log_ccu')
create_schema('s3','log_star')
create_schema('rs','log_login')

df %>% select(table_name)

oo = gs_loader('s3_url','META')
o2 = oo %>% filter(is_active == 1)

get_wotan_insert(o2,'wotan_delabs.info_s3_cdc')

df = o2;target_name = 'wotan_delabs.info_s3_cdc'

# gmailr ------------------------------------------------------------------

# install.packages('gmailr')
# library(gmailr)
# gmailr::gm_auth(path = keyring::key_get('gs4'))
# gm_auth_configure(path = keyring::key_get('gs4'))
