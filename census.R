library(tidyverse)
library(readr)

years = formatC(12:14, width = 2, flag = "0")
for (year in years){
  stem = paste0("CB", year, "00A11")
  if(!file.exists(paste0(stem, ".dat"))){
    download.file(paste0("http://www2.census.gov/econ20", year, "/CB/sector00/", 
                         stem, ".zip"), "CB1400A11.zip")
    unzip("CB1400A11.zip")
  }
}

col_types = cols(
  .default = col_character(),
  YEAR = col_integer(),
  ESTAB = col_integer(),
  EMP = col_integer(),
  EMP_N = col_integer(),
  PAYQTR1 = col_integer(),
  PAYQTR1_N = col_integer(),
  PAYANN = col_double(),
  PAYANN_N = col_integer()
)

raw_CB_data = dir(pattern = ".dat") %>% 
  map(read_delim, delim = "|", col_types = col_types) %>% 
  bind_rows()
  
  spread_data = raw_CB_data %>% 
  filter(nchar(as.character(NAICS2012)) == 3) %>% 
  filter(COUNTY %in% 2:998) %>% 
  distinct(GEO_ID, GEO_TTL, NAICS2012_TTL, EMP, YEAR) %>% 
  group_by(YEAR) %>% 
  by_slice(~spread(.x, NAICS2012_TTL, EMP, fill = 0), .collate = "rows")


counts = spread_data %>% 
  group_by() %>% 
  select(-starts_with("GEO")) %>% 
  filter(rowSums(.) > 0) %>% 
  select(which(colSums(.) > 0))


reshape2::melt(cor(as.matrix(counts) / rowSums(counts)) * upper.tri(cor(counts))) %>% 
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + 
  scale_fill_gradient2() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
