####################################################
## Exploring
####################################################

source("R/package.R")
source("R/help.R")

groups <- vroom("data/rct_locations.csv") %>% glimpse()
values <- vroom("data/opa_properties_public.csv") %>% glimpse()

points <- st_as_sf(groups, coords = c("longitude_x", "latitude_y"), crs = 4326)

## 

isntna <- function(x) sum(is.na(x)) != length(x)

recent <- 
  values %>% 
  filter(sale_date > as_date("2010-01-01")) %>% 
  filter(sale_price > 1000 & sale_price < 10 * 10^6) %>%
  filter(number_of_bedrooms > 0) %>%
  drop_na(lng, lat) %>%
  select(-assessment_date, -date_exterior_condition, -beginning_point, 
         -book_and_page, -cross_reference, -owner_1, -owner_2,
         -unit, -suffix, -street_direction, -fuel, -quality_grade) %>%
  select_if(isntna) %>%
  select_if(!str_detect(colnames(.), "mail|code")) %>%
  glimpse()

##

shapes <- st_as_sf(recent, coords = c("lng", "lat"), crs = 4326)

tmap_mode("plot")

map <- tm_shape(shapes %>%
           mutate(`log price (per sf)` = log(sale_price / total_livable_area))) +
  tm_dots(col = "log price (per sf)",
          legend.hist = TRUE,
          pal = pal, 
          midpoint = NA) +
  tm_shape(points) +
  tm_dots(col = '#000000') +
  tm_layout(main.title = "Philadelphia Properties (with sites overlaid)",
            legend.outside = TRUE)

tmap_save(map, filename = "properties.png", height = 6, width = 8, dpi = 300)

##



