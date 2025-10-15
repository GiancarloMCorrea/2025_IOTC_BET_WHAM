
# -------------------------------------------------------------------------
# Transform from SS model time step (quarter) to year-quarter:
ssts2yq = function(qtr,initial = 1950, base = 13) {
  yearqtr = (qtr-base)/4+initial+1/8
  return(yearqtr)
}


# -------------------------------------------------------------------------
# Add land map to a existing sf plot:
add_sf_map = function(my_plot) {
  
  out_plot = my_plot + 
    geom_sf(data = worldmap, fill = "gray60", color = "gray60") +
    coord_sf(expand = FALSE, xlim = xLim, ylim = yLim) +
    xlab(NULL) + ylab(NULL) +
    scale_y_continuous(breaks = yBreaks) + scale_x_continuous(breaks = xBreaks) 
  return(out_plot)
  
}


# -------------------------------------------------------------------------
# Get grid dimension (latitude and longitude) from grid type:
get_grid_lat_dim = function(grid_type) {
  
  grid_type = as.character(grid_type)
  out_val = ifelse(grid_type=='9', 30, # new classification, confirmed by Manu
                   ifelse(grid_type=='A', 10, # new classification, confirmed by Manu
                          ifelse(grid_type=='7', 10, # new classification, confirmed by Manu
                                 ifelse(grid_type=='8', 20, # new classification, confirmed by Manu
                                        ifelse(grid_type=='5', 1, 
                                               ifelse(grid_type=='6', 5, 0)))))) # Type 5 (dim = 1) and 6 (dim = 5)
  return(out_val)
  
}
get_grid_lon_dim = function(grid_type) {
  
  grid_type = as.character(grid_type)
  out_val = ifelse(grid_type=='9', 30, # new classification, confirmed by Manu
                   ifelse(grid_type=='A', 20, # new classification, confirmed by Manu
                          ifelse(grid_type=='7', 10, # new classification, confirmed by Manu
                                 ifelse(grid_type=='8', 20, # new classification, confirmed by Manu
                                        ifelse(grid_type=='5', 1, 
                                               ifelse(grid_type=='6', 5, 0)))))) # Type 5 (dim = 1) and 6 (dim = 5)
  return(out_val)
  
}


# -------------------------------------------------------------------------
# Get longitude and latitude center from Grid (Dan's function)

get.lat.from.id = function(id) {
  id = as.character(id)
  size_grid_ = substr(id,1,1)
  
  size_lat_ = get_grid_lat_dim(size_grid_)
  if(size_lat_ == 0) stop("Unrecognized grid type. Check your grid codes.")
  
  quadrant =substr(id,2,2)
  lat_label  = substr(id,3,4)
  lat = as.numeric(lat_label)+size_lat_/2
  lat = ifelse(quadrant == 1  | quadrant == 4, lat,-lat)			
}

get.long.from.id = function(id) {
  id = as.character(id)
  size_grid_ = substr(id,1,1)
  
  size_long_ = get_grid_lon_dim(size_grid_)
  if(size_long_ == 0) stop("Unrecognized grid type. Check your grid codes.")
  
  quadrant = substr(id,2,2)
  long_label  = substr(id,5,7)
  long = as.numeric(long_label)+size_long_/2
  long = ifelse(quadrant == 1  | quadrant == 2,long, -long+360)
}

# -------------------------------------------------------------------------
# Get 1A 'Area' column based on longitude and latitude:
get_1Aarea_from_lonlat = function(Long, Lat) {
  # Areas: 1 (1a), 2 (1b)
  out_area =  ifelse(Lat > 10 & Long <= 75, 1, # originally, it was Long < 75, but produced unassigned areas for GI after GridCell correction (Type 1 30x30)
                     ifelse((Lat > -10 & Lat < 10 & Long  < 60) | (Lat > -15 & Lat < 10 & Long  > 60 & Long <= 75), 1, # originally, it was Long < 75, but produced unassigned areas for GI after GridCell correction (Type 1 30x30)
                            ifelse((Lat > -60 & Lat < -10 & Long > 20 & Long < 40) | (Lat > -30 & Lat < -10 & Long > 40 & Long  < 60), 1,
                                   ifelse((Lat > -60 & Lat < -30 & Long > 40 & Long < 60) | (Lat > -60 & Lat <= -15 & Long  > 60 & Long <= 160), 1, # originally, the max long was 150, but new data std had data until long 160. 
                                          ifelse(Lat > -15 & Long > 75 & Long < 150, 1, 0)))))		
  return(out_area)
  
}


# -------------------------------------------------------------------------
# Create 4A AssessmentArea and ModelArea columns from 'Area' column:
create_4Aarea_cols = function(Data) {
  
  Data = plyr::ddply(Data,c("Area","FisheryCode"),.fun = function(d) {
    d$AssessmentArea = d$Area
    d = mutate_cond(d,FisheryCode=='FS' & Area ==1, AssessmentArea = 2)
    d = mutate_cond(d,FisheryCode=='FS' & Area ==4, AssessmentArea = 5)
    d = mutate_cond(d,FisheryCode=='LS' & Area ==1, AssessmentArea = 2)
    d = mutate_cond(d,FisheryCode=='LS' & Area ==4, AssessmentArea = 5)
    d = mutate_cond(d,FisheryCode=='LF', AssessmentArea = 5)
    d = mutate_cond(d,FisheryCode=='BB', AssessmentArea = 2)
    d = mutate_cond(d,FisheryCode=='GI' & (Area == 2 | Area==3), AssessmentArea = 1)
    d = mutate_cond(d,FisheryCode=='GI' & Area == 4, AssessmentArea = 5)
    d = mutate_cond(d,FisheryCode=='HD', AssessmentArea = 1)
    d = mutate_cond(d,FisheryCode=='TR' & Area == 1, AssessmentArea = 2)
    d = mutate_cond(d,FisheryCode=='TR' & Area == 4, AssessmentArea = 5)
    d = mutate_cond(d,FisheryCode=='OT' & (Area == 2 | Area==3), AssessmentArea = 1)
    d = mutate_cond(d,FisheryCode=='OT' & Area == 4, AssessmentArea = 5)
    return(d)})	
  
  Data = plyr::ddply(Data,c("Area","FisheryCode"),.fun = function(d) {
    d$AssessmentAreaName = d$AssessmentArea
    d$ModelArea = d$AssessmentArea
    d = mutate_cond(d,AssessmentArea=='1',AssessmentAreaName = '1a')
    d = mutate_cond(d,AssessmentArea=='2',AssessmentAreaName = '1b')
    d = mutate_cond(d,AssessmentArea=='3',AssessmentAreaName = '2')
    d = mutate_cond(d,AssessmentArea=='4',AssessmentAreaName = '3')
    d = mutate_cond(d,AssessmentArea=='5',AssessmentAreaName = '4')
    d = mutate_cond(d,AssessmentArea=='1',ModelArea = '1')
    d = mutate_cond(d,AssessmentArea=='2',ModelArea = '1')
    d = mutate_cond(d,AssessmentArea=='3',ModelArea = '2')
    d = mutate_cond(d,AssessmentArea=='4',ModelArea = '3')
    d = mutate_cond(d,AssessmentArea=='5',ModelArea = '4')	
    return(d)})
  
  return(Data)
  
}

# -------------------------------------------------------------------------
# Create 2A AssessmentArea and ModelArea columns from 'Area' column:
create_2Aarea_cols = function(Data) {
  
  Data = plyr::ddply(Data,c("Area","FisheryCode"),.fun = function(d) {
    d$AssessmentArea = d$Area
    d = mutate_cond(d,FisheryCode=='FS' & Area ==1, AssessmentArea = 2)
    d = mutate_cond(d,FisheryCode=='LS' & Area ==1, AssessmentArea = 2)
    d = mutate_cond(d,FisheryCode=='LF', AssessmentArea = 3)
    d = mutate_cond(d,FisheryCode=='BB', AssessmentArea = 2)
    d = mutate_cond(d,FisheryCode=='GI' & Area == 2, AssessmentArea = 1)
    d = mutate_cond(d,FisheryCode=='HD', AssessmentArea = 1)
    d = mutate_cond(d,FisheryCode=='TR' & Area == 1, AssessmentArea = 2)
    d = mutate_cond(d,FisheryCode=='OT' & (Area == 2), AssessmentArea = 1)
    return(d)})	
  
  Data = plyr::ddply(Data,c("Area","FisheryCode"),.fun = function(d) {
    d$AssessmentAreaName = d$AssessmentArea
    d$ModelArea = d$AssessmentArea
    d = mutate_cond(d,AssessmentArea=='1',AssessmentAreaName = '1a')
    d = mutate_cond(d,AssessmentArea=='2',AssessmentAreaName = '1b')
    d = mutate_cond(d,AssessmentArea=='3',AssessmentAreaName = '2')
    d = mutate_cond(d,AssessmentArea=='1',ModelArea = '1')
    d = mutate_cond(d,AssessmentArea=='2',ModelArea = '1')
    d = mutate_cond(d,AssessmentArea=='3',ModelArea = '2')
    return(d)})
  
  return(Data)
  
}

# -------------------------------------------------------------------------
# Several functions relevant for data preparation

yearqtr2qtr = function(year,qtr,initial,base) {
  qtr = (base-1)+4*(year-initial)+qtr
}

qtr2yearqtr = function(qtr,initial,base) {
  yearqtr = (qtr-base)/4+initial+1/8
}
yearqtr2numeric = function(year, qtr) {
  yearqtr = year + (qtr-1)/4+1/8
}

month2qrt = function(month) {
  qrt = floor((month-1)/3) + 1
}
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
Paste <- function (..., sep = "")  paste(..., sep = sep)
Sum <- function (..., na.rm = T)  sum(..., na.rm = na.rm)


# -------------------------------------------------------------------------
# Filter 4A LF data for SS input:
filter_LF_4A_type1 = function(data) { # used in 2021 assessment

  work = data %>%
    dplyr::filter(!(Fleet %in% c('TWN','SYC') & Gear == 'LL')) %>%
    dplyr::filter(!(ModelFishery == "LL_1a" & Year %in% c(1970:1995, 2010:2020))) %>% # please check if 2020-2022 data needs to be excluded
    dplyr::filter(!(ModelFishery == "LL_1b" & Year %in% c(1950:1959))) %>%
    dplyr::filter(!(ModelFishery == "LL_2" & Year %in% c(1950:1959))) %>%
    dplyr::filter(!(ModelFishery == "LL_3" & Year %in% c(1950:1959))) %>%
    dplyr::filter(!(ModelFishery == "LL_4" & Year %in% c(1950:1959,2001:2005,2015,2019))) %>%  # please check if 2020-2022 data needs to be excluded
    dplyr::filter(!(ModelFishery == "LF_4" & Year < 2005)) %>% # confirmed by Agurtzane
    dplyr::filter(!(ModelFishery == "GI_4" & Year %in% c(1975:1987))) %>%
    dplyr::filter(!(ModelFishery == "HD_1a" & Year %in% c(1950:2007))) %>%
    dplyr::filter(!(ModelFishery == "OT_4" & Year %in% c(1983,2016))) %>%
    dplyr::filter(!(ModelFishery == "TR_4" & Year %in% c(2016:2019))) %>%  # please check if 2020-2022 data needs to be excluded
    dplyr::filter(!(ModelFishery == "TR_1b")) %>%
    dplyr::filter(!(ModelFishery == "TR_2"))

  return(work)

}

filter_LF_4A_type2 = function(data, filter_nsamp = TRUE) { # new filtering
  
  work = data %>% 
    # Filters reviewed by Simon and agreed by the team:
    dplyr::filter(!(Fleet %in% c('TWN','SYC') & Gear == 'LL')) %>%
    dplyr::filter(!(ModelFishery == "LL_1a" & Year %in% c(1970:1995, 2010:2023))) %>% 
    dplyr::filter(!(ModelFishery == "LL_1b" & Year %in% c(1950:1959))) %>%
    dplyr::filter(!(ModelFishery == "LL_2" & Year %in% c(1950:1959))) %>%
    dplyr::filter(!(ModelFishery == "LL_3" & Year %in% c(1950:1959))) %>%
    dplyr::filter(!(ModelFishery == "LL_4" & Year %in% c(1950:1959,2001:2005,2015,2019))) %>%
    dplyr::filter(!(ModelFishery == "LF_4" & Year < 2005)) %>% # confirmed by Agurtzane
    dplyr::filter(!(Fleet %in% c('SYC') & Gear == 'ELL' & Year %in% 2003:2019)) %>% # temporarily, Manu will fix this in the raw data
    # Remove weird patterns:
    dplyr::filter(!(ModelFishery == "GI_1a" & Fleet == 'LKA' & Year %in% c(2021))) %>%
    dplyr::filter(!(ModelFishery == "HD_1a" & Fleet != 'MDV')) %>% # only use MDV for HD 1a, agreed with team
    dplyr::filter(!(ModelFishery == "HD_1a" & Fleet == 'MDV' & Year == 2003)) %>% # remove HD with noisy obs
    dplyr::filter(!(ModelFishery == "HD_1a" & Fleet == 'MDV' & Year == 2015 & Quarter == 1)) %>% # remove HD with noisy obs
    dplyr::filter(!(ModelFishery == "OT_1a" & Year %in% c(2021:2022))) %>%
    dplyr::filter(!(ModelFishery == "OT_4" & Year %in% c(2016))) %>%
    dplyr::filter(!(ModelFishery == "TR_4" & Year %in% c(2016:2019))) %>%
    dplyr::filter(!(ModelFishery == "TR_1b")) %>%
    dplyr::filter(!(ModelFishery == "TR_2"))
    # Remove rows with less than 100 Nfish sampled:
    if(filter_nsamp) work = work %>% dplyr::filter(!(Nfish_samp < 100 & Quality > 0)) # remove low sample size but only those rows not considered best quality

  return(work)
  
}

# -------------------------------------------------------------------------
# Filter 2A LF data for SS input:
# Check this function later
filter_LF_2A = function(data) {
  
  work = data %>% 
    dplyr::filter(!(Fleet %in% c('TWN','SYC') & Gear == 'LL')) %>%
    dplyr::filter(!(ModelFishery == "LL_1a" & Year %in% c(1970:1995, 2010:2023))) %>% # please check if 2020-2022 data needs to be excluded
    dplyr::filter(!(ModelFishery == "LL_1b" & Year %in% c(1950:1959))) %>%
    dplyr::filter(!(ModelFishery == "LL_2" & Year %in% c(1950:1959))) %>%
    dplyr::filter(!(ModelFishery == "LF_2" & Year < 2005)) %>% # confirmed by Agurtzane
    dplyr::filter(!(Fleet %in% c('SYC') & Gear == 'ELL' & Year %in% 2003:2019)) %>% # temporarily, Manu will fix this in the raw data
    # Remove rows with less than 100 Nfish sampled:
    dplyr::filter(!(Nfish_samp < 100 & Quality > 0)) %>% # remove low sample size but only those rows not considered best quality
    # Remove weird patterns:
    dplyr::filter(!(ModelFishery == "GI_1a" & Fleet == 'LKA' & Year %in% c(2021))) %>%    
    dplyr::filter(!(ModelFishery == "HD_1a" & Fleet != 'MDV')) %>% # only use MDV for HD 1a, agreed with team
    dplyr::filter(!(ModelFishery == "HD_1a" & Fleet == 'MDV' & Year == 2003)) %>% # remove HD with noisy obs
    dplyr::filter(!(ModelFishery == "HD_1a" & Fleet == 'MDV' & Year == 2015 & Quarter == 1)) %>% # remove HD with noisy obs
    dplyr::filter(!(ModelFishery == "OT_1a" & Year %in% c(2021:2022))) %>%
    dplyr::filter(!(ModelFishery == "OT_2" & Year %in% c(2016))) %>%
    dplyr::filter(!(ModelFishery == "TR_2" & Year %in% c(2016:2019))) %>%
    dplyr::filter(!(ModelFishery == "TR_1b")) 
  
  return(work)
  
}

# -------------------------------------------------------------------------
# Filter 1A LF data for SS input:
# Check this function later
filter_LF_1A = function(data) {
  
  work = data %>% 
    # Filters reviewed by Simon and agreed by the team:
    dplyr::filter(!(Fleet %in% c('TWN','SYC') & Gear == 'LL')) %>%
    dplyr::filter(!(ModelFishery == "LL_1a" & Year %in% c(1970:1995, 2010:2023))) %>% 
    dplyr::filter(!(ModelFishery == "LL_1b" & Year %in% c(1950:1959))) %>%
    dplyr::filter(!(ModelFishery == "LF_1b" & Year < 2005)) %>% # confirmed by Agurtzane
    dplyr::filter(!(Fleet %in% c('SYC') & Gear == 'ELL' & Year %in% 2003:2019)) %>% # temporarily, Manu will fix this in the raw data
    # Remove rows with less than 100 Nfish sampled:
    dplyr::filter(!(Nfish_samp < 100 & Quality > 0)) %>% # remove low sample size but only those rows not considered best quality
    # Remove weird patterns:
    dplyr::filter(!(ModelFishery == "GI_1a" & Fleet == 'LKA' & Year %in% c(2021))) %>%
    dplyr::filter(!(ModelFishery == "HD_1a" & Fleet != 'MDV')) %>% # only use MDV for HD 1a, agreed with team
    dplyr::filter(!(ModelFishery == "HD_1a" & Fleet == 'MDV' & Year == 2003)) %>% # remove HD with noisy obs
    dplyr::filter(!(ModelFishery == "HD_1a" & Fleet == 'MDV' & Year == 2015 & Quarter == 1)) %>% # remove HD with noisy obs
    dplyr::filter(!(ModelFishery == "OT_1a" & Year %in% c(2021:2022))) %>%
    dplyr::filter(!(ModelFishery == "OT_1b" & Year %in% c(2016))) %>%
    dplyr::filter(!(ModelFishery == "TR_1b" & Year %in% c(2016:2019)))
  
  return(work)
  
}

# -------------------------------------------------------------------------
# Repeat rows to create std Grid:

transform_to_stdgrid = function(df, std_res = 5) { # input will be a data.frame with single row
  
  # - do nothing with grid_type == 5 (1x1) or 6 (5x5)
  # - catch data have all grid_type == 6
  # - when grid_type == 5, information inside the grid will be combined without weighting
  if(df$grid_type %in% c('5', '6')) { # Do nothing
    out_df = df
  } else {
    lat_res = get_grid_lat_dim(df$grid_type)
    long_res = get_grid_lon_dim(df$grid_type)
    lat_range = c(df$lat - lat_res*0.5 + std_res*0.5, df$lat + lat_res*0.5 - std_res*0.5)
    long_range = c(df$long - long_res*0.5 + std_res*0.5, df$long + long_res*0.5 - std_res*0.5)
    # lower left corner:
    # min_lat_grid = min(lat_range) - std_res*0.5
    # min_lon_grid = min(long_range) - std_res*0.5
    # These are centroids of new grids:
    tmp_grid = expand.grid(long = seq(from = long_range[1], to = long_range[2], by = std_res),
                           lat = seq(from = lat_range[1], to = lat_range[2], by = std_res))
    # # Now let's find if some grids are completely on land. If so, exclude them:
    # # Create grid sf object:
    # tmp_points = tmp_grid %>% st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
    # tmp_grid_sf = st_make_grid(tmp_points, cellsize = c(std_res, std_res), offset = c(min_lon_grid, min_lat_grid)) %>%
    #                   st_set_crs(4326) %>% st_sf() %>% dplyr::mutate(grid_ID = 1:n())
    # # Calculate area on land:
    # tmp_grid_sf_2 = tmp_grid_sf %>% group_split(grid_ID) %>%
    #   purrr::map(~ calculate_area_on_land(.x)) %>%
    #   list_rbind() %>% select(-geometry)
    # tmp_grid_sf = left_join(tmp_grid_sf, tmp_grid_sf_2, by = 'grid_ID')
    # tmp_point_sf = st_centroid(tmp_grid_sf) %>%
    #                   dplyr::mutate(long = round(sf::st_coordinates(.)[,1], 1),
    #                                 lat = round(sf::st_coordinates(.)[,2], 1))
    # st_geometry(tmp_point_sf) = NULL
    # # Filter grid 99.9% on land
    # tmp_point_sf = tmp_point_sf %>% dplyr::filter(portion_on_land < 0.999)
    tmp_point_sf = tmp_grid
    # Continue code:
    n_rep = nrow(tmp_point_sf)
    out_df = df %>% dplyr::slice(rep(1:n(), each = n_rep))
    # # Divide sno and len freq by number of rows
    # if(len_df) {
    #   out_df = out_df %>% mutate(sno = sno/n_rep)
    #   out_df = out_df %>% mutate(across(l010:l198 , ~ ./n_rep))
    # }
    # Replace long lat values:
    out_df$long = tmp_point_sf$long
    out_df$lat = tmp_point_sf$lat
  }
  
  return(out_df)
}

# -------------------------------------------------------------------------
# Correct sno and size composition for extrapolated grids (grid type = 1:4)
correct_size_comp = function(df) {
  
  n_rep = nrow(df)
  out_df = df %>% mutate(sno = sno/n_rep)
  out_df = out_df %>% mutate(across(l010:l198 , ~ ./n_rep))
  return(out_df)
}


# -------------------------------------------------------------------------
# Calculate area on land for a sf grid object (i.e., one row):
# Create three new columns to the data.frame:
# - grid_area (km)
# - area_on_land (km): area of grid on land
# - portion_on_land: portion of grid on land
calculate_area_on_land = function(dat) {
  
  library(rgeos)
  library(rnaturalearth)
  world_map = rnaturalearth::ne_countries(scale = 'small', returnclass = c("sf"))
  wm = as(world_map, "Spatial")
  cs = gUnaryUnion(wm, id=as.character(wm$continent))
  cs_sf = st_as_sf(cs)
  inter_grid = st_intersection(cs_sf, dat)
  if(nrow(inter_grid) > 0) {
    area_on_land = sum(as.numeric(st_area(inter_grid)))*1e-06 # in km2
  } else { area_on_land = 0 }
  
  # Calculate grid area and proportion on land:
  dat = dat %>% mutate(area_on_land = area_on_land,
                       grid_area = as.numeric(st_area(dat))*1e-06, # in km2
                       portion_on_land = round(area_on_land/grid_area, digits = 3))

  return(dat)
  
}

# -------------------------------------------------------------------------
# Mean length at age (Jan-1st) by growth function (Fonteneau or Farley)
# This should come from the SS3 model, and are used in the tagging data ss3 input
# Note that information from age 1 to 28 is provided:
Len_font = c(23.821,34.9831,43.1626,46.936,49.9479,52.8725,58.2097,68.1843,77.6614,87.5578,98.6306,106.399,114.182,121.998,
        125.114,127.895,130.377,132.592,134.569,136.334,137.908,139.314,140.568,141.688,142.687,143.578,144.374,146.017)
Len_farl = c(29.94634,  44.53486,  51.23907,  59.87039,  69.86614,  78.93331,  87.15816,  94.61894, 101.38664,
         107.52563, 113.09432, 118.14569, 122.72780, 126.88425, 130.65457, 134.07464, 137.17699, 139.99114,
         142.54386, 144.85944, 146.95991, 148.86525, 150.59359, 152.16137, 153.58350, 154.87352, 156.04371,
         157.10518)


# -------------------------------------------------------------------------
# Specify scaling factors for LL CPUE scaling (see Hoyle and Langley 2020, 10.1016/j.fishres.2020.105586)
betwts = list('7994 m8' = c(0.799, 1, 0.373+0.486, 0.626))
names(betwts[['7994 m8']]) = c('R1s', 'R2', 'R3', 'R1n')


# -------------------------------------------------------------------------
# Age slicing for tagging data:
age_slicing = function(len, mlen_at_age, mlen_at_age0 = 10) {
    LenMid = c(mlen_at_age0, (mlen_at_age[-length(mlen_at_age)] + (mlen_at_age[-1] - mlen_at_age[-length(mlen_at_age)])/2))
	l = len
	age = 0
	for(i in 1:(length(LenMid)-1)) age <- ifelse(l >= LenMid[i] & l < LenMid[i+1], i, age)
	if(l >= LenMid[length(LenMid)]) age = length(LenMid) 
	return(age)
}

# -------------------------------------------------------------------------
# Correct CPUE: effort creep
apply_eff_creep = function(data, yr_col = 'yr', fleet_col = 'fleet', 
                           cpue_col = 'col', cv_col = 'cv', rate = 0.01) { # annual rate
  tmp_dat <- as.data.frame(data)
  tmp_dat = tmp_dat[,c(yr_col, fleet_col, cpue_col, cv_col)]
  tmp_dat = tmp_dat[order(tmp_dat[,2], tmp_dat[,1]), ]
  out_dat = by(tmp_dat, tmp_dat[,2], function(x) {
    xb = numeric(nrow(x))
    xb[1] = x[1,3]
    for(i in 2:nrow(x)) {
      xb[i] = x[i,3]*(1-rate)^((i-1)/4)
    }
    x[,3] = xb
    return(x)
  })
  out_dat = do.call(rbind, out_dat)
  rownames(out_dat) = 1:nrow(out_dat)
  return(out_dat)
}


# -------------------------------------------------------------------------
# Fishery definitions:
get_fisheries = function(config = '4A_io') {
  if(!(config %in% c('4A_io', '2A_io', '1A_io'))) stop('Spatial configuration is not properly defined.')
  if(config == '4A_io') {
    # Fishery 1   Gillnet (GI 1a)                              [region 1] 1
    # Fishery 2   Handline (HD 1a)                             [region 1] 1
    # Fishery 3   Longline (LL 1a)                             [region 1] 1 
    # Fishery 4   Other (OT 1a)                                [region 1] 1
    # Fishery 5   Baitboat (BB 1b)                             [region 2] 1
    # Fishery 6   Purse-seine - free schools (FS 1b)           [region 2] 1
    # Fishery 7   Longline (LL 1b)                             [region 2] 1
    # Fishery 8   Purse-seine - log schools (LS 1b)           [region 2] 1
    # Fishery 9   Troll (TR 1b)                               [region 2] 1 
    # Fishery 10  Longline (LL 2)                             [region 3] 2 
    # Fishery 11  Longline (LL 3)                             [region 4] 3
    # Fishery 12  Gillnet (GI 4)                              [region 5] 4
    # Fishery 13  Longline (LL 4)                             [region 5] 4
    # Fishery 14  Other (OT 4)                                [region 5] 4
    # Fishery 15  Troll (TR 4)                                [region 5] 4
    # Fishery 16  Purse-seine - free schools (FS 2)           [region 3] 2
    # Fishery 17  Purse-seine - log schools (LS 2)            [region 3] 2
    # Fishery 18  Troll (TR 2)                                [region 3] 2
    # Fishery 19  Purse-seine - free schools (FS 4)           [region 5] 4
    # Fishery 20  Purse-seine - log schools (LS 4)            [region 5] 4
    # Fishery 21  Longline - fresh tuna (FL 4)                [region 5] 4
    fish_df = data.frame(fleet_name = c('GI_1a','HD_1a','LL_1a','OT_1a','BB_1b','FS_1b','LL_1b','LS_1b','TR_1b','LL_2',
                                        'LL_3','GI_4','LL_4','OT_4','TR_4','FS_2','LS_2','TR_2','FS_4','LS_4','LF_4'))
  }
  if(config == '2A_io') {
    # Fishery 1   Gillnet (GI 1a)                              [region 1] 1
    # Fishery 2   Handline (HD 1a)                             [region 1] 1
    # Fishery 3   Longline (LL 1a)                             [region 1] 1 
    # Fishery 4   Other (OT 1a)                                [region 1] 1
    # Fishery 5   Baitboat (BB 1b)                             [region 2] 1
    # Fishery 6   Purse-seine - free schools (FS 1b)           [region 2] 1
    # Fishery 7   Longline (LL 1b)                             [region 2] 1
    # Fishery 8   Purse-seine - log schools (LS 1b)           [region 2] 1
    # Fishery 9   Troll (TR 1b)                               [region 2] 1 
    # Fishery 10  Longline (LL 2)                             [region 3] 2
    # Fishery 11  Gillnet (GI 2)                              [region 3] 2
    # Fishery 12  Other (OT 2)                                [region 3] 2
    # Fishery 13  Troll (TR 2)                                [region 3] 2
    # Fishery 14  Purse-seine - free schools (FS 2)           [region 3] 2
    # Fishery 15  Purse-seine - log schools (LS 2)            [region 3] 2
    # Fishery 16  Longline - fresh tuna (LF 2)                [region 3] 2
    fish_df = data.frame(fleet_name = c('GI_1a','HD_1a','LL_1a','OT_1a','BB_1b','FS_1b','LL_1b','LS_1b','TR_1b','LL_2',
                                        'GI_2','OT_2','TR_2','FS_2','LS_2','LF_2'))
  }
  if(config == '1A_io') {
    # Fishery 1   Gillnet (GI 1a)                              [region 1] 1
    # Fishery 2   Handline (HD 1a)                             [region 1] 1
    # Fishery 3   Longline (LL 1a)                             [region 1] 1 
    # Fishery 4   Other (OT 1a)                                [region 1] 1
    # Fishery 5   Baitboat (BB 1b)                             [region 2] 1
    # Fishery 6   Purse-seine - free schools (FS 1b)           [region 2] 1
    # Fishery 7   Longline (LL 1b)                             [region 2] 1
    # Fishery 8   Purse-seine - log schools (LS 1b)           [region 2] 1
    # Fishery 9   Troll (TR 1b)                               [region 2] 1 
    # Fishery 10  Gillnet (GI 1b)                              [region 2] 1
    # Fishery 11  Other (OT 1b)                                [region 2] 1
    # Fishery 12  Longline - fresh tuna (LF 1b)                [region 2] 1
    fish_df = data.frame(fleet_name = c('GI_1a','HD_1a','LL_1a','OT_1a','BB_1b','FS_1b','LL_1b','LS_1b','TR_1b',
                                        'GI_1b','OT_1b','LF_1b'))
  }
  fish_df = fish_df %>% mutate(fleet_number = 1:nrow(fish_df), .before = fleet_name)
  return(fish_df)
}

# ---------------------
# Farley 2023 two stage vB function:
twoStage_vB = function(all_ages = 1:10, Linf_par, gamma_par, k1_par, a_0, k2_par, alpha_par) {
  
  L_a = numeric(length(all_ages))
  for(a in seq_along(all_ages)) {
    
    tau = alpha_par + (1/k2_par)*log(1 - gamma_par*(1-exp(-k1_par*alpha_par)))
    if(all_ages[a] <= alpha_par) L_a[a] = gamma_par*Linf_par*(1-exp(-k1_par*(all_ages[a]-a_0)))
    if(all_ages[a] > alpha_par) L_a[a] = Linf_par*(1-exp(-k2_par*(all_ages[a]-a_0-tau)))
    
  }
  
  return(L_a)
}


# -------------------------------------------------------------------------
# Maturity function in Zudaire 2022:
maturity_Zudaire = function(len_vec, alpha = -9.25, beta = 0.091) {
  mat = exp(alpha + beta*len_vec)/(1+exp(alpha + beta*len_vec))
  return(mat)
}


# -------------------------------------------------------------------------
# K vector to approximate Farley et al 2023 in SS3
k_vec_Farley = c(0.48, 0.467, 1.376, 1.263, rep(1, times = 9))


# -------------------------------------------------------------------------
# Several growth-related functions:

growth2stage.f<- function(age, param)
{
  Linf <- param[1]
  k1 <- param[2]
  k2 <- param[3]
  astar <- param[4]
  frac <- param[5]
  a0 <- param[6]
  
  a <- age + a0
  
  # calculate a0 for second vb curve relative to a0 for first vb curve
  a02.rel.a01 <- astar+1/k2*log(1-frac*(1-exp(-k1*astar)))
  g <- a*0
  tf <- a<=astar & !is.na(a)
  g[tf] <- Linf*frac*(1-exp(-k1*a[tf]))
  tf <- a>astar & !is.na(a)
  g[tf] <- Linf*(1-exp(-k2*(a[tf] - a02.rel.a01)))
  g[is.na(a)]<- NA
  
  return(g)
}

sqdiff.2stage <- function(age, len, param) {
  exp.len <- growth2stage.f(age, param)
  sqdiff <- (len-exp.len)^2
  return(sqdiff)
}


calc.age.2stage <- function(FL, param, min.age, max.age) {
  age.est <- rep(NA,length(FL))
  for(i in 1:length(FL))
    age.est[i] <- nlminb(5, sqdiff.2stage, len=FL[i], param=param, lower=min.age, upper=max.age)$par
  return(age.est)
}


# -------------------------------------------------------------------------
#   some functions to facilitate the analysis of the outputs of ss3
#
#     summary.check- creates a data.frame with some general variables to check initial convergency issues.
#     savePlot - a default function to save plots
#     aggregate.ssMod <- read and aggregate different ss3 scenarios, to facilitate comparison
#

# -------------------------------------------------------------------------
# SAVEPLOT

SavePlot<-function(plotname,width=8,height=4){
  file <- file.path(dir_plot,paste0(plotname,'.png'))
  dev.print(png,file,width=width,height=height,units='in',res=300)
}

# -------------------------------------------------------------------------
# PLOT SETTINGS

theme_fun <-function(){
  theme_bw()+
    theme(axis.title.x = element_text(size = 18, face = "bold"),
          axis.title.y = element_text(size = 18, face = "bold"),
          axis.text.x = element_text(size=18, angle=0),
          axis.text.y = element_text(size=18, angle=0),
          title=element_text(size=18,angle=0),
          legend.text = element_text(size=12))
}

# -------------------------------------------------------------------------
# MODEL PERFORMANCE 
mod.performance <- function(sc_nm,desc,sc_ss3,summary_file){
  if (file.exists(summary_file)) { 
    
    df <- read.csv(summary_file)
    if(!sc_nm %in% df$Name ) {
      sc_sum <- summary.check(sc_nm, sc_ss3,desc)
      df <- as.data.frame(rbind(df, sc_sum))
      print("The file exists, but adding a new row") 
    }}else { 
      print("The file does not exist") 
      df <- summary.check(sc_nm, sc_ss3,desc)
    }
  return(df)
}

# -------------------------------------------------------------------------
#  SUMMARY.CHECK

summary.check <- function(sc.nm, ss3,desc){
  nms <- c( "Name",	"Description","LKL",	"conv",	"hessian",	"n_param",	"n_param_bound", "Time","Read_sspar")
  df <- data.frame(matrix(ncol=length(nms)))
  colnames(df) <- nms
  #Remove forecast parameters
  pattern <- c("ForeRecr")
  n_param  <- ss3$parameters   %>%  dplyr::filter(!grepl(pattern,Label)) %>% dplyr::filter(!is.na(Active_Cnt)) %>% 
    summarise(n = n())
  
  n_param_bound  <- ss3$parameters   %>%  dplyr::filter(!grepl(pattern,Label)) %>% dplyr::filter(!is.na(Active_Cnt)) %>%   dplyr::filter(Status %in% c("LO","HI")) %>% 
    summarise(n = n())
  
  aux <- c(sc.nm,desc,ss3$likelihoods_used["TOTAL","values"],ss3$maximum_gradient_component, ss3$log_det_hessian,n_param, n_param_bound,ss3$RunTime,ss3$Start_from_par)
  df[1, ] <- aux
  return(df)
}

# -------------------------------------------------------------------------
# AGGREGATE.SSMOD

aggregate.ssMod <- function(scs, path_scs){
  mod_sum <- NULL
  for (i in 1:length(scs)){
    sc_dir = path_scs[i]
    ss3 <- SSgetoutput(dirvec=sc_dir ,  getcovar=FALSE,forecast=FALSE)
    mod_sum <- c(mod_sum,ss3)
    rm(ss3)
    
  }
  return(SSsummarize(mod_sum))
}


# -------------------------------------------------------------------------

# Select first year (when model starts):
mod_str_yr = 1979
mod_end_yr = 2024
# First and last quarter:
mod_str_qt = yearqtr2qtr(mod_str_yr, 1, 1950, 13)
mod_end_qt = yearqtr2qtr(mod_end_yr, 4, 1950, 13)

# fLEET Order
fleet_order = c('LL', 'PSFS', 'PSLS', 'FL', 'LINE', 'BB', 'OTHER')

# Model labels (from 1 to 4):
model_labels = c('1BlockLL_noLS', '1BlockLL_LS', 
                  '2BlockLL_noLS', '2BlockLL_LS')

