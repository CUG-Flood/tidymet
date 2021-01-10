{

    # 2019年数据错误较多
    # dist in the unit of km
    st_moveInfo[, dist := get_dist(lon, lat), .(site)]

    info = st_moveInfo[dist > 100, .N, .(site)][order(-N), ]

    # 1.manually fix position error
    d_wrongPos = st_moveInfo[lon <= 7000 | lat <= 1500, ]

    st_moveInfo[site == 58246 & lon == 0, lon := 12000]
    st_moveInfo[site == 55294, `:=`(lon = 9141, lat = 3216)]
    st_moveInfo[site == 54287 & lon < 7000, `:=`(lon = 12805, lat = 4201)]
    st_moveInfo[site == 51701 & lon < 7000, `:=`(lon = 7524, lat = 4031)]
}

d_short = st_moveInfo[n_period <= 10, ]
sites_bad = table(d_short$site) %>% .[. > 10] %>% names()
# c("51777", "55279", "55294", "55472", "56223")

st_moveInfo[site %in% sites_bad[1] & period_date_begin >= "2019-01-01",
            `:=`(lon = 8811, lat = 3901, alt = alt[5])]
st_moveInfo[site %in% sites_bad[1]] %>% fix_position()
st_moveInfo[site %in% sites_bad[2] & period_date_begin >= "2011-01-01",
            `:=`(lon = 9001, lat = 3123, alt = 470)]
st_moveInfo[site %in% sites_bad[3]] %>% fix_position()

d = st_moveInfo[site == 58246]
d = st_moveInfo[site == 55294]
# 55294: the correct position 9141 3216; 中国, 西藏, 安多
