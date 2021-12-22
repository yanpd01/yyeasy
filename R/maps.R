#' Map tools
#'
#' Coordinate transformation. Obtain the coordinate. Get the address.
#'
#' Visit https://lbs.amap.com/api/webservice/summary
#' for more infomation.
#' NOTE: These functions only supoort chinese address.
#' @rdname maps
#' @param coord  string, Latitude follows longitude, separated by commas.\\n
#' eg:"108.952560,34.204798"
#' @param type "baidu", "gps"
#' @param key amap key
#' @param output output type, default is JSON.
#' @return coordinates of amap(GCJ-02). Or the Chinese address.
#' @examples
#' ## maps_get_coords("陕西师范大学")
#' maps_get_coords("%E9%99%95%E8%A5%BF%E5%B8%88%E8%8C%83%E5%A4%A7%E5%AD%A6")
#' maps_get_address("108.952560,34.204798")
#' maps_get_elevation("108.952560,34.204798")
#' maps_trans2gcj02("108.952560,34.204798", type = "baidu")
#' @export
maps_trans2gcj02 <- function(coord,
                             type = "baidu",
                             key = amap_key,
                             output = "JSON") {
    tmp_url <- paste0(
        "https://restapi.amap.com/v3/assistant/coordinate/convert",
        "?key=", key, ## 第一个参数前用 ? 开头，后续参数以 & 开头
        "&locations=", coord,
        "&coordsys=", type,
        "&output=", output
    ) %>%
        utils::URLencode()
    n <- 1
    while (n <= 2) {
        tmp_json <- jsonlite::fromJSON(paste(
            readLines(tmp_url, warn = F, encoding = "UTF-8"),
            collapse = ""
        ))
        status <- tmp_json$status
        # status
        if (status == 1) {
            tmp_coords <- tmp_json$location
            break
        } else {
            print(n)
            tmp_coords <- "NULL,NULL"
            n <- n + 1
        }
    }
    return(tmp_coords)
}

#' @rdname maps
#' @param address The Chinese address
#' @export
maps_get_coords <- function(address,
                            key = amap_key,
                            output = "JSON") {
    tmp_url <- paste0(
        "https://restapi.amap.com/v3/geocode/geo",
        "?key=", key, ## 第一个参数前用 ? 开头，后续参数以 & 开头
        "&address=", address, ## 详细地址
        "&output=", output
    ) %>%
        utils::URLencode()
    n <- 1
    while (n <= 2) {
        tmp_json <- jsonlite::fromJSON(paste(
            readLines(tmp_url, warn = F, encoding = "UTF-8"),
            collapse = ""
        ))
        status <- tmp_json$status
        # status
        if (status == 1) {
            tmp_coords <- tmp_json$geocodes$location
            break
        } else {
            print(n)
            tmp_coords <- "NULL,NULL"
            n <- n + 1
        }
    }
    return(tmp_coords)
}



#' @rdname maps
#' @export
maps_get_address <- function(coord,
                             key = amap_key,
                             output = "JSON") {
    tmp_url <- paste0(
        "https://restapi.amap.com/v3/geocode/regeo",
        "?key=", key, ## 第一个参数前用 ? 开头，后续参数以 & 开头
        "&location=", coord, ## 经纬度
        "&output=", output
    ) %>%
        utils::URLencode()
    n <- 1
    while (n <= 2) {
        tmp_json <- jsonlite::fromJSON(paste(
            readLines(tmp_url, warn = F, encoding = "UTF-8"),
            collapse = ""
        ))
        status <- tmp_json$status
        # status
        if (status == 1) {
            address <- tmp_json$regeocode$formatted_address
            break
        } else {
            print(n)
            address <- "NULL"
            n <- n + 1
        }
    }
    return(address)
}

#' @rdname maps
#' @export
maps_get_elevation <- function(coord,
                               key = google_key) {
    tmp_coord0 <- unlist(strsplit(coord, ","))
    locations <- paste0(tmp_coord0[2], ",", tmp_coord0[1])
    tmp_url <- paste0(
        "https://maps.googleapis.com/maps/api/elevation/json",
        "?key=", key, ## 第一个参数前用 ? 开头，后续参数以 & 开头
        "&locations=", locations
    ) %>%
        utils::URLencode()
    n <- 1
    while (n <= 2) {
        tmp_json <- jsonlite::fromJSON(paste(
            readLines(tmp_url, warn = F, encoding = "UTF-8"),
            collapse = ""
        ))
        status <- tmp_json$status
        # status
        if (status == "OK") {
            tmp_coords <- tmp_json$results[, "elevation"]
            break
        } else {
            print(n)
            tmp_coords <- NA
            n <- n + 1
        }
    }
    return(tmp_coords)
}


####### key start #######
tmp_ak1 <- 5210 + 1114
tmp_ak2 <- "a50d89c6e0"
tmp_ak3 <- 850 - 109
tmp_ak4 <- "dfd3aa7c6f67"
tmp_ak5 <- paste0(letters[c(4, 5, 5)], collapse = "")
amap_key <- paste0(tmp_ak1, tmp_ak2, tmp_ak3, tmp_ak4, tmp_ak5)

tmp_gk1 <- paste0(LETTERS[c(1, 9)], collapse = "")
tmp_gk2 <- paste0(letters[c(26, 1)], collapse = "")
tmp_gk3 <- "SyBazjXtRY"
tmp_gk4 <- paste0(letters[c(4, 2, 6, 19)], collapse = "")
tmp_gk5 <- "HUrL9x0I-x8yGWJzw1r"
tmp_gk6 <- paste0((9 - 2), "c")
google_key <- paste0(tmp_gk1, tmp_gk2, tmp_gk3, tmp_gk4, tmp_gk5, tmp_gk6)

####### key over ##########