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
#'
#' @return coordinates of amap(GCJ-02). Or the Chinese address.
#' @export
coord_trans2gaode <- function(coord,
                              type = "baidu",
                              key = amap_key,
                              output = "JSON") {
    url <- paste0(
        "https://restapi.amap.com/v3/assistant/coordinate/convert?",
        "&key=", key,
        "&locations=", coord,
        "&coordsys=", type,
        "&output=", output
    )
    n <- 1
    while (n <= 5) {
        tmp_json <- jsonlite::fromJSON(paste(readLines(url, warn = F, encoding = "UTF-8"), collapse = ""))
        status <- tmp_json$status
        # Double judgment
        if (status == 1 & length(tmp_json$location) != 0) {
            coord_gaode <- tmp_json$location
            break
        } else {
            print(n)
            coord_gaode <- "NULL,NULL"
            n <- n + 1
        }
    }
    return(coord_gaode)
}

#' @rdname maps
#' @param address The Chinese address
#' @export
coord_get <- function(address,
                      key = amap_key,
                      output = "JSON") {
    url <- paste0(
        "https://restapi.amap.com/v3/geocode/geo?",
        "&key=", key, ## key
        "&address=", address, ## 详细地址
        "&output=", "JSON"
    )
    n <- 1
    while (n <= 5) {
        tmp_json <- jsonlite::fromJSON(paste(readLines(url, warn = F, encoding = "UTF-8"), collapse = ""))
        status <- tmp_json$status
        # Double judgment
        if (status == 1 & length(tmp_json$geocodes) != 0) {
            coord_gaode <- tmp_json$geocodes$location
            break
        } else {
            print(n)
            coord_gaode <- "NULL,NULL"
            n <- n + 1
        }
    }
    return(coord_gaode)
}



#' @rdname maps
#' @examples
#' coord_get("chinese address")
#' coord_rev("108.952560,34.204798")
#' coord_trans2gaode("108.952560,34.204798")
#' @export
coord_rev <- function(coord,
                      key = amap_key,
                      output = "JSON") {
    url <- paste0(
        "https://restapi.amap.com/v3/geocode/regeo?",
        "&key=", key, ## key
        "&location=", coord, ## 经纬度
        "&output=", "JSON"
    )
    n <- 1
    while (n <= 5) {
        tmp_json <- jsonlite::fromJSON(paste(readLines(url, warn = F, encoding = "UTF-8"), collapse = ""))
        status <- tmp_json$status
        # Double judgment
        if (status == 1 & length(tmp_json$regeocode) != 0) {
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


tmp_key1 <- 5210 + 1114
tmp_key2 <- "a50d89c6e0"
tmp_key3 <- 850 - 109
tmp_key4 <- "dfd3aa7c6f67dee"
amap_key <- paste0(tmp_key1, tmp_key2, tmp_key3, tmp_key4)