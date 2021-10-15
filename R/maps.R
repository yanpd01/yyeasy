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
                              key = "f538a09476ee5a6d62b457684b6c32a5",
                              output = "JSON"
) {
    url<- paste0(
        "https://restapi.amap.com/v3/assistant/coordinate/convert?",
        "&key=", key,
        "&locations=", coord,
        "&coordsys=", type,
        "&output=", output
    )
    n <- 1
    while (n<=5) {
        tmp_json <- jsonlite::fromJSON(paste(readLines(url,warn = F, encoding = 'UTF-8'), collapse = ""))
        status <- tmp_json$status ## 判断查询是否成功
        if (status == 1 & length(tmp_json$location) != 0){ ## 有时候成功了但是没有收到数据，所以用了一个“且”进行判定
            coord_gaode <- tmp_json$location
            break
        } else {
            print(n)
            coord_gaode <- 'NULL,NULL'
            n <- n + 1
        }
    }
    return(coord_gaode)
}

#' @rdname maps
#' @param address The Chinese address
#' @export
coord_get <- function(address,
                      key = "f538a09476ee5a6d62b457684b6c32a5",
                      output = "JSON") {
    url <- paste0(
        'https://restapi.amap.com/v3/geocode/geo?',
        '&key=', key, ## key
        '&address=', address, ## 详细地址
        '&output=', 'JSON'
    )
    n <- 1
    while (n<=5) {
        tmp_json <- jsonlite::fromJSON(paste(readLines(url,warn = F, encoding = 'UTF-8'), collapse = ""))
        status <- tmp_json$status ## 判断查询是否成功
        if (status == 1 & length(tmp_json$geocodes) != 0){ ## 有时候成功了但是没有收到数据，所以用了一个“且”进行判定
            coord_gaode <- tmp_json$geocodes$location
            break
        } else {
            print(n)
            coord_gaode <- 'NULL,NULL'
            n <- n+1
        }
    }
    return(coord_gaode)
}



#' @rdname maps
#' @examples
#' coord_get("chinese address")
#' coord_rev("108.952560,34.204798")
#' coord_trans2gaode("108.952560,34.204798")
#'
#' @export
coord_rev <- function(coord,
                      key = "f538a09476ee5a6d62b457684b6c32a5",
                      output = "JSON") {
    url <- paste0(
        'https://restapi.amap.com/v3/geocode/regeo?',
        '&key=', key, ## key
        "&location=", coord, ## 经纬度
        '&output=', 'JSON'
    )
    n <- 1
    while (n<=5) {
        tmp_json <- jsonlite::fromJSON(paste(readLines(url,warn = F, encoding = 'UTF-8'), collapse = ""))
        status <- tmp_json$status ## 判断查询是否成功
        if (status == 1 & length(tmp_json$regeocode) != 0){ ## 有时候成功了但是没有收到数据，所以用了一个“且”进行判定
            address <- tmp_json$regeocode$formatted_address
            break
        } else {
            print(n)
            address <- 'NULL'
            n <- n+1
        }
    }
    return(address)
}
