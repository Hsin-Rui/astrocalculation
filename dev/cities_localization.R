library(RSQLite)
library(DBI)
library(dplyr)

# 1. Connect to the SQLite Database
# ------------------------------------------------------------------------------
db_path <- system.file("extdata", "cities.sqlite", package = "astrocalculation")
if (db_path == "") {
  db_path <- "inst/extdata/cities.sqlite" # Fallback for dev environment
}

message("Connecting to database at: ", db_path)
con <- connect_cities_db()

# 2. Schema Migration: Add name_zh columns
# ------------------------------------------------------------------------------
add_column_if_missing <- function(con, table, col_name, col_type) {
  fields <- DBI::dbListFields(con, table)
  if (!col_name %in% fields) {
    message(sprintf("Adding '%s' column to '%s' table...", col_name, table))
    DBI::dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s %s", table, col_name, col_type))
  }
}

add_column_if_missing(con, "countries", "name_zh", "TEXT")
add_column_if_missing(con, "cities", "name_zh", "TEXT")

# 3. Taiwan City Mapping (Exact matches from your tw_cities.csv)
# ------------------------------------------------------------------------------
tw_map <- c(
  "Taipei" = "台北市",
  "Taipei City" = "台北市",
  "New Taipei City" = "新北市",
  "Taichung" = "台中市",
  "Tainan" = "台南市",
  "Kaohsiung" = "高雄市",
  "Taoyuan City" = "桃園市",
  "Keelung" = "基隆市",
  "Hsinchu City" = "新竹市",
  "Hsinchu County" = "新竹縣",
  "Chiayi City" = "嘉義市",
  "Chiayi County" = "嘉義縣",
  "Miaoli" = "苗栗縣",
  "Changhua" = "彰化縣",
  "Nantou" = "南投縣",
  "Yunlin" = "雲林縣",
  "Yilan" = "宜蘭縣",
  "Hualien" = "花蓮縣",
  "Taitung" = "台東縣",
  "Pingtung" = "屏東縣",
  "Penghu" = "澎湖縣",
  "Kinmen" = "金門縣",
  "Lienchiang County" = "連江縣"
)

message("Updating Taiwan City Translations...")
dbBegin(con)
tryCatch({
  count <- 0
  for (en_name in names(tw_map)) {
    zh_name <- tw_map[[en_name]]

    # FIX: Use ':zh' and ':en' for SQLite named parameters
    res <- dbExecute(con, "UPDATE cities SET name_zh = :zh WHERE name = :en AND country_code = 'TW'",
                     params = list(zh = zh_name, en = en_name))
    count <- count + res
  }
  dbCommit(con)
  message(sprintf("Updated %d Taiwan city records.", count))
}, error = function(e) {
  dbRollback(con)
  stop("Failed to update Taiwan cities: ", e$message)
})
# 4. Full 252 Country Mapping (Based on your CSV)
# ------------------------------------------------------------------------------
# Note: "NA" is Namibia. We quote it carefully so R doesn't treat it as missing data.
country_map <- c(
  "AD" = "安道爾", "AE" = "阿聯酋", "AF" = "阿富汗", "AG" = "安地卡及巴布達",
  "AI" = "安圭拉", "AL" = "阿爾巴尼亞", "AM" = "亞美尼亞", "AO" = "安哥拉",
  "AQ" = "南極洲", "AR" = "阿根廷", "AS" = "美屬薩摩亞", "AT" = "奧地利",
  "AU" = "澳洲", "AW" = "阿魯巴", "AX" = "奧蘭群島", "AZ" = "亞塞拜然",
  "BA" = "波士尼亞", "BB" = "巴貝多", "BD" = "孟加拉", "BE" = "比利時",
  "BF" = "布吉納法索", "BG" = "保加利亞", "BH" = "巴林", "BI" = "蒲隆地",
  "BJ" = "貝南", "BL" = "聖巴泰勒米", "BM" = "百慕達", "BN" = "汶萊",
  "BO" = "玻利維亞", "BQ" = "波內赫、聖尤斯特歇斯及薩巴", "BR" = "巴西", "BS" = "巴哈馬",
  "BT" = "不丹", "BV" = "布威島", "BW" = "波札那", "BY" = "白俄羅斯",
  "BZ" = "貝里斯", "CA" = "加拿大", "CC" = "科科斯（基林）群島", "CD" = "剛果民主共和國",
  "CF" = "中非共和國", "CG" = "剛果共和國", "CH" = "瑞士", "CI" = "象牙海岸",
  "CK" = "庫克群島", "CL" = "智利", "CM" = "喀麥隆", "CN" = "中國",
  "CO" = "哥倫比亞", "CR" = "哥斯大黎加", "CU" = "古巴", "CV" = "維德角",
  "CW" = "庫拉索", "CX" = "聖誕島", "CY" = "塞浦路斯", "CZ" = "捷克",
  "DE" = "德國", "DJ" = "吉布地", "DK" = "丹麥", "DM" = "多米尼克",
  "DO" = "多明尼加", "DZ" = "阿爾及利亞", "EC" = "厄瓜多", "EE" = "愛沙尼亞",
  "EG" = "埃及", "EH" = "西撒哈拉", "ER" = "厄利垂亞", "ES" = "西班牙",
  "ET" = "衣索比亞", "FI" = "芬蘭", "FJ" = "斐濟", "FK" = "福克蘭群島",
  "FM" = "密克羅尼西亞", "FO" = "法羅群島", "FR" = "法國", "GA" = "加彭",
  "GB" = "英國", "GD" = "格瑞那達", "GE" = "喬治亞", "GF" = "法屬圭亞那",
  "GG" = "根西島", "GH" = "迦納", "GI" = "直布羅陀", "GL" = "格陵蘭",
  "GM" = "甘比亞", "GN" = "幾內亞", "GP" = "瓜地洛普", "GQ" = "赤道幾內亞",
  "GR" = "希臘", "GS" = "南喬治亞與南桑威奇群島", "GT" = "瓜地馬拉", "GU" = "關島",
  "GW" = "幾內亞比索", "GY" = "蓋亞那", "HK" = "香港", "HM" = "赫德島和麥克唐納群島",
  "HN" = "宏都拉斯", "HR" = "克羅埃西亞", "HT" = "海地", "HU" = "匈牙利",
  "ID" = "印尼", "IE" = "愛爾蘭", "IL" = "以色列", "IM" = "曼島",
  "IN" = "印度", "IO" = "英屬印度洋領地", "IQ" = "伊拉克", "IR" = "伊朗",
  "IS" = "冰島", "IT" = "義大利", "JE" = "澤西島", "JM" = "牙買加",
  "JO" = "約旦", "JP" = "日本", "KE" = "肯亞", "KG" = "吉爾吉斯",
  "KH" = "柬埔寨", "KI" = "吉里巴斯", "KM" = "葛摩", "KN" = "聖克里斯多福及尼維斯",
  "KP" = "北韓", "KR" = "南韓", "XK" = "科索沃", "KW" = "科威特",
  "KY" = "開曼群島", "KZ" = "哈薩克", "LA" = "寮國", "LB" = "黎巴嫩",
  "LC" = "聖露西亞", "LI" = "列支敦斯登", "LK" = "斯里蘭卡", "LR" = "賴比瑞亞",
  "LS" = "賴索托", "LT" = "立陶宛", "LU" = "盧森堡", "LV" = "拉脫維亞",
  "LY" = "利比亞", "MA" = "摩洛哥", "MC" = "摩納哥", "MD" = "摩爾多瓦",
  "ME" = "蒙特內哥羅", "MF" = "聖馬丁（法屬）", "MG" = "馬達加斯加", "MH" = "馬紹爾群島",
  "MK" = "北馬其頓", "ML" = "馬利", "MM" = "緬甸", "MN" = "蒙古",
  "MO" = "澳門", "MP" = "北馬里亞納群島", "MQ" = "馬丁尼克", "MR" = "茅利塔尼亞",
  "MS" = "蒙塞拉特", "MT" = "馬爾他", "MU" = "摩里西斯", "MV" = "馬爾地夫",
  "MW" = "馬拉威", "MX" = "墨西哥", "MY" = "馬來西亞", "MZ" = "莫三比克",
  "NA" = "納米比亞", "NC" = "新喀里多尼亞", "NE" = "尼日", "NF" = "諾福克島",
  "NG" = "奈及利亞", "NI" = "尼加拉瓜", "NL" = "荷蘭", "NO" = "挪威",
  "NP" = "尼泊爾", "NR" = "諾魯", "NU" = "紐埃", "NZ" = "紐西蘭",
  "OM" = "阿曼", "PA" = "巴拿馬", "PE" = "秘魯", "PF" = "法屬玻里尼西亞",
  "PG" = "巴布亞紐幾內亞", "PH" = "菲律賓", "PK" = "巴基斯坦", "PL" = "波蘭",
  "PM" = "聖皮埃與密克隆", "PN" = "皮特肯群島", "PR" = "波多黎各", "PS" = "巴勒斯坦",
  "PT" = "葡萄牙", "PW" = "帛琉", "PY" = "巴拉圭", "QA" = "卡達",
  "RE" = "留尼旺", "RO" = "羅馬尼亞", "RS" = "塞爾維亞", "RU" = "俄羅斯",
  "RW" = "盧安達", "SA" = "沙烏地阿拉伯", "SB" = "索羅門群島", "SC" = "塞席爾",
  "SD" = "蘇丹", "SS" = "南蘇丹", "SE" = "瑞典", "SG" = "新加坡",
  "SH" = "聖赫勒拿", "SI" = "斯洛維尼亞", "SJ" = "斯瓦尔巴和扬马延", "SK" = "斯洛伐克",
  "SL" = "獅子山", "SM" = "聖馬利諾", "SN" = "塞內加爾", "SO" = "索馬利亞",
  "SR" = "蘇利南", "ST" = "聖多美普林西比", "SV" = "薩爾瓦多", "SX" = "荷屬聖馬丁",
  "SY" = "敘利亞", "SZ" = "史瓦帝尼", "TC" = "特克斯與凱科斯群島", "TD" = "查德",
  "TF" = "法屬南部領地", "TG" = "多哥", "TH" = "泰國", "TJ" = "塔吉克",
  "TK" = "托克勞", "TL" = "東帝汶", "TM" = "土庫曼", "TN" = "突尼西亞",
  "TO" = "東加", "TR" = "土耳其", "TT" = "千里達及托巴哥", "TV" = "吐瓦魯",
  "TW" = "台灣", "TZ" = "坦尚尼亞", "UA" = "烏克蘭", "UG" = "烏干達",
  "UM" = "美國本土外小島嶼", "US" = "美國", "UY" = "烏拉圭", "UZ" = "烏茲別克",
  "VA" = "梵蒂岡", "VC" = "聖文森及格瑞那丁", "VE" = "委內瑞拉", "VG" = "英屬維京群島",
  "VI" = "美屬維京群島", "VN" = "越南", "VU" = "萬那杜", "WF" = "瓦利斯和富圖納",
  "WS" = "薩摩亞", "YE" = "葉門", "YT" = "馬約特", "ZA" = "南非",
  "ZM" = "尚比亞", "ZW" = "辛巴威",
  "CS" = "塞爾維亞與蒙特內哥羅", "AN" = "荷屬安地列斯"
)

message("Updating Country Translations (Full List)...")
dbBegin(con)
tryCatch({
  count <- 0
  for (code in names(country_map)) {
    zh_name <- country_map[[code]]
    # FIX: Use named parameters :zh and :code
    res <- dbExecute(con, "UPDATE countries SET name_zh = :zh WHERE country_code = :code",
                     params = list(zh = zh_name, code = code))
    count <- count + res
  }
  dbCommit(con)
  message(sprintf("Updated %d country records.", count))
}, error = function(e) {
  dbRollback(con)
  stop("Failed to update countries: ", e$message)
})
# 5. Verification
# ------------------------------------------------------------------------------
res <- dbGetQuery(con, "SELECT count(*) as missing FROM cities WHERE country_code = 'TW' AND name_zh IS NULL")
if (res$missing > 0) {
  warning(sprintf("⚠️ Warning: %d Taiwan cities still lack translations.", res$missing))
} else {
  message("✅ SUCCESS: All Taiwan cities now have Chinese translations!")
}

dbDisconnect(con)
