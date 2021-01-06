#=====================================
# Init Env
#=====================================
rm(list = ls())

#=====================================
# Set Env
#=====================================
# Pycharm 용도
Sys.setlocale("LC_CTYPE", ".1251")

# RStudio 용도
Sys.setlocale("LC_ALL", "Korean")
options(encoding = "UTF-8")
Sys.setenv(LANG = "ko_KR.UTF-8")

# Sys.setlocale("LC_ALL", "English")
# options(encoding = "UTF-8")
# Sys.setenv(LANG = "en_US.UTF-8")

globalVar = new.env()

globalVar$optDig = 10
globalVar$memLimit = 9999999999999

# configg
# globalVar$config = getwd()
globalVar$config = "."
globalVar$inpConfig = paste(globalVar$config, "RESOURCES", 'INPUT', sep = '/')
globalVar$figConfig = paste(globalVar$config, "RESOURCES", 'FIG', sep = '/')
globalVar$outConfig = paste(globalVar$config, "RESOURCES", 'OUTPUT', sep = '/')
globalVar$logConfig = paste(globalVar$config, "RESOURCES", 'LOG', sep = '/')
globalVar$systemConfig = paste(globalVar$config, "RESOURCES", 'CONFIG', 'system.cfg', sep = '/')
globalVar$fontConfig = paste(globalVar$config, "RESOURCES", 'CONFIG', 'FONT_INFO', sep = '/')

# key
configInfo = yaml::yaml.load_file(globalVar$systemConfig)
globalVar$googleKey = configInfo$default$googleKey
globalVar$dataKey = configInfo$default$dataKey
globalVar$naverKeyId = configInfo$default$naverKeyId
globalVar$naverKeyPw = configInfo$default$naverKeyPw
globalVar$kakaoRestApiKey = configInfo$default$kakaoRestApiKey
globalVar$gyeonggiDataKey = configInfo$default$gyeonggiDataKey
globalVar$naverApigwApiKeyId = configInfo$default$naverApigwApiKeyId
globalVar$naverApigwApiKey = configInfo$default$naverApigwApiKey

utils::ls.str(globalVar)

# Rtools
# writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
# Sys.which("make")

#=====================================
# Set Fun
#=====================================
log = log4r::create.logger()
log4r::logfile(log) = paste0(globalVar$logConfig, "/log4r_", format(Sys.time(), "%Y%m%d"), ".log")
log4r::level(log) = "INFO"

tryCatch(

  expr = {
    # 주 소스 코드
    log4r::info(log, sprintf("%s", "[START] Main R"))

  }

  , warning = function(warning) {
    log4r::warn(log, warning)
  }

  , error = function(error) {
    log4r::error(log, error)
  }

  , finally = {
    log4r::info(log, sprintf("%s", "[END] Main R"))
  }
)


perfEval = function(x, y) {

  if (length(x) < 1) { return(sprintf("%s", "x 값 없음")) }
  if (length(y) < 1) { return(sprintf("%s", "y 값 없음")) }

  slope = coef(lm(y ~ x))[2]
  interp = coef(lm(y ~ x))[1]
  xMean = mean(x, na.rm = TRUE)
  yMean = mean(y, na.rm = TRUE)
  xSd = sd(x, na.rm = TRUE)
  ySd = sd(y, na.rm = TRUE)
  cnt = length(x)
  bias = mean(x - y, na.rm = TRUE)
  rBias = (bias / yMean) * 100.0
  rmse = sqrt(mean((x - y)^2, na.rm = TRUE))
  rRmse = (rmse / yMean) * 100.0
  r = cor(x, y)
  r2 = cor(x, y)^2
  diffMean = mean(x - y, na.rm = TRUE)
  diffSd = sd(x - y, na.rm = TRUE)
  # perDiffMean = mean((x - y) / y, na.rm = TRUE) * 100.0

  return(c(slope, interp, xMean, yMean, xSd, ySd, cnt, bias, rBias, rmse, rRmse, r, r2, diffMean, diffSd))

}

#=====================================
# Set Data
#=====================================
options(digits = globalVar$optDig)
options(java.parameters = "-Xmx8192m")
memory.limit(size = globalVar$memLimit)

library(ggmap)
ggmap::register_google(key = globalVar$googleKey)

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

serviceName = "PRJ0004"

library(extrafont)
library(tidyverse)
library(data.table)
library(lubridate)
library(lubridate)
library(GeoLight)
library(showtext)
library(ggmap)
library(data.table)
library(colorRamps)
library(showtext)
library(marmap)
library(maptools)
library(lubridate)


#=====================================
# Set Font
#=====================================

# 원도우에서 설치된 폰트 확인
sysfonts::font_files() %>%
  as.tibble() %>%
  dplyr::filter(
    stringr::str_detect(family, regex("KoPub|Century|Palatino"))
    , stringr::str_detect(face, regex("Regular|Medium"))
  )

#******************************
# 인코딩으로 인해 오류 발생
#******************************
# # 폰트 추가
# extrafont::font_import(paths = globalVar$fontConfig, pattern = "NewCenturySchoolbook.ttf", prompt = FALSE)
# extrafont::font_import(paths = globalVar$fontConfig, pattern = "pala.ttf", prompt = FALSE)
# 
# 
# # 폰트 확인
# extrafont::fonts()


#******************************
# 인코딩으로 인해 오류 발생
#******************************
# 인터넷 환경에서 구글 폰트 추가
# font.add.google("Gochi Hand", "gochi")

# 오프라인 환경에서 특정 경로에서 국/영문 폰트 추가
# 영문 폰트
sysfonts::font.add(family = "New Century Schoolbook", regular = paste(globalVar$fontConfig, "NewCenturySchoolbook.ttf", sep = "/"))
sysfonts::font.add(family = "Palatino Linotype", regular = paste(globalVar$fontConfig, "pala.ttf", sep = "/"))

# 국문 폰트
sysfonts::font.add(family = "KoPubWorld Dotum Medium", regular = paste(globalVar$fontConfig, "KoPubWorld Dotum Medium.ttf", sep = "/"))

# 폰트 읽기
showtext::showtext.auto()

# 폰트 확인
sysfonts::font_families()

font = "New Century Schoolbook"
fontKor = "KoPubWorld Dotum Medium"
fontEng = "Palatino Linotype"

#==============================================================
# 2차년도.R
#==============================================================

#=====================================================
#   URL-API Time setting
#=====================================================

#***********************************
# 1.07 sec elapsed
#***********************************
tictoc::tic()

kst = seq(ISOdatetime(year = 2015, month = 1, day = 1, hour = 00, min = 00, sec = 00),
          ISOdatetime(year = 2017, month = 12, day = 31, hour = 23, min = 59, sec = 59), by = "1 hour", tz = "Asia/Seoul")

c01 = as.numeric(str_sub(kst, 1, 4))
c02 = as.numeric(str_sub(kst, 6, 7))
c03 = as.numeric(str_sub(kst, 9, 10))
c04 = as.numeric(str_sub(kst, 12, 13))

time = data.frame(c01, c02, c03, c04)

time_L1 = time %>%
  dplyr::mutate(fn = sprintf("%04d%02d%02d%02d", c01, c02, c03, c04)) %>%
  dplyr::select(fn)

write.table(time_L1, file = './2015_2017_1hour.Time', sep = " ", row.names = F, col.names = F)

tictoc::toc()

#***********************************
# 0.16 sec elapsed
#***********************************
tictoc::tic()

dtDateKst = seq(
  lubridate::ymd_hms("2015-01-01 00:00:00", tz = "Asia/Seoul")
  , lubridate::ymd_hms("2017-12-31 23:59:59", tz = "Asia/Seoul")
  , by = "1 hour"
)

dtDate = tibble::tibble(dtDateKst) %>%
  dplyr::mutate(sDate = format(dtDateKst, "%Y%m%d%H")) %>%
  dplyr::select(sDate)

saveFile = sprintf("%s_%s", serviceName, "Date_1-Hour_2015-2017.csv")

data.table::fwrite(
  data.frame(dtDate)
  , sep = ","
  , file = saveFile
  , append = FALSE
  , row.names = FALSE
  , col.names = TRUE
  , dateTimeAs = "write.csv"
  , na = NA
)

tictoc::toc()


#=====================================================
#   Time 설정
#==========================
# ===========================

#***********************************
# 161.42 sec elapsed
#***********************************
tictoc::tic()

kst = seq(
  ISOdatetime(year = 2013, month = 1, day = 1, hour = 00, min = 00, sec = 00)
  , ISOdatetime(year = 2017, month = 12, day = 31, hour = 23, min = 59, sec = 59)
  , by = "1 min"
  , tz = "Asua/Seoul"
)

c01 = as.numeric(substr(kst, 1, 4))
c02 = as.numeric(substr(kst, 6, 7))
c03 = as.numeric(substr(kst, 9, 10))
c04 = as.numeric(substr(kst, 12, 13))
c05 = as.numeric(substr(kst, 15, 16))
c06 = as.numeric(substr(kst, 18, 19))
time = data.frame(c01, c02, c03, c04, c05, c06)

time_L1 = time %>%
  mutate(
    kst = ISOdatetime(year = c01, month = c02, day = c03, hour = c04, min = c05, sec = c06, tz = "")
    , utc = with_tz(kst, "utc")
  )

colnames(time_L1) = c("year", "month", "day", "hour", "minute", "sec", "kst", "utc")
head(time_L1)

tictoc::toc()


#***********************************
# 1.85 sec elapsed
#***********************************
tictoc::tic()

dtDateTimeKst = seq(
  lubridate::ymd_hms("2013-01-01 00:00:00", tz = "Asia/Seoul")
  , lubridate::ymd_hms("2017-12-31 23:59:59", tz = "Asia/Seoul")
  , by = "1 min"
)

dtDateTimeList = tibble::tibble(dtDateTimeKst) %>%
  dplyr::mutate(
    year = lubridate::year(dtDateTimeKst)
    , month = lubridate::month(dtDateTimeKst)
    , day = lubridate::day(dtDateTimeKst)
    , hour = lubridate::hour(dtDateTimeKst)
    , min = lubridate::minute(dtDateTimeKst)
    , sec = lubridate::second(dtDateTimeKst)
    , dtDateTimeUtc = lubridate::with_tz(dtDateTimeKst, "UTC")
  )

tictoc::toc()


#===========================================================
#   IORS observation
#===========================================================
# iors_L1 = read.csv('2nd plan/DATA_L1', header=F, sep="")
iors_L1 = read.csv('Irradiance/DATA_L1', header = FALSE, sep = "") %>%
  as.tibble() %>%
  magrittr::set_colnames(c("year", "month", "day", "hour", "minute", "sec", "iors_dsr", "sun_shine"))

iors_L1

# head(iors_L1)
# colnames(iors_L1) = c("year", "month", "day", "hour", "minute", "sec", "iors_dsr", "sun_shine")

# 이어도 종합해양과학기지 위/경도
iorsGeoLat = 32.12295277777780
iorsGeoLon = 125.182447222222

iors_L2 = iors_L1 %>%
  dplyr::filter(dplyr::between(year, 2014, 2014)) %>%
  dplyr::mutate(
    dtDateTimeKst = lubridate::make_datetime(year, month, day, hour, minute, sec, tz = "Asia/Seoul")
    , dtDateTimeUtc = lubridate::with_tz(dtDateTimeKst, tzone = "UTC")
    , dtDate = lubridate::date(dtDateTimeKst)
    , dtXran = lubridate::decimal_date(dtDateTimeKst)
    , sza = GeoLight::zenith(GeoLight::solar(dtDateTimeKst), iorsGeoLon, iorsGeoLat)
    , jd = yday(dtDateTimeKst)
    , ga = 2 * pi * (jd - 1) / 365.0
    , Eo = (1.000110 +
      0.034221 * cos(ga) +
      0.001280 * sin(ga) +
      0.000719 * cos(2 * ga) +
      0.000077 * sin(2 * ga))
    , Max_PPL = (1368 * Eo * 1.5 * (cos(sza * pi / 180)^1.2)) + 100
    , Min_PPL = -4
    , Max_ERL = (1368 * Eo * 1.2 * (cos(sza * pi / 180)^1.2)) + 50
    , Min_ERL = -2
  ) %>%
  dplyr::filter(!duplicated(dtDateTimeKst))


iors_L2
summary(iors_L2)

# 원시자료
# MONTH = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
#
# iors_L2 = iors_L1 %>%
#   # dplyr::filter( year >= 2005)  %>%
#   # dplyr::filter( iors_dsr != -999)  %>%
#   dplyr::filter(2014 <= year & year <= 2016) %>%
#   # dplyr::filter( 9 <= hour  &  hour <= 16 ) %>%
#   dplyr::mutate(fn = as.numeric(sprintf("%04d%02d%02d", year, month, day)),
#                 kst = ISOdatetime(year = year, month = month, day = day, hour = hour, min = minute, sec = sec, tz = ""),
#                 utc = with_tz(kst, tz = "utc"),
#                 xran = year +
#                   ((month - 1) / 12.) +
#                   ((day - 1) / (12. * MONTH[month])) +
#                   (hour / (12. * MONTH[month] * 24.)) +
#                   (minute / (12. * MONTH[month] * 24. * 60.)) +
#                   (sec / (12. * MONTH[month] * 24. * 60. * 60.)),
#                 sza = SZA(kst, Lat = lat, Lon = long),
#                 jd = yday(kst),
#                 ga = 2 * pi * (jd - 1) / 365.0,
#                 Eo = (1.000110 +
#                   0.034221 * cos(ga) +
#                   0.001280 * sin(ga) +
#                   0.000719 * cos(2 * ga) +
#                   0.000077 * sin(2 * ga)),
#                 Max_PPL = (1368 * Eo * 1.5 * (cos(sza * pi / 180)^1.2)) + 100,
#                 Min_PPL = -4,
#                 Max_ERL = (1368 * Eo * 1.2 * (cos(sza * pi / 180)^1.2)) + 50,
#                 Min_ERL = -2) %>%
#   # dplyr::filter( Max_PPL != "NaN") %>%
#   # dplyr::filter( Max_ERL != "NaN") %>%
#   dplyr::filter(!duplicated(kst))


# summary(iors_L2)

# head(iors_L2)
#
# iors_L3 = iors_L2 %>%
#    dplyr::anti_join(QC, by="fn")
#
# iors_L4 = iors_L3 %>%
#    dplyr::filter( Max_PPL != "NaN") %>%
#    dplyr::filter( Max_ERL != "NaN") %>%
#    dplyr::filter( Min_PPL <= iors_dsr  &  iors_dsr <= Max_PPL ) %>%
#    dplyr::filter( Min_ERL <= iors_dsr  &  iors_dsr <= Max_ERL )


# 육안 검토 자료.
# QC = read.csv('2nd plan/QC_SW_1st', header = FALSE, sep = "") %>%
#   as.tibble() %>%
#   dplyr::mutate(dtDate = lubridate::ymd(V1)
# colnames(QC) = c("fn")
# head(QC)

# summary(iors_L4)
# iors_L4 = iors_L2 %>%
#   dplyr::filter(! dtDate %in% QC$dtDate)
# dplyr::anti_join(QC, by = "fn")

tryCatch(
  expr = {
    log4r::info(log, sprintf("%s", "[START] Maek Image"))

    # 주 소스 코드
    X = iors_L2$dtXran
    Y = iors_L2$iors_dsr

    lmFit = lm(Y ~ X)
    # cor.test(X, Y)
    # head(iors_L2)

    saveImg = sprintf("%s/%s_%s", globalVar$figConfig, serviceName, "GWNU_ori4.png")
    png(file = saveImg, width = 12, height = 8, units = "in", res = 600)

    par(mar = c(5.1, 7, 5.1, 5), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
    # par(mar = c(5.1, 7, 5.1, 5), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2)
    plot(X, iors_L2$Max_PPL, xlim = c(2014, 2017), col = 'red', ylim = c(0, 3000), xlab = "", ylab = "", yaxt = "n", xaxt = "n", xaxs = "i", yaxs = "i")
    points(X, iors_L2$Max_ERL, col = "blue", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
    points(X, Y, pch = 21, bg = "white", col = 'black', xlab = "", ylab = "", yaxt = "n", xaxt = "n")
    axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(2014, 2017, 1))
    mtext('Time  [Year]', side = 1, col = 'black', line = 3, cex = 2.2)
    axis(2, col = "black", col.axis = "black", las = 1, at = seq(0, 3000, 500))
    mtext(expression(paste(bold("1-Minute  Solar  Radiation  (IORS)  [Wm"^bold("-2") * "]"))), side = 2, col = "black", line = 5, cex = 2.2)
    # abline(h=seq(200, 1000, 200), lw=1, col='lightgrey', lty="dotted")  ;  abline(v=seq(2006, 2015, 1), lw=1, col='lightgrey', lty="dotted")

    abline(lmFit, col = 'orange', font = 2, lw = 3)
    text(2014.2, 2900, paste0("(IORS) = ", round(lmFit$coefficients[2], 2), " x (Year) +", round(lmFit$coefficients[1], 2)), adj = 0, col = 'orange', font = 2, cex = 1.5)
    # text(2005.2, 2250, paste0("R = ", round(cor(X, Y),2), " (p<", round(cor.test(X, Y)$p.value,2), ")" ), adj=0, col='orange', font=2, cex=1.5)
    text(2014.2, 2650, paste0("R = ", round(cor(X, Y), 2), " (p-value < 0.001)"), adj = 0, col = 'orange', font = 2, cex = 1.5)
    # text(2005.2, 2400, paste0("N = ", length(X)), adj=0, col='orange', font=2, cex=1.5)
    text(2014.2, 2400, paste0("N = 393,883"), adj = 0, col = 'orange', font = 2, cex = 1.5)
  }

  , warning = function(warning) {
    log4r::warn(log, warning)
  }

  , error = function(error) {
    log4r::error(log, error)
  }

  , finally = {
    dev.off()

    log4r::info(log, sprintf("%s", "[END] Maek Image"))
  }
)

# X = iors_L2$dtXran
# Y = iors_L2$iors_dsr
#
# lm.fit = lm(Y ~ X)


# cor.test(X, Y)

# head(iors_L2)
# summary(Y)

# plot(X, Y)


# png("Fig/GWNU_ori.png", 1300, 650)
# par(mar=c(5.1, 7, 5.1, 5), cex.main=2.0, cex.lab=1.6, cex.axis=1.6, cex=1.3, font.axis=2, font.lab=2, font.main=2, font=2, family = "New Century Schoolbook")
# plot(X,  iors_L2$Max_PPL,  xlim=c(2005, 2017), col='red', ylim=c(0, 3000), xlab="", ylab="", yaxt="n", xaxt="n", xaxs="i", yaxs="i")
# points(X, iors_L2$Max_ERL, col="blue", xlab="", ylab="", yaxt="n", xaxt="n")
# points(X, Y, pch=21, bg="white", col='black', xlab="", ylab="", yaxt="n", xaxt="n")
# axis(1, col='black', col.axis='black', las=1, at=seq(2005, 2017, 1))
# mtext('Time  [Year]',side=1, col='black', line=3, cex=2.2)
# axis(2, col="black", col.axis="black", las=1, at=seq(0, 3000, 500))
# mtext( expression(paste(bold("1/10-Minute  Solar  Radiation  (IORS)  [Wm"^bold("-2")*"]"))), side=2, col="black", line=5, cex=2.2)
# # abline(h=seq(200, 1000, 200), lw=1, col='lightgrey', lty="dotted")  ;  abline(v=seq(2006, 2015, 1), lw=1, col='lightgrey', lty="dotted")
# lm.fit = lm(Y ~ X)
# abline(lm.fit, col='orange', font=2, lw=3)
# text(2005.2, 2900, paste0("(IORS) = ", round(lm.fit$coefficients[2],2)," x (Year) +", round(lm.fit$coefficients[1], 2)), adj=0, col='orange', font=2, cex=1.5)
# # text(2005.2, 2250, paste0("R = ", round(cor(X, Y),2), " (p<", round(cor.test(X, Y)$p.value,2), ")" ), adj=0, col='orange', font=2, cex=1.5)
# text(2005.2, 2650, paste0("R = ", round(cor(X, Y),2), " (p-value < 0.001)" ), adj=0, col='orange', font=2, cex=1.5)
# # text(2005.2, 2400, paste0("N = ", length(X)), adj=0, col='orange', font=2, cex=1.5)
# text(2005.2, 2400, paste0("N = 1,185,772"), adj=0, col='orange', font=2, cex=1.5)
# dev.off()

# png("Fig/GWNU_ori4.png", 1300, 650)

# iors_L2 = iors_L2 %>%
# dplyr::top


# saveImg = sprintf("%s/%s_%s", globalVar$figConfig, serviceName, "GWNU_ori4.png")
# png(file = saveImg, width = 10, height = 6, units = "in", res = 600)
#
# par(mar = c(5.1, 7, 5.1, 5), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
# # par(mar = c(5.1, 7, 5.1, 5), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2)
# plot(X, iors_L2$Max_PPL, xlim = c(2014, 2017), col = 'red', ylim = c(0, 3000), xlab = "", ylab = "", yaxt = "n", xaxt = "n", xaxs = "i", yaxs = "i")
# points(X, iors_L2$Max_ERL, col = "blue", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
# points(X, Y, pch = 21, bg = "white", col = 'black', xlab = "", ylab = "", yaxt = "n", xaxt = "n")
# axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(2014, 2017, 1))
# mtext('Time  [Year]', side = 1, col = 'black', line = 3, cex = 2.2)
# axis(2, col = "black", col.axis = "black", las = 1, at = seq(0, 3000, 500))
# mtext(expression(paste(bold("1-Minute  Solar  Radiation  (IORS)  [Wm"^bold("-2") * "]"))), side = 2, col = "black", line = 5, cex = 2.2)
# # abline(h=seq(200, 1000, 200), lw=1, col='lightgrey', lty="dotted")  ;  abline(v=seq(2006, 2015, 1), lw=1, col='lightgrey', lty="dotted")
#
# abline(lm.fit, col = 'orange', font = 2, lw = 3)
# text(2014.2, 2900, paste0("(IORS) = ", round(lm.fit$coefficients[2], 2), " x (Year) +", round(lm.fit$coefficients[1], 2)), adj = 0, col = 'orange', font = 2, cex = 1.5)
# # text(2005.2, 2250, paste0("R = ", round(cor(X, Y),2), " (p<", round(cor.test(X, Y)$p.value,2), ")" ), adj=0, col='orange', font=2, cex=1.5)
# text(2014.2, 2650, paste0("R = ", round(cor(X, Y), 2), " (p-value < 0.001)"), adj = 0, col = 'orange', font = 2, cex = 1.5)
# # text(2005.2, 2400, paste0("N = ", length(X)), adj=0, col='orange', font=2, cex=1.5)
# text(2014.2, 2400, paste0("N = 393,883"), adj = 0, col = 'orange', font = 2, cex = 1.5)
#
# dev.off()

## Fig. -----
# X = iors_L2$sza
# Y = iors_L2$iors_dsr
#
# # png("Fig/GWNU_ori_SZA.png", 1300, 650)
#
# saveImg = sprintf("%s/%s_%s", globalVar$figConfig, serviceName, "GWNU_ori_SZA.png")
# png(file = saveImg, width = 10, height = 6, units = "in", res = 600)
#
#
# # par(mar = c(5.1, 7, 5.1, 5), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
# par(mar = c(5.1, 7, 5.1, 5), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2)
# plot(X, iors_L2$Max_ERL, xlim = c(0, 90), col = 'blue', ylim = c(0, 3000), xlab = "", ylab = "", yaxt = "n", xaxt = "n", xaxs = "i", yaxs = "i")
# points(X, iors_L2$Max_PPL, col = "red", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
# points(X, Y, xlab = "", ylab = "", yaxt = "n", xaxt = "n", pch = 21, bg = "white", col = 'black')
# axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(0, 90, 10))
# mtext('Solar  Zenith  Angle', side = 1, col = 'black', line = 3, cex = 2.2)
# axis(2, col = "black", col.axis = "black", las = 1, at = seq(0, 3000, 500))
# mtext(expression(paste(bold("1/10-Minute  Solar  Radiation  (IORS)  [Wm"^bold("-2") * "]"))), side = 2, col = "black", line = 5, cex = 2.2)

# dev.off()

tryCatch(
  expr = {
    log4r::info(log, sprintf("%s", "[START] Maek Image"))

    # 주 소스 코드
    X = iors_L2$sza
    Y = iors_L2$iors_dsr

    # png("Fig/GWNU_ori_SZA.png", 1300, 650)

    saveImg = sprintf("%s/%s_%s", globalVar$figConfig, serviceName, "GWNU_ori_SZA.png")
    png(file = saveImg, width = 12, height = 8, units = "in", res = 600)

    par(mar = c(5.1, 7, 5.1, 5), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
    plot(X, iors_L2$Max_ERL, xlim = c(0, 90), col = 'blue', ylim = c(0, 3000), xlab = "", ylab = "", yaxt = "n", xaxt = "n", xaxs = "i", yaxs = "i")
    points(X, iors_L2$Max_PPL, col = "red", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
    points(X, Y, xlab = "", ylab = "", yaxt = "n", xaxt = "n", pch = 21, bg = "white", col = 'black')
    axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(0, 90, 10))
    mtext('Solar  Zenith  Angle', side = 1, col = 'black', line = 3, cex = 2.2)
    axis(2, col = "black", col.axis = "black", las = 1, at = seq(0, 3000, 500))
    mtext(expression(paste(bold("1/10-Minute  Solar  Radiation  (IORS)  [Wm"^bold("-2") * "]"))), side = 2, col = "black", line = 5, cex = 2.2)
  }
  , warning = function(warning) {
    log4r::warn(log, warning)
  }
  , error = function(error) {
    log4r::error(log, error)
  }
  , finally = {
    dev.off()

    log4r::info(log, sprintf("%s", "[END] Maek Image"))
  }
)


#*********************************
# 품질 검사
#*********************************
# 육안 검토 자료
qcData = read.csv('2nd plan/QC_SW_1st', header = FALSE, sep = "") %>%
  as.tibble() %>%
  dplyr::mutate(dtDate = lubridate::ymd(V1))

qcData


# colnames(QC) = c("fn")
# head(QC)

# summary(iors_L4)
# iors_L4 = iors_L2 %>%
#   dplyr::filter(! dtDate %in% QC$dtDate)

# 품질관리 (1, 2, 3단계)
# MONTH = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
# lat = 32.12295277777780
# long = 125.182447222222

iors_L2_QC = iors_L1 %>%
  dplyr::filter(
    dplyr::between(year, 2005, 2020)
    , dplyr::between(hour, 5, 20)
    , iors_dsr > 0
  ) %>%
  dplyr::mutate(
    dtDateTimeKst = lubridate::make_datetime(year, month, day, hour, minute, sec, tz = "Asia/Seoul")
    , dtDateTimeUtc = lubridate::with_tz(dtDateTimeKst, tzone = "UTC")
    , dtDate = lubridate::date(dtDateTimeKst)
    , dtXran = lubridate::decimal_date(dtDateTimeKst)
    , sza = GeoLight::zenith(GeoLight::solar(dtDateTimeKst), iorsGeoLon, iorsGeoLat)
    , jd = yday(dtDateTimeKst)
    , ga = 2 * pi * (jd - 1) / 365.0
    , Eo = (1.000110 +
      0.034221 * cos(ga) +
      0.001280 * sin(ga) +
      0.000719 * cos(2 * ga) +
      0.000077 * sin(2 * ga))
    , Max_PPL = (1368 * Eo * 1.5 * (cos(sza * pi / 180)^1.2)) + 100
    , Min_PPL = -4
    , Max_ERL = (1368 * Eo * 1.2 * (cos(sza * pi / 180)^1.2)) + 50
    , Min_ERL = -2
  ) %>%
  dplyr::filter(
    !duplicated(dtDateTimeKst)
    , Max_PPL != "NaN"
    , Max_ERL != "NaN"
  ) %>%
  dplyr::filter(
    ! dtDate %in% qcData$dtDate
    , ! dplyr::between(iors_dsr, Min_PPL, Max_PPL)
    , ! dplyr::between(iors_dsr, Min_ERL, Max_ERL)
  )

iors_L2_QC

# iors_L2_QC = iors_L1 %>%
#   # dplyr::filter( year >= 2005)  %>%
#   dplyr::filter(iors_dsr > 0) %>%
#   dplyr::filter(2014 <= year & year <= 2016) %>%
#   dplyr::filter(5 <= hour & hour <= 20) %>%
#   # dplyr::filter( between(hour, 9, 15) )  %>%
#   dplyr::mutate(fn = as.numeric(sprintf("%04d%02d%02d", year, month, day)),
#                 kst = ISOdatetime(year = year, month = month, day = day, hour = hour, min = minute, sec = sec, tz = ""),
#                 utc = with_tz(kst, tz = "utc"),
#                 xran = year +
#                   ((month - 1) / 12.) +
#                   ((day - 1) / (12. * MONTH[month])) +
#                   (hour / (12. * MONTH[month] * 24.)) +
#                   (minute / (12. * MONTH[month] * 24. * 60.)) +
#                   (sec / (12. * MONTH[month] * 24. * 60. * 60.)),
#                 sza = SZA(kst, Lat = lat, Lon = long),
#                 jd = yday(kst),
#                 ga = 2 * pi * (jd - 1) / 365.0,
#                 Eo = (1.000110 +
#                   0.034221 * cos(ga) +
#                   0.001280 * sin(ga) +
#                   0.000719 * cos(2 * ga) +
#                   0.000077 * sin(2 * ga)),
#                 Max_PPL = (1368 * Eo * 1.5 * (cos(sza * pi / 180)^1.2)) + 100,
#                 Min_PPL = -4,
#                 Max_ERL = (1368 * Eo * 1.2 * (cos(sza * pi / 180)^1.2)) + 50,
#                 Min_ERL = -2) %>%
#   dplyr::anti_join(QC, by = "fn") %>%
#   dplyr::filter(Max_PPL != "NaN") %>%
#   dplyr::filter(Max_ERL != "NaN") %>%
#   dplyr::filter(Min_PPL <= iors_dsr & iors_dsr <= Max_PPL) %>%
#   dplyr::filter(Min_ERL <= iors_dsr & iors_dsr <= Max_ERL) %>%
#   dplyr::filter(!duplicated(kst))


# write.table(iors_L2_QC, file = '20040101-20161231.IORS_QC2', sep = " ", row.names = F, col.names = T)

## 일평균
# Daily_Mean_data_L2_QC = data_L2_QC %>%
#    dplyr::group_by(year, month, day) %>%
#    dplyr::summarise(mean_dsr = mean(dsr, na.rm=T),
#                     count = n())

# iors_L3_QC = iors_L2_QC %>%
# dplyr::filter( year >= 2013) %>%
# dplyr::select(year, month, day, hour, minute, sec, iors_dsr)

# tail(iors_L3_QC)

# write.table(iors_L3_QC, file = '200501-201702.IORS', sep = " ", row.names = F, col.names = F)

#******************************************
# 
#******************************************
tryCatch(
  expr = {
    log4r::info(log, sprintf("%s", "[START] Maek Image"))

    X = iors_L2_QC$dtXran
    Y = iors_L2_QC$iors_dsr

    lmFit = lm(Y ~ X)

    saveImg = sprintf("%s/%s_%s", globalVar$figConfig, serviceName, "GWNU_QC2.png")
    png(file = saveImg, width = 14, height = 10, units = "in", res = 600)

    par(mar = c(5.1, 7, 5.1, 5), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
    plot(X, iors_L2_QC$Max_PPL, xlim = c(2005, 2017), col = 'red', ylim = c(0, 3000), xlab = "", ylab = "", yaxt = "n", xaxt = "n", xaxs = "i", yaxs = "i")
    points(X, iors_L2_QC$Max_ERL, col = "blue", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
    points(X, Y, pch = 21, bg = "white", col = 'black', xlab = "", ylab = "", yaxt = "n", xaxt = "n")
    axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(2005, 2017, 1))
    mtext('Time  [Year]', side = 1, col = 'black', line = 3, cex = 2.2)
    axis(2, col = "black", col.axis = "black", las = 1, at = seq(0, 3000, 500))
    mtext(expression(paste(bold("1/10-Minute  Solar  Radiaion  (IORS)  [Wm"^bold("-2") * "]"))), side = 2, col = "black", line = 5, cex = 2.2)
    # abline(h=seq(200, 1000, 200), lw=1, col='lightgrey', lty="dotted")  ;  abline(v=seq(2006, 2015, 1), lw=1, col='lightgrey', lty="dotted")

    abline(lmFit, col = 'orange', font = 2, lw = 3)
    text(2005.2, 2900, paste0("(IORS) = ", round(lmFit$coefficients[2], 2), " x (Year) + ", round(lmFit$coefficients[1], 2)), adj = 0, col = 'orange', font = 2, cex = 1.5)
    # text(2005.2, 2250, paste0("R = ", round(cor(X, Y),2), " (p<", round(cor.test(X, Y)$p.value,2), ")" ), adj=0, col='orange', font=2, cex=1.5)
    text(2005.2, 2650, paste0("R = ", round(cor(X, Y), 2), " (p-value < 0.001)"), adj = 0, col = 'orange', font = 2, cex = 1.5)
    # text(2005.2, 2400, paste0("N = ", length(X)), adj=0, col='orange', font=2, cex=1.5)
    text(2005.2, 2400, paste0("N = 483,967"), adj = 0, col = 'orange', font = 2, cex = 1.5)

  }
  , warning = function(warning) {
    log4r::warn(log, warning)
  }
  , error = function(error) {
    log4r::error(log, error)
  }
  , finally = {
    dev.off()

    log4r::info(log, sprintf("%s", "[END] Maek Image"))
  }
)


tryCatch(
  expr = {
    log4r::info(log, sprintf("%s", "[START] Maek Image"))

    X = iors_L2_QC$sza
    Y = iors_L2_QC$iors_dsr

    saveImg = sprintf("%s/%s_%s", globalVar$figConfig, serviceName, "GWNU_QC2_SZA.png")
    png(file = saveImg, width = 14, height = 10, units = "in", res = 600)

    par(mar = c(5.1, 7, 5.1, 5), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
    plot(X, Y, xlim = c(0, 90), pch = 21, bg = "white", col = 'black', ylim = c(0, 3000), xlab = "", ylab = "", yaxt = "n", xaxt = "n", xaxs = "i", yaxs = "i")
    points(X, iors_L2_QC$Max_ERL, col = "blue", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
    points(X, iors_L2_QC$Max_PPL, col = 'red', xlab = "", ylab = "", yaxt = "n", xaxt = "n")
    axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(0, 90, 10))
    mtext('Solar  Zenith  Angle', side = 1, col = 'black', line = 3, cex = 2.2)
    axis(2, col = "black", col.axis = "black", las = 1, at = seq(0, 3000, 500))
    mtext(expression(paste(bold("1/10-Minute  Solar  Radiation  (IORS)  [Wm"^bold("-2") * "]"))), side = 2, col = "black", line = 5, cex = 2.2)
  }
  , warning = function(warning) {
    log4r::warn(log, warning)
  }
  , error = function(error) {
    log4r::error(log, error)
  }
  , finally = {
    dev.off()

    log4r::info(log, sprintf("%s", "[END] Maek Image"))
  }
)


#******************************************
# COMS/MI DSR
#******************************************
# data = read.csv("2nd plan/COMS_IEODO_INS_20150101-20170825.OUT", sep = "", head = FALSE)
#
# head(data)
#
# c01 = as.numeric(substr(data[, 1], 1, 4))
# c02 = as.numeric(substr(data[, 1], 5, 6))
# c03 = as.numeric(substr(data[, 1], 7, 8))
# c04 = as.numeric(substr(data[, 1], 9, 10))
# c05 = as.numeric(substr(data[, 1], 11, 12))
# data2 = data.frame(c01, c02, c03, c04, c05)
# coms_ins = data.frame(data2, data[, -1])
#
# colnames(coms_ins) = c("year", "month", "day", "hour", "minute", "coms_dsr")
#
# coms_ins_L1 = coms_ins %>%
#   dplyr::mutate(kst = ISOdatetime(year = year, month = month, day = day, hour = hour, min = minute, sec = 00, tz = ""),
#                 utc = with_tz(kst, "utc")) %>%
#   dplyr::filter(!duplicated(kst))
#
# head(coms_ins_L1)


coms_ins_L1 = read.csv("2nd plan/COMS_IEODO_INS_20150101-20170825.OUT", sep = "", head = FALSE) %>%
  as.tibble() %>%
  dplyr::mutate(
    dtDateTimeKst = lubridate::force_tz(readr::parse_datetime(as.character(V1), "%Y%m%d%H%M"), tzone = "Asia/Seoul")
    , dtDateTimeUtc = lubridate::with_tz(dtDateTimeKst, tzone = "UTC")
  ) %>%
  dplyr::filter(!duplicated(dtDateTimeKst)) %>%
  rename(coms_dsr = V2)

#******************************************
# COMS/MI CA
#******************************************
# data = read.csv("2nd plan/COMS_IEODO_CA_20121202-20170825.OUT", sep = "", head = F)
# head(data)
#
# c01 = as.numeric(substr(data[, 1], 1, 4))
# c02 = as.numeric(substr(data[, 1], 5, 6))
# c03 = as.numeric(substr(data[, 1], 7, 8))
# c04 = as.numeric(substr(data[, 1], 9, 10))
# c05 = as.numeric(substr(data[, 1], 11, 12))
# data2 = data.frame(c01, c02, c03, c04, c05)
# coms_ca = data.frame(data2, data[, -1])
#
# colnames(coms_ca) = c("year", "month", "day", "hour", "minute", "coms_ca")
#
# coms_ca_L1 = coms_ca %>%
#   dplyr::mutate(kst = ISOdatetime(year = c01, month = c02, day = c03, hour = c04, min = c05, sec = 00, tz = ""),
#                 utc = with_tz(kst, "utc")) %>%
# dplyr::filter(!duplicated(kst))
#
# head(coms_ca_L1)

coms_ca_L1 = read.csv("2nd plan/COMS_IEODO_CA_20121202-20170825.OUT", sep = "", head = FALSE) %>%
  as.tibble() %>%
  dplyr::mutate(
    dtDateTimeKst = lubridate::force_tz(readr::parse_datetime(as.character(V1), "%Y%m%d%H%M"), tzone = "Asia/Seoul")
    , dtDateTimeUtc = lubridate::with_tz(dtDateTimeKst, tzone = "UTC")
  ) %>%
  dplyr::filter(!duplicated(dtDateTimeKst)) %>%
  rename(coms_ca = V2)

#******************************************
# GWNU Model
#******************************************
# gwnu1 = read.csv("2nd plan/GWNU_IORS_20130101-20151231.OUT", sep = "", head = FALSE)
# colnames(gwnu1) = c("year", "month", "day", "hour", "minute", "lat", "lon", "sza", "cos_sza",
#                     "ext", "alt", "alb", "oz", "aer", "temp", "slp", "sp", "tpw", "cm", "cf",
#                     "vis", "gwnu_dsr", "mj", "tot", "dir", "dif")
#
# head(gwnu1)
#
# gwnu2 = read.csv("2nd plan/GWNU_IORS_20160101-20161231.dat", sep = "", head = F)
# colnames(gwnu2) = c("year", "month", "day", "hour", "minute", "lat", "lon", "sza", "cos_sza",
#                     "ext", "alt", "alb", "oz", "aer", "temp", "slp", "sp", "tpw", "cm", "cf",
#                     "vis", "gwnu_dsr", "mj", "tot", "dir", "dif")
#
#
# gwnu2_L1 = gwnu2 %>%
#   dplyr::mutate(utc = ISOdatetime(year = year, month = month, day = day, hour = hour, min = minute, sec = 00, tz = "utc"),
#                 kst = with_tz(utc, ""),
#                 year = as.numeric(format(kst, "%Y")),
#                 month = as.numeric(format(kst, "%m")),
#                 day = as.numeric(format(kst, "%d")),
#                 hour = as.numeric(format(kst, "%H")),
#                 minute = as.numeric(format(kst, "%M"))) %>%
#   # sec    = as.numeric(format(kst, "%S")) )
#   dplyr::select(-utc, -kst)
#
#
# gwnu = rbind(gwnu1, gwnu2_L1)
#
# gwnu_L1 = gwnu %>%
#   # dplyr::filter( between(year, 2016, 2016) )  %>%
#   dplyr::filter(between(hour, 9, 15)) %>%
#   dplyr::mutate(kst = ISOdatetime(year = year, month = month, day = day, hour = hour, min = minute, sec = 00, tz = ""),
#                 xran = year +
#                   ((month - 1) / 12.) +
#                   ((day - 1) / (12. * MONTH[month])) +
#                   (hour / (12. * MONTH[month] * 24.)) +
#                   (minute / (12. * MONTH[month] * 24. * 60.)),
#                 utc = with_tz(kst, "")) %>%
#   dplyr::filter(!duplicated(kst))
#
#
# gwnu_L2 = gwnu_L1 %>%
#   dplyr::filter(between(hour, 9, 15)) %>%
#   filter(year >= 2013)
#
# head(gwnu_L2)


gwnu1 = read.csv("2nd plan/GWNU_IORS_20130101-20151231.OUT", sep = "", head = FALSE)
gwnu2 = read.csv("2nd plan/GWNU_IORS_20160101-20161231.dat", sep = "", head = FALSE)

gwnu_L1 = dplyr::bind_rows(gwnu1, gwnu2) %>%
  as.tibble() %>%
  magrittr::set_colnames(c("year", "month", "day", "hour", "minute", "lat", "lon", "sza", "cos_sza", "ext", "alt", "alb", "oz", "aer", "temp", "slp", "sp", "tpw", "cm", "cf", "vis", "gwnu_dsr", "mj", "tot", "dir", "dif")) %>%
  dplyr::mutate(
    dtDateTimeKst = lubridate::make_datetime(year, month, day, hour, minute, tz = "Asia/Seoul")
    , dtDateTimeUtc = lubridate::with_tz(dtDateTimeKst, tzone = "UTC")
    , dtXran = lubridate::decimal_date(dtDateTimeKst)
  ) %>%
  dplyr::filter(!duplicated(dtDateTimeKst))


gwnu_L2 = gwnu_L1 %>%
  dplyr::filter(
    dplyr::between(hour, 9, 15)
    , year >= 2013
  )

#================================================
#  Obs. merge  &  Moving average
#================================================
data_L1 = dtDateTimeList %>%
  dplyr::left_join(iors_L2_QC, by = c("dtDateTimeKst" = "dtDateTimeKst")) %>%
  dplyr::left_join(coms_ins_L1, by = c("dtDateTimeKst" = "dtDateTimeKst")) %>%
  dplyr::left_join(coms_ca_L1, by = c("dtDateTimeKst" = "dtDateTimeKst")) %>%
  dplyr::left_join(gwnu_L1, by = c("dtDateTimeKst" = "dtDateTimeKst")) %>%
  dplyr::select(year.x, month.x, day.x, hour.x, min, sec.x, dtDateTimeKst, dtDateTimeUtc.x, coms_dsr, coms_ca, gwnu_dsr, iors_dsr, cm, dtXran.x) %>%
  dplyr::rename(year = year.x, month = month.x, hour = hour.x, sec = sec.x, dtXran = dtXran.x, dtDateTimeUtc = dtDateTimeUtc.x)

data_L1

data_L2 = na.omit(data_L1)

QC = data_L2 %>%
  dplyr::filter(abs(iors_dsr - gwnu_dsr) > 300)

index = sample(1:nrow(QC), length(QC[, 1]) * 0.8, replace = F)
QC2 = QC[index,]

# 신뢰구간 95% 데이터 사용
diff = data_L2$iors_dsr - data_L2$gwnu_dsr
rightConf95 = mean(diff, na.rm = TRUE) + (2 * sd(diff, na.rm = TRUE))
leftConf95 = mean(diff, na.rm = TRUE) - (2 * sd(diff, na.rm = TRUE))

hist(diff)

data_L3 = data_L2 %>%
  dplyr::filter(dplyr::between(iors_dsr - gwnu_dsr, leftConf95, rightConf95))

# 청천 영역
data_L4_clear = data_L3 %>%
  dplyr::filter(
    cm == 0
    , coms_ca == 0
  )

# 전천 영역 (구름 + 청천)
data_L4 = data_L3

#*********************************************
# IORS vs GWNU 산점도
#*********************************************

X = data_L4$gwnu_dsr
Y = data_L4$iors_dsr

X2 = data_L4_clear$gwnu_dsr
Y2 = data_L4_clear$iors_dsr

val = perfEval(X, Y)
# sprintf("%.2f", val)
val2 = perfEval(X2, Y2)
# sprintf("%.2f", val2)

xcord = 20
ycord = seq(1370, 0, -80)

xcord2 = 620
ycord2 = seq(440, 0, -80)


# clear (red), cloudy (blue)

saveImg = sprintf("%s/%s_%s", globalVar$figConfig, serviceName, "scatterplot_GWNU_IORS.png")

ggplot() +
  coord_fixed(ratio = 1) +
  theme_bw() +
  geom_point(aes(X, Y), shape = 21, size = 4, colour = "blue", fill = "white", alpha = 1) +
  geom_point(aes(X2, Y2), shape = 21, size = 4, colour = "red", fill = "white", alpha = 1) +
  # stat_bin2d(bins=200) +
  # scale_fill_gradientn(colours = myPalette(11), limits=c(0, 20), na.value="#9E0142") +
  # scale_fill_gradientn(colours=matlab.like(11), limits=c(0, 40), na.value="#AA0000") +
  # scale_fill_gradientn(colours=matlab.like2(11), limits=c(0, 40), na.value="#BF0000") +
  annotate("text", x = xcord, y = ycord[1], label = "--------------  All sky  --------------", size = 5.5, hjust = 0, colour = "blue", fontface = "bold", family = font) +
  annotate("text", x = xcord, y = ycord[2], label = paste0("(IORS) = ", sprintf("%.2f", val[1]), " x (GWNU) ", sprintf("%.2f", val[2])), size = 5, hjust = 0, colour = "blue", fontface = "bold", family = font) +
  annotate("text", x = xcord, y = ycord[3], label = paste0("R = ", sprintf("%.2f", val[12]), " (p-value < 0.001)"), size = 5, hjust = 0, colour = "blue", family = font, fontface = "bold") +
  annotate("text", x = xcord, y = ycord[4], label = paste0("Bias = ", sprintf("%.2f", val[8]), " (", sprintf("%.2f", val[9]), " %)"), parse = F, size = 5, hjust = 0, family = font, fontface = "bold", color = "black") +
  annotate("text", x = xcord, y = ycord[5], label = paste0("RMSE = ", sprintf("%.2f", val[10]), " (", sprintf("%.2f", val[11]), " %)"), parse = F, size = 5, hjust = 0, family = font, fontface = "bold", color = "black") +
  annotate("text", x = xcord, y = ycord[6], label = paste0("N = ", sprintf("%.0f", val[7])), size = 5, hjust = 0, colour = "black", family = font, fontface = "bold", colour = "black") +
  # geom_abline(intercept=0, slope=1, linetype=1, colour="black", size=1) +
  # stat_smooth(method="lm", colour="blue", se=F, aes(X, Y)) +
  scale_x_continuous(minor_breaks = seq(0, 1400, by = 200), breaks = seq(0, 1400, by = 200), expand = c(0, 0), limits = c(0, 1400)) +
  scale_y_continuous(minor_breaks = seq(0, 1400, by = 200), breaks = seq(0, 1400, by = 200), expand = c(0, 0), limits = c(0, 1400)) +

  annotate("text", x = xcord2, y = ycord2[1], label = "-------------  Clear sky  -------------", size = 5.5, hjust = 0, colour = "red", fontface = "bold", family = font) +
  annotate("text", x = xcord2, y = ycord2[2], label = paste0("(IORS) = ", sprintf("%.2f", val2[1]), " x (GWNU) +", sprintf("%.2f", val2[2])), size = 5, hjust = 0, colour = "red", fontface = "bold", family = font) +
  annotate("text", x = xcord2, y = ycord2[3], label = paste0("R = ", sprintf("%.2f", val2[12]), " (p-value < 0.001)"), size = 5, hjust = 0, colour = "red", family = font, fontface = "bold") +
  annotate("text", x = xcord2, y = ycord2[4], label = paste0("Bias = ", sprintf("%.2f", val2[8]), " (", sprintf("%.2f", val2[9]), " %)"), parse = F, size = 5, hjust = 0, family = font, fontface = "bold", color = "black") +
  annotate("text", x = xcord2, y = ycord2[5], label = paste0("RMSE = ", sprintf("%.2f", val2[10]), " (", sprintf("%.2f", val2[11]), " %)"), parse = F, size = 5, hjust = 0, family = font, fontface = "bold", color = "black") +
  annotate("text", x = xcord2, y = ycord2[6], label = paste0("N = ", sprintf("%.0f", val2[7])), size = 5, hjust = 0, colour = "black", family = font, fontface = "bold", colour = "black") +
  # annotate("text", x=200, y=980, label="bold(RMSE ~\"=\"~ \"63.22\" ~Wm^\"-2\"~\"(21.69\"~\"%)\")", parse=T, size=4.2, hjust=0, family=font) +
  geom_abline(intercept = 0, slope = 1, linetype = 1, colour = "black", size = 1) +
  stat_smooth(method = "lm", colour = "blue", se = F, aes(X, Y)) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X2, Y2), size = 1) +
  labs(
    title = NULL
    , x = expression(paste(bold("GWNU  Radiation  Model  DSR  [Wm"^bold("-2") * "]")))
    , y = expression(paste(bold("IORS  Observatory  DSR  [Wm"^bold("-2") * "]")))
    ) +
  # labs(fill = "Count") +
  labs(fill = NULL) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "black")
    , axis.title.x = element_text(face = "bold", size = 19, colour = "black")
    , axis.title.y = element_text(face = "bold", size = 19, colour = "black", angle = 90)
    , axis.text.x = element_text(face = "bold", size = 19, colour = "black")
    , axis.text.y = element_text(face = "bold", size = 19, colour = "black")
    , legend.title = element_text(face = "bold", size = 12, colour = "black")
    , legend.position = c(0, 1), legend.justification = c(0, 0.96)
    , legend.key = element_blank()
    , legend.text = element_text(size = 12, face = "bold")
    , legend.background = element_blank()
    , text = element_text(family = font)
    , plot.margin = unit(c(0, 8, 0, 0), "mm")
  ) +
  ggsave(filename = saveImg, width = 7, height = 7, dpi = 600)


#*********************************************
# IORS vs COMS 산점도
#*********************************************

X = data_L4$coms_dsr
Y = data_L4$iors_dsr

X2 = data_L4_clear$coms_dsr
Y2 = data_L4_clear$iors_dsr

val = perfEval(X, Y)
# sprintf("%.2f", val)
val2 = perfEval(X2, Y2)
# sprintf("%.2f", val2)

xcord = 20
ycord = seq(1370, 0, -80)

xcord2 = 620
ycord2 = seq(440, 0, -80)

saveImg = sprintf("%s/%s_%s", globalVar$figConfig, serviceName, "scatterplot_COMS_IORS.png")

ggplot() +
  coord_fixed(ratio = 1) +
  theme_bw() +
  geom_point(aes(X, Y), shape = 21, size = 4, colour = "blue", fill = "white", alpha = 1) +
  geom_point(aes(X2, Y2), shape = 21, size = 4, colour = "red", fill = "white", alpha = 1) +
  # stat_bin2d(bins=200) +
  # scale_fill_gradientn(colours = myPalette(11), limits=c(0, 20), na.value="#9E0142") +
  # scale_fill_gradientn(colours=matlab.like(11), limits=c(0, 40), na.value="#AA0000") +
  # scale_fill_gradientn(colours=matlab.like2(11), limits=c(0, 40), na.value="#BF0000") +
  annotate("text", x = xcord, y = ycord[1], label = "--------------  All sky  --------------", size = 5.5, hjust = 0, colour = "blue", fontface = "bold", family = font) +
  annotate("text", x = xcord, y = ycord[2], label = paste0("(IORS) = ", sprintf("%.2f", val[1]), " x (COMS) ", sprintf("%.2f", val[2])), size = 5, hjust = 0, colour = "blue", fontface = "bold", family = font) +
  annotate("text", x = xcord, y = ycord[3], label = paste0("R = ", sprintf("%.2f", val[12]), " (p-value < 0.001)"), size = 5, hjust = 0, colour = "blue", family = font, fontface = "bold") +
  annotate("text", x = xcord, y = ycord[4], label = paste0("Bias = ", sprintf("%.2f", val[8]), " (", sprintf("%.2f", val[9]), " %)"), parse = F, size = 5, hjust = 0, family = font, fontface = "bold", color = "black") +
  annotate("text", x = xcord, y = ycord[5], label = paste0("RMSE = ", sprintf("%.2f", val[10]), " (", sprintf("%.2f", val[11]), " %)"), parse = F, size = 5, hjust = 0, family = font, fontface = "bold", color = "black") +
  annotate("text", x = xcord, y = ycord[6], label = paste0("N = ", sprintf("%.0f", val[7])), size = 5, hjust = 0, colour = "black", family = font, fontface = "bold", colour = "black") +
  # geom_abline(intercept=0, slope=1, linetype=1, colour="black", size=1) +
  # stat_smooth(method="lm", colour="blue", se=F, aes(X, Y)) +
  scale_x_continuous(minor_breaks = seq(0, 1400, by = 200), breaks = seq(0, 1400, by = 200), expand = c(0, 0), limits = c(0, 1400)) +
  scale_y_continuous(minor_breaks = seq(0, 1400, by = 200), breaks = seq(0, 1400, by = 200), expand = c(0, 0), limits = c(0, 1400)) +
  annotate("text", x = xcord2, y = ycord2[1], label = "-------------  Clear sky  -------------", size = 5.5, hjust = 0, colour = "red", fontface = "bold", family = font) +
  annotate("text", x = xcord2, y = ycord2[2], label = paste0("(IORS) = ", sprintf("%.2f", val2[1]), " x (COMS) +", sprintf("%.2f", val2[2])), size = 5, hjust = 0, colour = "red", fontface = "bold", family = font) +
  annotate("text", x = xcord2, y = ycord2[3], label = paste0("R = ", sprintf("%.2f", val2[12]), " (p-value < 0.001)"), size = 5, hjust = 0, colour = "red", family = font, fontface = "bold") +
  annotate("text", x = xcord2, y = ycord2[4], label = paste0("Bias = ", sprintf("%.2f", val2[8]), " (", sprintf("%.2f", val2[9]), " %)"), parse = F, size = 5, hjust = 0, family = font, fontface = "bold", color = "black") +
  annotate("text", x = xcord2, y = ycord2[5], label = paste0("RMSE = ", sprintf("%.2f", val2[10]), " (", sprintf("%.2f", val2[11]), " %)"), parse = F, size = 5, hjust = 0, family = font, fontface = "bold", color = "black") +
  annotate("text", x = xcord2, y = ycord2[6], label = paste0("N = ", sprintf("%.0f", val2[7])), size = 5, hjust = 0, colour = "black", family = font, fontface = "bold", colour = "black") +
  # annotate("text", x=200, y=980, label="bold(RMSE ~\"=\"~ \"63.22\" ~Wm^\"-2\"~\"(21.69\"~\"%)\")", parse=T, size=4.2, hjust=0, family=font) +
  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1) +
  stat_smooth(method = "lm", colour = "blue", se = F, aes(X, Y)) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X2, Y2), size = 1) +
  labs(
    title = NULL
    , fill = NULL
    , x = expression(paste(bold("COMS / MI  DSR  [Wm"^bold("-2") * "]")))
    , y = expression(paste(bold("IORS  Observatory  DSR  [Wm"^bold("-2") * "]")))
    ) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "black")
    , axis.title.x = element_text(face = "bold", size = 19, colour = "black")
    , axis.title.y = element_text(face = "bold", size = 19, colour = "black", angle = 90)
    , axis.text.x = element_text(face = "bold", size = 19, colour = "black")
    , axis.text.y = element_text(face = "bold", size = 19, colour = "black")
    , legend.title = element_text(face = "bold", size = 12, colour = "black")
    , legend.position = c(0, 1), legend.justification = c(0, 0.96)
    , legend.key = element_blank()
    , legend.text = element_text(size = 12, face = "bold")
    , legend.background = element_blank()
    , text = element_text(family = font)
    , plot.margin = unit(c(0, 8, 0, 0), "mm")
  ) +
  ggsave(filename = saveImg, width = 7, height = 7, dpi = 600)


#====================================================
#  Time series
#====================================================

colList = c("IORS_DSR", "GWNU_DSR")
# colInfo = "IORS_DSR"

for (colInfo in colList) {
  
  if (colInfo == "IORS_DSR") {
    X = iors_L2_QC$dtXran
    Y = iors_L2_QC$iors_dsr
    
    yLabel = expression(paste(bold("IORS  Observatory  DSR  [Wm"^bold("-2")*"]")))
  } else {
    X = gwnu_L2$dtXran
    Y = gwnu_L2$gwnu_dsr
    
    yLabel = expression(paste(bold("GWNU  Radiation  Model  DSR  [Wm"^bold("-2") * "]")))
  }
  
  if (length(X) < 0) { next }
  if (length(Y) < 0) { next }
  
  val = perfEval(X, Y)
  
  saveImg = sprintf("%s/%s_%s.png", globalVar$figConfig, serviceName, colInfo)
  
  xcord = 2013.05
  ycord = seq(1350, 0, -100)
  
  ggplot() +
    # coord_fixed(ratio=1) +
    theme_bw() +
    # geom_point(aes(X, Y)) +
    geom_point(aes(X, Y), shape = 21, colour = "black", fill = "white", size = 2.5) +
    annotate("text", x = xcord, y = ycord[1], label = paste0("(DSR) = ", sprintf("%.2f", val[1]), " x (Year) + ", sprintf("%.2f", val[2])), size = 6, hjust = 0, color = "red", fontface = "bold", family = font) +
    # annotate("text", x=250, y=ycord[2], label=paste0("R = ", sprintf("%.2f",val[12]), " (p < 0.001) | Stdev = ", sprintf("%.2f",val[14])), size=5, hjust=0, color="red", family=font, fontface="bold") +
    annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", val[12]), " (p-value < 0.001)"), size = 6, hjust = 0, colour = "red", family = font, fontface = "bold") +
    # annotate("text", x=250, y=ycord[3], label=paste0("Stdev = ", sprintf("%.2f",val[14])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
    # annotate("text", x=250, y=ycord[4], label=paste0("AMI = ", sprintf("%.3f",val[3]), " | CERES = ", sprintf("%.3f",val[4])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
    # annotate("text", x=xcord, y=ycord[3], label=paste0("Bias = ", sprintf("%.2f",val[8]), " (", sprintf("%.2f",val[9])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
    # annotate("text", x=xcord, y=ycord[4], label=paste0("RMSE = ", sprintf("%.2f",val[10]), " (", sprintf("%.2f",val[11])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
    # annotate("text", x=xcord, y=ycord[3], label=paste0("N = ", sprintf("%.0f",val[7]) ), size=6, hjust=0, color="black", family=font, fontface="bold") +
    annotate("text", x = xcord, y = ycord[3], label = paste0("N = 32,311"), size = 6, hjust = 0, color = "black", family = font, fontface = "bold") +
    geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
    stat_smooth(method = "lm", color = "red", se = F, aes(X, Y)) +
    scale_x_continuous(minor_breaks = seq(2013, 2017, by = 1), breaks = seq(2013, 2017, by = 1), expand = c(0, 0), limits = c(2013, 2017)) +
    scale_y_continuous(minor_breaks = seq(0, 1400, by = 200), breaks = seq(0, 1400, by = 200), expand = c(0, 0), limits = c(0, 1400)) +
    labs(
      title = NULL
      , subtitle = NULL
      , fill = NULL
      , y = yLabel
      , x = expression(paste(bold("Time  [Year]")))
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 20, color = "black", hjust = 0.5)
      , axis.title.x = element_text(face = "bold", size = 20, colour = "black")
      , axis.title.y = element_text(face = "bold", size = 20, colour = "black", angle = 90)
      , axis.text.x = element_text(face = "bold", size = 20, colour = "black")
      , axis.text.y = element_text(face = "bold", size = 20, colour = "black")
      , legend.title = element_text(face = "bold", size = 12, colour = "black")
      , legend.position = c(0, 1), legend.justification = c(0, 0.9)
      , legend.key = element_blank()
      , legend.text = element_text(size = 12, face = "bold")
      , legend.background = element_blank()
      , text = element_text(family = font)
      , plot.margin = unit(c(0, 8, 0, 0), "mm")
    ) +
    ggsave(filename = saveImg, width = 12, height = 6, dpi = 600)
  
}


#***************************************************
# 에어로졸 시계열
#***************************************************
X = gwnu_L2$dtXran
Y = gwnu_L2$aer

val = perfEval(X, Y)
# sprintf("%.2f", val)

xcord = 2013.05
ycord = seq(2.9, 0, -0.25)

saveImg = sprintf("%s/%s_%s", globalVar$figConfig, serviceName, "GWNU_AOD.png")

ggplot() +
  # coord_fixed(ratio=1) +
  theme_bw() +
  # geom_point(aes(X, Y)) +
  geom_point(aes(X, Y), shape = 21, colour = "black", fill = "white", size = 2.5) +
  annotate("text", x = xcord, y = ycord[1], label = paste0("(AOD) = ", sprintf("%.2f", val[1]), " x (Year) + ", sprintf("%.2f", val[2])), size = 6, hjust = 0, color = "red", fontface = "bold", family = font) +
  # annotate("text", x=250, y=ycord[2], label=paste0("R = ", sprintf("%.2f",val[12]), " (p < 0.001) | Stdev = ", sprintf("%.2f",val[14])), size=5, hjust=0, color="red", family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", val2[12]), " (p-value < 0.001)"), size = 6, hjust = 0, colour = "red", family = font, fontface = "bold") +
  # annotate("text", x=250, y=ycord[3], label=paste0("Stdev = ", sprintf("%.2f",val[14])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=250, y=ycord[4], label=paste0("AMI = ", sprintf("%.3f",val[3]), " | CERES = ", sprintf("%.3f",val[4])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[3], label=paste0("Bias = ", sprintf("%.2f",val[8]), " (", sprintf("%.2f",val[9])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[4], label=paste0("RMSE = ", sprintf("%.2f",val[10]), " (", sprintf("%.2f",val[11])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[3], label=paste0("N = ", sprintf("%.0f",val[7]) ), size=6, hjust=0, color="black", family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[3], label = paste0("N = 32,311"), size = 6, hjust = 0, color = "black", family = font, fontface = "bold") +
  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X, Y)) +
  scale_x_continuous(minor_breaks = seq(2013, 2017, by = 1), breaks = seq(2013, 2017, by = 1), expand = c(0, 0), limits = c(2013, 2017)) +
  scale_y_continuous(minor_breaks = seq(0, 3, by = 0.5), breaks = seq(0, 3, by = 0.5), expand = c(0, 0), limits = c(0, 3)) +
  labs(
    title = "에어로졸 시계열"
    , subtitle = NULL
    , y = expression(paste(bold("Aerosol  Optical  Depth by GWNU")))
    , x = expression(paste(bold("Time  [Year]")))
    , fill = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "black")
    , axis.title.x = element_text(face = "bold", size = 20, colour = "black")
    , axis.title.y = element_text(face = "bold", size = 20, colour = "black", angle = 90)
    , axis.text.x = element_text(face = "bold", size = 20, colour = "black")
    , axis.text.y = element_text(face = "bold", size = 20, colour = "black")
    , legend.title = element_text(face = "bold", size = 12, colour = "black")
    , legend.position = c(0, 1), legend.justification = c(0, 0.9)
    , legend.key = element_blank()
    , legend.text = element_text(size = 12, face = "bold")
    , legend.background = element_blank()
    , text = element_text(family = font)
    , plot.margin = unit(c(0, 8, 0, 2), "mm")
    ) +
  ggsave(filename = saveImg, width = 12, height = 6, dpi = 600)


#***************************************************
# 가강수량 시계열
#***************************************************
X = gwnu_L2$dtXran
Y = gwnu_L2$tpw

# plot(X, Y)

val = perfEval(X, Y)
# sprintf("%.2f", val)

xcord = 2013.05
ycord = seq(9.5, 0, -1)

saveImg = sprintf("%s/%s_%s", globalVar$figConfig, serviceName, "GWNU_TPW.png")

ggplot() +
  # coord_fixed(ratio=1) +
  theme_bw() +
  # geom_point(aes(X, Y)) +
  geom_point(aes(X, Y), shape = 21, colour = "black", fill = "white", size = 2.5) +
  annotate("text", x = xcord, y = ycord[1], label = paste0("(TPW) = ", sprintf("%.2f", val[1]), " x (Year) + ", sprintf("%.2f", val[2])), size = 6, hjust = 0, color = "red", fontface = "bold", family = font) +
  # annotate("text", x=250, y=ycord[2], label=paste0("R = ", sprintf("%.2f",val[12]), " (p < 0.001) | Stdev = ", sprintf("%.2f",val[14])), size=5, hjust=0, color="red", family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", val2[12]), " (p-value < 0.001)"), size = 6, hjust = 0, colour = "red", family = font, fontface = "bold") +
  # annotate("text", x=250, y=ycord[3], label=paste0("Stdev = ", sprintf("%.2f",val[14])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=250, y=ycord[4], label=paste0("AMI = ", sprintf("%.3f",val[3]), " | CERES = ", sprintf("%.3f",val[4])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[3], label=paste0("Bias = ", sprintf("%.2f",val[8]), " (", sprintf("%.2f",val[9])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[4], label=paste0("RMSE = ", sprintf("%.2f",val[10]), " (", sprintf("%.2f",val[11])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[3], label=paste0("N = ", sprintf("%.0f",val[7]) ), size=6, hjust=0, color="black", family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[3], label = paste0("N = 32,311"), size = 6, hjust = 0, color = "black", family = font, fontface = "bold") +
  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X, Y)) +
  scale_x_continuous(minor_breaks = seq(2013, 2017, by = 1), breaks = seq(2013, 2017, by = 1), expand = c(0, 0), limits = c(2013, 2017)) +
  scale_y_continuous(minor_breaks = seq(0, 10, by = 2), breaks = seq(0, 10, by = 2), expand = c(0, 0), limits = c(0, 10)) +
  # labs(title = "Sunshine Duration  by  Sunshine Recorder  of  2013/01/01-2016/12/31") +
  labs(
    title = "가강수량 시계열"
    , subtitle = NULL
    , y = expression(paste(bold("Total  Precipitable  Water by GWNU  [cm]")))
    , x = expression(paste(bold("Time  [Year]")))
    , fill = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "black")
    , axis.title.x = element_text(face = "bold", size = 20, colour = "black")
    , axis.title.y = element_text(face = "bold", size = 20, colour = "black", angle = 90)
    , axis.text.x = element_text(face = "bold", size = 20, colour = "black")
    , axis.text.y = element_text(face = "bold", size = 20, colour = "black")
    , legend.title = element_text(face = "bold", size = 12, colour = "black") 
    , legend.position = c(0, 1), legend.justification = c(0, 0.9)
    , legend.key = element_blank()
    , legend.text = element_text(size = 12, face = "bold")
    , legend.background = element_blank()
    , text = element_text(family = font)
    , plot.margin = unit(c(9.5, 8, 0, 2), "mm")
    ) +
  ggsave(filename = saveImg, width = 12, height = 6, dpi = 600)


#***************************************************
# 운량 시계열
#***************************************************
X = gwnu_L2$dtXran
Y = 1 - (gwnu_L2$gwnu_dsr / gwnu_L2$tot)

# plot(X, Y)

val = perfEval(X, Y)
# sprintf("%.2f", val)

xcord = 2013.05
ycord = seq(0.97, 0, -0.1)

saveImg = sprintf("%s/%s_%s", globalVar$figConfig, serviceName, "GWNU_CA.png")

ggplot() +
  # coord_fixed(ratio=1) +
  theme_bw() +
  # geom_point(aes(X, Y)) +
  geom_point(aes(X, Y), shape = 21, colour = "black", fill = "white", size = 2.5) +
  annotate("text", x = xcord, y = ycord[1], label = paste0("(CA) = ", sprintf("%.2f", val[1]), " x (Year) + ", sprintf("%.2f", val[2])), size = 6, hjust = 0, color = "red", fontface = "bold", family = font) +
  # annotate("text", x=250, y=ycord[2], label=paste0("R = ", sprintf("%.2f",val[12]), " (p < 0.001) | Stdev = ", sprintf("%.2f",val[14])), size=5, hjust=0, color="red", family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", val2[12]), " (p-value < 0.001)"), size = 6, hjust = 0, colour = "red", family = font, fontface = "bold") +
  # annotate("text", x=250, y=ycord[3], label=paste0("Stdev = ", sprintf("%.2f",val[14])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=250, y=ycord[4], label=paste0("AMI = ", sprintf("%.3f",val[3]), " | CERES = ", sprintf("%.3f",val[4])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[3], label=paste0("Bias = ", sprintf("%.2f",val[8]), " (", sprintf("%.2f",val[9])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[4], label=paste0("RMSE = ", sprintf("%.2f",val[10]), " (", sprintf("%.2f",val[11])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[3], label=paste0("N = ", sprintf("%.0f",val[7]) ), size=6, hjust=0, color="black", family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[3], label = paste0("N = 32,311"), size = 6, hjust = 0, color = "black", family = font, fontface = "bold") +
  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X, Y)) +
  scale_x_continuous(minor_breaks = seq(2013, 2017, by = 1), breaks = seq(2013, 2017, by = 1), expand = c(0, 0), limits = c(2013, 2017)) +
  scale_y_continuous(minor_breaks = seq(0, 1, by = 0.2), breaks = seq(0, 1, by = 0.2), expand = c(0, 0), limits = c(0, 1)) +
  labs(
    title = "운량 시계열"
    , subtitle = NULL
    , y = expression(paste(bold("Cloud  Amount by GWNU")))
    , x = expression(paste(bold("Time  [Year]")))
    , fill = NULL
    ) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "black")
    , axis.title.x = element_text(face = "bold", size = 20, colour = "black")
    , axis.title.y = element_text(face = "bold", size = 20, colour = "black", angle = 90)
    , axis.text.x = element_text(face = "bold", size = 20, colour = "black")
    , axis.text.y = element_text(face = "bold", size = 20, colour = "black")
    , legend.title = element_text(face = "bold", size = 12, colour = "black")
    , legend.position = c(0, 1), legend.justification = c(0, 0.9)
    , legend.key = element_blank()
    , legend.text = element_text(size = 12, face = "bold")
    , legend.background = element_blank()
    , text = element_text(family = font)
    , plot.margin = unit(c(0, 8, 0, 2), "mm")
    ) +
  ggsave(filename = saveImg, width = 12, height = 6, dpi = 600)


#======================================
# Satellite vs obs vs gwnu (boxplot)
#======================================
X = data_L4$iors_dsr
Y = data_L4$coms_dsr
Z = data_L4$gwnu_dsr

Xmean = mean(X)
Ymean = mean(Y)
Zmean = mean(Z)

summary(X)
summary(Y)
summary(Z)

saveImg = sprintf("%s/%s_%s", globalVar$figConfig, serviceName, "Boxplot.png")

ggplot() +
  theme_bw() +
  geom_boxplot(aes('IORS', X), colour = '#F8877F', fill = '#F8877F', alpha = 0.2, width = 0.35) +
  geom_boxplot(aes('COMS', Y), colour = '#87B3FE', fill = '#87B3FE', alpha = 0.2, width = 0.35) +
  geom_boxplot(aes('GWNU', Z), colour = '#39C864', fill = '#39C864', alpha = 0.2, width = 0.35) +
  geom_point(aes('IORS', Xmean), colour = '#C57BFA', size = 4) +
  geom_point(aes('COMS', Ymean), colour = '#C57BFA', size = 4) +
  geom_point(aes('GWNU', Zmean), colour = '#C57BFA', size = 4) +
  annotate("text", x = 'COMS', y = 20.2340, label = "      Min.", size = 5.5, hjust = 0, vjust = 0.5, fontface = "bold", family = font) +
  annotate("text", x = 'COMS', y = 169.0000, label = "      25 %", size = 5.5, hjust = 0, vjust = 0.5, fontface = "bold", family = font) +
  annotate("text", x = 'COMS', y = 368.5000, label = "      50 %", size = 5.5, hjust = 0, vjust = 0.5, fontface = "bold", family = font) +
  annotate("text", x = 'COMS', y = 465.8417, label = "      Mean", size = 5.5, hjust = -0.4, vjust = 0.5, fontface = "bold", family = font, colour = '#C57BFA') +
  annotate("text", x = 'COMS', y = 594.6900, label = "      75 %", size = 5.5, hjust = 0, vjust = 0.5, fontface = "bold", family = font) +
  annotate("text", x = 'COMS', y = 1106.7800, label = "     Max.", size = 5.5, hjust = 0, vjust = 0.5, fontface = "bold", family = font) +
  scale_y_continuous(breaks = seq(0, 1200, by = 200), expand = c(0, 0), limits = c(0, 1200)) +
  labs(
    title = NULL
    , x = NULL
    , y = expression(paste(bold("Downward  Shortwave  Radiation  (DSR)  [Wm"^bold("-2") * "]")))
    , fill = NULL
    ) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "black")
    , axis.title.x = element_text(face = "bold", size = 19, colour = "black")
    , axis.title.y = element_text(face = "bold", size = 19, colour = "black", angle = 90)
    , axis.text.x = element_text(face = "bold", size = 19, colour = "black")
    , axis.text.y = element_text(face = "bold", size = 19, colour = "black")
    , legend.title = element_text(face = "bold", size = 12, colour = "black")
    , legend.position = c(0, 1), legend.justification = c(0, 0.9)
    , legend.key = element_blank()
    , legend.text = element_text(size = 12, face = "bold")
    , legend.background = element_blank()
    , text = element_text(family = font)
    , plot.margin = unit(c(0, 8, 0, 0), "mm")
    ) +
  ggsave(filename = saveImg, width = 7, height = 7, dpi = 600)








#==============================================================
# IEODO map.R
#==============================================================

#==================================
# Google Map point, Marker
#==================================

# ggmap::register_google(key = "AIzaSyCkYokUFIcH5OYDaYU0IrFLX89wX1o7-qc")

iorsGeoLat = c(32.955, 31.1277, 33.12061, 32.1229)
iorsGeoLon = c(129.0814, 120.3954, 126.26712, 125.18)
marker = data.frame(iorsGeoLon, iorsGeoLat)
center = c(125.5, 36)

# ggmap::get_map(
#   location = '서울'
#   , zoom = 14
#   , maptype = 'roadmap'
#   , source = 'google') %>%
#   ggmap()


saveImg = sprintf("%s/%s_%s", globalVar$figConfig, serviceName, "Map_IEODO.png")

map = get_googlemap(center, zoom = 6, maptype = "hybrid", markers = marker)
ggmap(map, extent = "device") +
  ggsave(filename = saveImg, width = 8, height = 8, dpi = 600)
  
#   png("OUTPUT//Map_IEODO.png", width = 800, height = 800, res = 100)
# dev.off()

#======================
#     Map
#======================
# ggmap(map)
# ggmap(map, extent = "device")
# ggmap(map, extent = 'panel')
# ggmap(map) +
#   #scale_x_longitude(xmin = 123, xmax = 132, step = 1) +
#   #scale_y_latitude(ymin = 32, ymax = 40, step = 1) +
#   theme(axis.title.x = element_text(face = "bold", size = 18, colour = "black")) +
#   theme(axis.title.y = element_text(face = "bold", size = 18, colour = "black", angle = 90)) +
#   theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black")) +
#   theme(axis.text.y = element_text(face = "bold", size = 15, colour = "black")) +
#   theme(text = element_text(face = "bold"))
#theme(text = element_text(family = font, face = "bold"))

#======================
#     Point name
#======================

saveImg = sprintf("%s/%s_%s", globalVar$figConfig, serviceName, "Map_point_name_AWS.png")

ggmap(map, extent = 'device') +
  geom_point(aes(x = 126.27, y = 33.12), colour = 'yellow', size = 3) +
  geom_point(aes(x = 120.40, y = 31.13), colour = 'yellow', size = 3) +
  geom_point(aes(x = 140.30, y = 30.49), colour = 'yellow', size = 3) +
  geom_point(aes(x = 125.18, y = 32.12), colour = 'yellow', size = 3) +
  # png("OUTPUT//Map_IEODO.png", width=800, height=800, res=100)
  # dev.off()
  #geom_text(aes(x = long, y = lat, label = name), hjust = 0.5, vjust = 0.25, nudge_x = 0.15, nudge_y = 0, size = 3,
  #          colour = 'white', family = "NanumBarunGothic", fontface = "bold", data = point_count) +
  #geom_text(aes(x = long, y = lat, label = name), hjust = 0.5, vjust = 0.25, nudge_x = 0.15, nudge_y = 0, size = 3,
  #          colour = 'white', family = "NanumBarunGothic", fontface = "bold", data = point_count) +
  #scale_x_longitude(xmin = 128, xmax = 132, step = 1) +
  #scale_y_latitude(ymin = 32, ymax = 39, step = 1) +
  labs(colour = "") +
  #annotate("text", x=128, y=38.6, label="AWS Point : Count", size=6, hjust=0.5, color="yellow", fontface="bold", family="NanumBarunGothic") +
  annotate("text", x = 128, y = 38.6, label = "AWS Point : Count", size = 6, hjust = 0.5, color = "yellow", fontface = "bold", family = fontKor) +
  #scale_colour_gradientn(colours = myPalette(11), limits=c(0, 200), na.value="#9E0142") +
  scale_colour_gradientn(colours = colorRamps::matlab.like(11), limits = c(0, 200), na.value = "#AA0000") +
  #scale_colour_gradientn(colours=matlab.like2(11), limits=c(0, 200), na.value="#BF0000") +
  theme(axis.title.x = element_text(face = "bold", size = 15, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 15, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 15, colour = "black")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 14, face = "bold", color = "white")) +
  theme(legend.background = element_blank()) +
  theme(text = element_text(family = font)) +
  ggsave(filename = saveImg, width = 8, height = 8, dpi = 600)

#================================
# Land/Ocean Map [DEM]
#================================
# 한반도
demData = marmap::getNOAA.bathy(121, 135.5, 31, 45, res = 1, keep = TRUE)

# 동아시아
demData = marmap::getNOAA.bathy(100, 150, 10, 50, res = 1, keep = TRUE)

autoplot(demData, geom=c("raster", "contour")) + 
  scale_fill_etopo()

#==========================================
#  ggplot convert : Height > 0 (land)
#                   Height < 0 (ocean)
#==========================================
data_ocean = data.table::fread("marmap_coord_121;31;135.5;45_res_1.csv", header = TRUE, stringsAsFactors = FALSE, data.table = FALSE) %>%
  magrittr::set_colnames(c("long", "lat", "alt"))
                           
                           
# data_ocean = data.table::fread("marmap_coord_100;10;150;50_res_1.csv", header = TRUE, stringsAsFactors = FALSE, data.table = FALSE) %>%
  # magrittr::set_colnames(c("long", "lat", "alt"))
  

data_ocean_L1 = data_ocean %>%
  dplyr::filter(alt < 0)


marmap::as.bathy(data_ocean_L1) %>%
  autoplot(geom=c("raster", "contour")) + 
  scale_fill_etopo()

data_ocean_L1 %>%
  ggplot(aes(x = long, y = lat, fill = alt)) +
  geom_tile()

data_ocean_L1 %>%
  ggplot(aes(x = long, y = lat, fill = alt)) +
  geom_tile() +
  guides(fill = guide_colorbar(barwidth = 1.25, barheight = 23)) +
  scale_fill_gradient2(limits = c(-7000, 0))

data_ocean_L1 = data_ocean %>%
  dplyr::filter(alt < 0)

data_ocean_L1 %>%
  ggplot(aes(x = long, y = lat, fill = alt)) +
  geom_point()

data_ocean %>%
  ggplot(aes(x = long, y = lat, fill = alt)) +
  geom_tile() +
  scale_fill_gradient2(low = "dodgerblue4", mid = "white", high = "darkgreen") +
  guides(fill = guide_colorbar(barwidth = 1.25, barheight = 23))

data_ocean %>%
  ggplot(aes(x = long, y = lat, fill = alt)) +
  geom_raster() +
  scale_fill_etopo() +
  guides(fill = guide_colorbar(barwidth = 1.25, barheight = 23)) +
  theme_map() +
  theme(legend.position = "right")


data_ocean %>%
  ggplot(aes(x = long, y = lat, fill = alt)) +
  geom_point()


data_ocean %>% ggplot(aes(x = long, y = lat, fill = alt)) +
  geom_raster() +
  # scale_fill_brewer()
  scale_fill_gradient2(limits = c(-7000, 0), breaks = c(-7000, -5000, -4000, -3000, -2000, -1000, -800, -600, -400, -200, -100, -80, -60, -40, -20, 0)) +
  # scale_fill_gradient2(low="dodgerblue4", mid="white", high="darkgreen") +
  guides(fill = guide_colorbar(barwidth = 1.25, barheight = 23))


data_ocean %>%
  ggplot(aes(x = long, y = lat, fill = alt)) +
  geom_raster() +
  scale_fill_etopo() +
  guides(fill = guide_colorbar(barwidth = 1.25, barheight = 23)) +
  theme_map() +
  theme(legend.position = "right")






#================================
# Land Map
#================================
#=======================
# adm : shp
#=======================
kor_shp = maptools::readShapePoly("Map/KOR_adm/KOR_adm0.shp")
jpn_shp = maptools::readShapePoly("Map/JPN_adm/JPN_adm0.shp")
chn_shp = maptools::readShapePoly("Map/CHN_adm/CHN_adm0.shp")
prk_shp = maptools::readShapePoly("Map/PRK_adm/PRK_adm0.shp")

#================================
# GSHHG (NOAA) : shp, bin
#================================
# gshhp_shp = readShapePoly("Map/NOAA/gshhg-shp-2.3.5-1//GSHHS_shp//i/GSHHS_i_L1.shp")
# gshhp_shp = readShapePoly("Map/NOAA/gshhg-shp-2.3.5-1//GSHHS_shp//i/GSHHS_i_L2.shp")
# gshhp_shp = readShapePoly("Map/NOAA/gshhg-shp-2.3.5-1//GSHHS_shp//i/GSHHS_i_L3.shp")
# gshhp_shp = readShapePoly("Map/NOAA/gshhg-shp-2.3.5-1//GSHHS_shp//i/GSHHS_i_L4.shp")
# gshhp_shp = readShapePoly("Map/NOAA/gshhg-shp-2.3.5-1//GSHHS_shp//i/GSHHS_i_L5.shp")
# gshhp_shp = readShapePoly("Map/NOAA/gshhg-shp-2.3.5-1//GSHHS_shp//i/GSHHS_i_L6.shp")

gshhg_bin = maptools::getRgshhsMap("Map/NOAA/gshhg-bin-2.3.4/gshhs_f.b", xlim = c(115, 140), ylim = c(30, 45))
s#wdb_rivers_bin  = getRgshhsMap("Map//NOAA//gshhg-bin-2.3.4//wdb_rivers_f.b", xlim=c(124, 132), ylim=c(32, 40))
#wdb_borders_bin = getRgshhsMap("Map//NOAA//gshhg-bin-2.3.4//wdb_borders_f.b", xlim=c(124, 132), ylim=c(32, 40))

#=======================
# NOAA : shp
#=======================
# ocean_shp     = readShapePoly("Map/NOAA/ne_10m_ocean/ne_10m_ocean.shp")
# land_shp      = readShapePoly("Map/NOAA/ne_10m_land/ne_10m_land.shp")
# coastline_shp = readShapeSpatial("Map/NOAA/ne_10m_coastline/ne_10m_coastline.shp")


gshhg_bin = fortify(gshhg_bin)
jpn_shp = fortify(jpn_shp)
prk_shp = fortify(prk_shp)
kor_shp = fortify(kor_shp)
chn_shp = fortify(chn_shp)

dplyr::tbl_df(gshhg_bin)
dplyr::tbl_df(chn_shp)

#=======================
# Map plot
#=======================
ggplot() +
  geom_polygon(data = gshhg_bin, aes(x = long, y = lat, group = group), fill = NA, colour = "black")

ggplot() +
  geom_polygon(data = jpn_shp, aes(x = long, y = lat, group = group), fill = NA, colour = "red") +
  geom_polygon(data = prk_shp, aes(x = long, y = lat, group = group), fill = NA, colour = "blue") +
  geom_polygon(data = kor_shp, aes(x = long, y = lat, group = group), fill = NA, colour = "black") +
  geom_polygon(data = chn_shp, aes(x = long, y = lat, group = group), fill = NA, colour = "green")


ggplot() +
  geom_tile(aes(x = long, y = lat, colour = alt), data = data_ocean) +
  scale_colour_etopo() +

ggplot() +
  geom_tile(aes(x = long, y = lat, colour = alt), data = data_ocean) +
  scale_colour_etopo() +
  #   geom_path(aes(x = long, y = lat, group=group), data=gshhg_bin, colour="black") +
  guides(colour = guide_colorbar(barwidth = 1.25, barheight = 23))
  #   geom_path(aes(x = long, y = lat, group=group), data=gshhg_bin, colour="black") +
  guides(colour = guide_colorbar(barwidth = 1.25, barheight = 23))


#=======================
# CERES mapping
#=======================
# data_ceres = fread("ceres_0309.dat",  header=F, stringsAsFactors=T, data.table=F)
data_ceres = data.table::fread("ceres_0318.dat", header = FALSE, stringsAsFactors = TRUE, data.table = FALSE)
colnames(data_ceres) = c("long", "lat", "var")

X = data_ceres$long
Y = data_ceres$lat
Z = data_ceres$var

interp_XYZ = interp(X, Y, Z, nx = 100, ny = 100)
raster_XYZ = raster(interp_XYZ)

data_ceres_L1 = rasterToPoints(raster_XYZ)
colnames(data_ceres_L1) <- c("long", "lat", "var")
data_ceres_L1 = data.frame(data_ceres_L1)

data_ceres_L1 = data_ceres_L1 %>% filter(var > 0)

korea = c(left = 115, bottom = 30, right = 140, top = 45)    # Korea
map = get_map(korea, zoom = 5, maptype = 'satellite')
ggmap(map)

longitude <- seq(115, 140, 3)
latitude <- seq(30, 45, 2)
x_longitude <- unlist(lapply(longitude, function(x) ifelse(x < 0, paste0(x, "°W"), ifelse(x > 0, paste0(x, "°E"), x))))
y_latitude <- unlist(lapply(latitude, function(x) ifelse(x < 0, paste0(x, "°S"), ifelse(x > 0, paste0(x, "°N"), x))))


ggmap(map) +
  # ggplot() +
  geom_blank() +
  geom_tile(aes(x = iorsGeoLon, y = iorsGeoLat, fill = var), data = data_ceres_L1) +
  scale_fill_gradientn(colours = matlab.like(11), limits = c(0, 1000), na.value = "#AA0000") +
  guides(fill = guide_colorbar(barwidth = 1.25, barheight = 25)) +
  geom_path(aes(x = iorsGeoLon, y = iorsGeoLat, group = group), data = kor_shp, colour = 'black') +
  geom_path(aes(x = iorsGeoLon, y = iorsGeoLat, group = group), data = jpn_shp, colour = 'black') +
  geom_path(aes(x = iorsGeoLon, y = iorsGeoLat, group = group), data = chn_shp, colour = 'black') +
  geom_path(aes(x = iorsGeoLon, y = iorsGeoLat, group = group), data = prk_shp, colour = 'black') +
  # labs(x = expression(paste(bold("Longitude")))) +
  # labs(y = expression(paste(bold("Latitude")))) +
  labs(x = "") +
  labs(y = "") +
  labs(fill = "") +
  theme(axis.title.x = element_text(face = "bold", size = 15, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 15, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 15, colour = "black")) +
  scale_x_continuous(breaks = longitude, labels = x_longitude, limits = c(120, 136)) +
  scale_y_continuous(breaks = latitude, labels = y_latitude, limits = c(30, 44)) +
  theme(legend.position = "right") +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 14, face = "bold", color = "black")) +
  theme(legend.background = element_blank()) +
  theme(text = element_text(family = font)) +
  # png("OUTPUT//CERES_0309.png", width=800, height=600, res=100)
  png("OUTPUT/CERES_0318.png", width = 800, height = 600, res = 100)
dev.off()















#==============================================================
# 시계열 분석.R
#==============================================================

#================================================
# AERONET  L1.5 read
# 에어로넷 L1B 자료 읽기
#================================================
# data = read.csv('Irradiance/130101_161231_Ieodo_Station.lev15', header = FALSE, sep = ",", skip = 5) %>%
#   as.tibble()
# 
# data
# 
# 
# # start ~ end  :  (2013.11.30 05:39:51) ~ (2016.06.30 06:14:07)
# c01 = as.numeric(substr(data[, 1], 1, 2))
# c02 = as.numeric(substr(data[, 1], 4, 5))
# c03 = as.numeric(substr(data[, 1], 7, 10))
# c04 = as.numeric(substr(data[, 2], 1, 2))
# c05 = as.numeric(substr(data[, 2], 4, 5))
# c06 = as.numeric(substr(data[, 2], 7, 8))
# data2 = data.frame(c03, c02, c01, c04, c05, c06)
# 
# data_L1 = data.frame(data2, data[, c(-1, -2)])
# colnames(data_L1) = c("year", "month", "day", "hour", "min", "sec", "Julian_Day", "AOT_1640", "AOT_1020", "AOT_870", "AOT_675", "AOT_667", "AOT_555", "AOT_551", "AOT_532", "AOT_531", "AOT_500", "AOT_490", "AOT_443", "AOT_440", "AOT_412", "AOT_380", "AOT_340", "water")
# data_L2 = data_L1[, c(1, 2, 3, 4, 5, 6, 7, 14, 24)]
# head(data_L2)


colList = c('sDate', 'sTime', 'Julian_Day', 'AOT_1640', 'AOT_1020', 'AOT_870', 'AOT_675', 'AOT_667', 'AOT_555', 'AOT_551', 'AOT_532', 'AOT_531', 'AOT_500', 'AOT_490', 'AOT_443', 'AOT_440', 'AOT_412', 'AOT_380', 'AOT_340', 'Water', 'TripletVar_1640', 'TripletVar_1020', 'TripletVar_870', 'TripletVar_675', 'TripletVar_667','TripletVar_555', 'TripletVar_551', 'TripletVar_532', 'TripletVar_531', 'TripletVar_500','TripletVar_490', 'TripletVar_443', 'TripletVar_440', 'TripletVar_412', 'TripletVar_380','TripletVar_340', 'WaterError', 'Angstrom_440_870', 'Angstrom_380_500', 'Angstrom_440_675','Angstrom_500_870', 'Angstrom_340_440', 'Angstrom_Polar_440_675', 'Last_Processing_Date', 'Solar_Zenith_Angle')

data_L3 = read.csv('Irradiance/130101_161231_Ieodo_Station.lev15', header = FALSE, sep = ",", skip = 5) %>%
  magrittr::set_colnames(colList) %>% 
  as.tibble() %>%
  dplyr::mutate(
    sDateTime = paste(sDate, sTime, sep = " ")
    , dtDateTime = readr::parse_datetime(sDateTime, "%d:%m:%Y %H:%M:%S")
    , dtYear = lubridate::year(dtDateTime)
    , dtMonth = lubridate::month(dtDateTime)
    , dtDay = lubridate::day(dtDateTime)
    , dtHour = lubridate::hour(dtDateTime)
    , dtXran = lubridate::decimal_date(dtDateTime)
  ) %>%
  dplyr::filter(
    dplyr::between(dtHour, 8, 18)
  )

data_L3

#================================================
#     AERONET  L1.5 read
#================================================
# data_L3 = data_L2 %>%
#   filter(hour >= 8 & hour <= 18) %>%
#   mutate(
#     xran = ISOdatetime(year, month, day, hour, min, sec, tz = "")
#   )
# 
# head(data_L3)

data_L3 %>% 
  ggplot(aes(x = dtXran, y = AOT_551, label = dtXran)) +
  geom_text(colour = "yellow", hjust = 0.5, vjust = 0.5, fontface = "bold", size = 4, nudge_y = 0.04) +
  geom_point()

#=================================================
# Histogram plot (AERONET)
#================================================
data_L3_count = data_L3 %>%
  # filter(hour >= 8 & hour <= 18) %>%
  group_by(dtYear) %>%
  summarise(cnt = n())


# data_L3_count
# sum(data_L3_count[, 2])

data_L3_count %>%
  ggplot(aes(x = dtYear, y = cnt, label = round(cnt, 0), fill = cnt)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(colour = "white", hjust = 0.5, vjust = 0.5, size = 5, nudge_y = -10, family = fontEng, fontface = "bold") +
  labs(
    title = "Histogram"
    , x ="Time [Year]"
    , y = "Count"
    , fill = "Count"
    ) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "black")
    , axis.title.x = element_text(size = 16, colour = "black")
    , axis.title.y = element_text(size = 16, colour = "black", angle = 90)
    , axis.text.x = element_text(size = 16, colour = "black")
    , axis.text.y = element_text(size = 16, colour = "black")
    , text = element_text(family = fontEng)
    )


#================================================
# Irradiance read
#================================================
data_L2 = read.csv('Irradiance/DATA_L3', header = FALSE, sep = "") %>%
  magrittr::set_colnames(c("sDate", "dtXran", "meanDay", "cumDay", "cnt")) %>%
  as.tibble() %>%
  dplyr::mutate(
    dtDateTime = readr::parse_datetime(as.character(sDate), "%Y%m%d")
    , dtYear = lubridate::year(dtDateTime)
  ) %>%
  dplyr::filter(
    dplyr::between(dtYear, 2005, 2013)
  )

data_L2

# head(data)
# c01 = as.numeric(substr(data[, 1], 1, 4))
# c02 = as.numeric(substr(data[, 1], 5, 6))
# c03 = as.numeric(substr(data[, 1], 7, 8))
# data2 = data.frame(c01, c02, c03)
# 
# data_L1 = data.frame(data2, data[, -1])
# colnames(data_L1) = 
# head(data_L1)
# 
# data_L2 = data_L1 %>%
#   filter(time >= 2005 & time <= 2013)
# 
# head(data_L2)
# summary(data_L2)

#=================================================
# Histogram plot
#================================================
data_L2_count = data_L2 %>%
  group_by(dtYear) %>%
  summarise(cntForYear = n())

saveImg = sprintf("%s/%s_%s", globalVar$figConfig, serviceName, "Histogram.png")

data_L2_count %>%
  ggplot(aes(x = dtYear, y = cntForYear, label = round(cntForYear, 0), fill = factor(dtYear))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(colour = "white", hjust = 0.5, vjust = 0.5, size = 5, nudge_y = -20, family = fontEng, fontface = "bold") +
  scale_x_continuous(breaks = seq(2005, 2013, by = 1), limits = c(2004.5, 2013.5)) +
  labs(
    title = "Histogram"
    , x ="Date [Year]"
    , y = "Count"
    , fill = "Year"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "black")
    , axis.title.x = element_text(size = 16, colour = "black")
    , axis.title.y = element_text(size = 16, colour = "black", angle = 90)
    , axis.text.x = element_text(size = 16, colour = "black")
    , axis.text.y = element_text(size = 16, colour = "black")
    , text = element_text(family = fontEng)
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
  


# # ggplot(data = data_L2_count, aes(x = dtYear, y = cntForYear, label = round(cntForYear, 0))) +
#   geom_bar(aes(colour = factor(col), fill = factor(col)), stat = "identity", position = "dodge") +
#   #  geom_label(aes(fill=factor(col)), colour="white", fontface="bold", size=5, hjust=0.5, vjust=0.5, nudge_y=-15) +
#   geom_text(colour = "white", hjust = 0.5, vjust = 0.5, fontface = "bold", size = 5, nudge_y = -15) +
#   scale_x_continuous(breaks = seq(2005, 2012, by = 1), limits = c(2004.5, 2012.5)) +
#   xlab(expression(paste(bold("Time [Year]")))) +
#   ylab(expression(paste(bold("Count")))) +
#   ggtitle("Histogram \n") +
#   theme(plot.title = element_text(face = "bold", size = 20, color = "black")) +
#   theme(axis.title.x = element_text(face = "bold", size = 16, colour = "black")) +
#   theme(axis.title.y = element_text(face = "bold", size = 16, colour = "black", angle = 90)) +
#   theme(axis.text.x = element_text(face = "bold", size = 16, colour = "black")) +
#   theme(axis.text.y = element_text(face = "bold", size = 16, colour = "black")) +
#   scale_fill_discrete(guide = F) +
#   scale_colour_discrete(guide = F) # +
# #png("Histogram.png", width = 700, height = 600, res = 100)
# #dev.off()

#=================================================
# Time plot
#================================================

XX = data_L2$dtXran
YY = data_L2$meanDay

lmFit = lm(YY ~ XX, data = data_L2)
# summary(lmFit)
# head(XX)

NN = length(YY); NN
inter = round(lmFit$coef[1], 2); inter
slope = round(lmFit$coef[2], 2); slope
corr = round(cor(XX, YY), 2); corr
det = (round(cor(XX, YY), 2))^2; det

font = "New Century Schoolbook"

data_L2 %>% ggplot(aes(x = XX, y = YY)) +
  geom_point() +
  annotate("text", x = 2005, y = 1200, label = "(DSR) = - 5.82 × (Time) + 12140.49", size = 4.2, hjust = 0, color = "red", fontface = "bold", family = font) +
  annotate("text", x = 2005, y = 1100, label = "bold(R~\"=\"~ \"- 0.06\"~\"(p < 0.01)\")", parse = T, size = 4.2, hjust = 0, color = "red", family = font) +
  annotate("text", x = 2005, y = 1000, label = "bold(N ~\"=\"~ \"2161\")", parse = T, size = 4.2, hjust = 0, family = font) +

  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
  stat_smooth(method = "lm", color = "red", se = F) +
  scale_x_continuous(breaks = seq(2005, 2013, by = 1), limits = c(2005, 2013)) +
  scale_y_continuous(breaks = seq(0, 1200, by = 200), limits = c(0, 1200)) +
  ggtitle("2015.01.01~2012.12.31 (QC o) \n Day Mean \n") +
  xlab(expression(paste(bold("Time [Year]")))) +
  ylab(expression(paste(bold("Downward Shortwave Radiation at Surface [Wm"^bold("-2") * "]")))) +
  theme(plot.title = element_text(face = "bold", size = 20, color = "black")) +
  theme(axis.title.x = element_text(face = "bold", size = 16, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 16, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 16, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 16, colour = "black")) +
  theme(legend.title = element_text(face = "bold", size = 12, colour = "black")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 10, face = "bold")) +
  theme(legend.background = element_blank()) +
  theme(text = element_text(family = font)) # +
#png("Day Mean.png", width = 700, height = 600, res = 100)
#dev.off()
























#==============================================================
# 20161019_Comparistion Test.R
# 이어도 종합해양과학기지 체류 연구
# 비교 관측 자료
#==============================================================


#========================================================
# IEODO : Comparison Observation [2016.10.19]
#========================================================
data = read.csv('IEODO_1sec_20161019.dat', header = F, sep = "")
tbl_df(data)
c01 = as.numeric(substr(data[, 1], 1, 4))
c02 = as.numeric(substr(data[, 1], 6, 7))
c03 = as.numeric(substr(data[, 1], 9, 10))
c04 = as.numeric(substr(data[, 2], 1, 2))
c05 = as.numeric(substr(data[, 2], 4, 5))
c06 = as.numeric(substr(data[, 2], 7, 8))
data2 = data.frame(c01, c02, c03, c04, c05, c06)

data_obs = data.frame(data2, data[, c(-1, -2, -4)])
colnames(data_obs) = c("year", "month", "day", "hour", "min", "sec", "obs")
tbl_df(data_obs)

loc1 = which(data_obs$sec == 01)
loc2 = which(data_obs$sec == 00)
plot(loc1, loc2)

mean = NULL
for (i in 1:length(aa)) {
  mean[i] = mean(data_obs[loc1[i]:loc2[i], 7], na.rm = T)
  cat(i, mean[i], "\n")
}


#==========================
#   QC (o)
#==========================
data_obs_L1 = data_obs
data_obs_L1[88:112, 7] = NA
data_obs_L1[1001:1090, 7] = NA
data_obs_L1[1894:1980, 7] = NA
data_obs_L1[2398:2416, 7] = NA
data_obs_L1[2801:2819, 7] = NA
data_obs_L1[3072:3087, 7] = NA
data_obs_L1[3373:3384, 7] = NA
data_obs_L1[3672:3689, 7] = NA
data_obs_L1[4174:4215, 7] = NA
data_obs_L1[5315:5389, 7] = NA
data_obs_L1[7450:7537, 7] = NA
data_obs_L1[8090:8100, 7] = NA

loc1 = which(data_obs_L1$sec == 01)
loc2 = which(data_obs_L1$sec == 00)
plot(loc1, loc2)

mean2 = NULL
for (i in 1:length(aa)) {
  mean2[i] = mean(data_obs_L1[loc1[i]:loc2[i], 7], na.rm = T)
  cat(i, mean[i], "\n")
}
mean2

plot(mean, mean2, xlim = c(0, 300), ylim = c(0, 300))


#==========================
#   merge
#==========================

data_obs = read.csv('IEDO_1min_20161019.txt', header = F, sep = "")
tbl_df(data_obs)
colnames(data_obs) = c("year", "month", "day", "hour", "min", "IEODO", "Reference", "Reference_QC")
tbl_df(data_obs)

data_obs_L1 = data_obs %>%
  mutate(xran = ISOdatetime(year, month, day, hour, min, sec = 00, tz = ""))
tbl_df(data_obs_L1)

data_obs_L1[103:106, 6] = NA

plot(Y3)
plot(Y)
#========================================
#     Time series (dot)
#========================================
xran_min = as.POSIXct("2016-10-19 12:30:00")
xran_max = as.POSIXct("2016-10-19 14:50:00")
X = data_obs_L1$xran
Y = data_obs_L1$IEODO
# Y2 = data_obs_L1$Reference
Y2 = data_obs_L1$Reference_QC

diff = Y2 - Y
data.frame(X, Y, Y2, diff)
per_diff = (Y2 - Y) / Y2
data.frame(data_obs_L1, diff)
plot(X, Y, type = 'l', ylim = c(130, 300))
points(X, Y2, col = 'blue', type = 'l')


png(file = paste("Comparison_Obs_dot_diff", ".png", sep = ""), 1200, 600)
par(mar = c(5.1, 7, 5.1, 7), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.5, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
plot(X, Y, ylim = c(120, 330), pch = 21, bg = 'black', col = 'black', cex = 1.4, xlab = "", ylab = "", yaxt = "n", xaxt = "n", xaxs = "i", yaxs = "i", xlim = c(xran_min, xran_max),
     main = '2016.10.19  12:30~14:50')
axis.POSIXct(1, X, format = "%H:%M", at = seq(xran_min, xran_max, by = "20 min"), las = 1)
mtext('Time  [Hour : Minute]', side = 1, col = 'black', line = 3, cex = 2.2)
axis(2, col = 'black', col.axis = 'black', las = 1, at = seq(0, 500, 30)); mtext(expression(paste(bold("Global  Solar  Radiation [Wm"^bold("-2") * "]"))), side = 2, col = 'black', line = 4, cex = 2.2)
# abline(h=seq(0, 500, 30), lw=1, col='lightgrey', lty="dotted")
# abline(v=seq(xran_min, xran_max, by="20 min"), lw=1, col='lightgrey', lty="dotted")
points(X, Y2, pch = 21, bg = 'red', col = 'red', cex = 1.5)
par(new = T)
plot(X, diff, col = 'green', pch = 21, bg = 'green', cex = 1.5, ylim = c(-20, 20), xaxt = "n", yaxt = "n", xlab = '', ylab = ' ', type = 'l', lwd = 3, xlim = c(xran_min, xran_max), xaxs = "i", yaxs = "i")
axis(4, col = 'green', col.axis = 'green', las = 1); mtext(expression(paste(bold("Diff.  [Wm"^bold("-2") * "]"))), side = 4, line = 3, cex = 2.2, col = 'green')
legend("topleft", legend = c("IEODO", "Reference"), pch = c(21, 21), col = c('black', 'red'),
       cex = 1.4, pt.cex = 1.5, text.font = 2, pt.bg = c('black', 'red'), bty = "n", xpd = T, text.col = c('black', 'red'))
legend("topright", legend = c("Diff"), col = c('green'), text.col = c('green'),
       cex = 1.4, pt.cex = 1.5, text.font = 2, lwd = 2, bty = "n", xpd = T)
dev.off()

png(file = paste("Comparison_Obs_line_diff", ".png", sep = ""), 1200, 600)
par(mar = c(5.1, 7, 5.1, 7), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.5, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
plot(X, Y, ylim = c(120, 330), pch = 21, bg = 'black', col = 'black', cex = 1.4, xlab = "", ylab = "", yaxt = "n", xaxt = "n", xaxs = "i", yaxs = "i", xlim = c(xran_min, xran_max),
     main = '2016.10.19  12:30~14:50', type = 'l', lwd = 3)
axis.POSIXct(1, X, format = "%H:%M", at = seq(xran_min, xran_max, by = "20 min"), las = 1)
mtext('Time  [Hour : Minute]', side = 1, col = 'black', line = 3, cex = 2.2)
axis(2, col = 'black', col.axis = 'black', las = 1, at = seq(0, 500, 30)); mtext(expression(paste(bold("Global  Solar  Radiation [Wm"^bold("-2") * "]"))), side = 2, col = 'black', line = 4, cex = 2.2)
# abline(h=seq(0, 500, 30), lw=1, col='lightgrey', lty="dotted")
# abline(v=seq(xran_min, xran_max, by="20 min"), lw=1, col='lightgrey', lty="dotted")
points(X, Y2, pch = 21, bg = 'red', col = 'red', cex = 1.5, type = 'l', lwd = 3)
par(new = T)
plot(X, diff, col = 'green', pch = 21, bg = 'green', cex = 1.5, ylim = c(-20, 20), xaxt = "n", yaxt = "n", xlab = '', ylab = ' ', type = 'l', lwd = 3, xlim = c(xran_min, xran_max), xaxs = "i", yaxs = "i")
axis(4, col = 'green', col.axis = 'green', las = 1); mtext(expression(paste(bold("Diff.  [Wm"^bold("-2") * "]"))), side = 4, line = 3, cex = 2.2, col = 'green')
legend("topleft", legend = c("IEODO", "Reference"), pch = c(21, 21), col = c('black', 'red'),
       cex = 1.4, pt.cex = 1.5, text.font = 2, pt.bg = c('black', 'red'), bty = "n", xpd = T, text.col = c('black', 'red'))
legend("topright", legend = c("Diff"), col = c('green'), text.col = c('green'),
       cex = 1.4, pt.cex = 1.5, text.font = 2, lwd = 2, bty = "n", xpd = T)
dev.off()


#========================================
#     Diff.  % Diff.
#========================================
diff = YY - Y
per_diff = ((YY - Y) / YY) * 100.0
summary(diff)

png(file = paste("OUTPUT//Comparison_Obs_Diff", ".png", sep = ""), 800, 600)
par(mar = c(5.1, 7, 5.1, 7), cex.main = 1.8, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
plot(X, diff, pch = 21, bg = cols[3], col = 'white', cex = 2.3, xlab = "", ylab = "", yaxt = "n",
     xaxt = "n", main = paste('Diff. : GWNU - IEODO\n % Diff. : ((GWNU-IEODO) / GWNU)*100.0'))
axis.POSIXct(1, X, format = "%H:%M", at = seq(as.POSIXct("2016-07-18 12:10:00"), as.POSIXct("2016-07-18 13:00:00"), by = "10 min"), las = 1)
mtext('Year  [Time]', side = 1, col = 'black', line = 3, cex = 1.8)
axis(2, col = cols[3], col.axis = cols[3], las = 1); mtext(expression(paste(bold("Diff.  [Wm"^bold("-2") * "]"))), side = 2, col = cols[3], line = 3, cex = 2.0)
grid(nx = 0, ny = NULL, lw = 1, col = 'lightgrey', lty = "dotted"); abline(v = seq(as.POSIXct("2016-07-18 12:10:00"), as.POSIXct("2016-07-18 13:00:00"), by = "10 min"), lw = 1, col = 'lightgrey', lty = "dotted")
points(X, YY, pch = 21, bg = cols[2], col = 'white', cex = 2.3)
par(new = T)
plot(X, per_diff, pch = 21, bg = cols[4], col = 'white', cex = 2.3, yaxt = "n", xlab = "", ylab = "", xaxt = "n", bty = "n")
axis(4, col = cols[4], col.axis = cols[4], las = 1); mtext(expression(paste(bold("% Diff.  [%]"))), side = 4, col = cols[4], line = 4, cex = 2.0)
legend("topright", legend = c("Diff.", "% Diff."), pch = c(21, 21), col = c('white', 'white'),
       cex = 1.3, pt.cex = 2.3, text.font = 1, pt.bg = c(cols[c(3, 4)]))
dev.off()

#========================================
#     1:1 scatter plot
#========================================
png(file = paste("OUTPUT//Comparison_Obs_scatter", ".png", sep = ""), 700, 700)
par(mar = c(5.1, 7, 5.1, 5.1), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
plot(Y, YY, xlim = c(260, 380), ylim = c(260, 380), pch = 21, bg = cols[1], col = 'white', cex = 2.3, xlab = expression(paste(bold("IEODO DSR at Surface [Wm"^bold("-2") * "]"))),
     ylab = "", main = paste('2016.07.17 (12:10~12:59)'), las = 1)
mtext(expression(paste(bold("GWNU DSR at Surface  [Wm"^bold("-2") * "]"))), side = 2, line = 4, cex = 2.0)
grid(lw = 1, col = 'lightgrey', lty = "dotted")
lmFit = lm(YY ~ Y)
abline(lmFit, col = 'red', font = 2, lw = 2); abline(a = 0, b = 1, lty = 2, lw = 2, col = 'blue')
text(260, 380, paste("(GWNU) =", round(lmFit$coefficients[2], 2), "x (IEODO) +", round(lmFit$coefficients[1], 2)), adj = 0, col = 'red', font = 2, cex = 1.3)
text(260, 370, expression(bold(R^"2")), adj = 0, font = 2, col = 'red', cex = 1.3)
text(260, 370, paste("      =", round((cor(Y, YY)^2), 3), "(p < 0.001)"), adj = 0, col = 'red', font = 2, cex = 1.3)
text(260, 360, paste("Bias =", round(accuracy(Y, YY)[, 1], 2), "(", round(accuracy(Y, YY)[, 4], 2), "% )"), adj = 0, col = 'orange', font = 2, cex = 1.3)
text(260, 350, paste("RMSE =", round(accuracy(Y, YY)[, 2], 2), "(", round((accuracy(Y, YY)[, 2] / mean(YY)) * 100.0, 2), "% )"), adj = 0, col = 'orange', font = 2, cex = 1.3)
text(260, 340, paste("N = ", round(length(Y), 0)), adj = 0, col = 'black', font = 2, cex = 1.3)
dev.off()


X = data_obs$ieodo  # IEODO
Y = data_obs$gwnu   # Reference

lmFit = lm(Y ~ X)

N = length(X)
Bias = accuracy(X, Y)[, 1]
per_Bias = accuracy(X, Y)[, 4]
RMSE = accuracy(X, Y)[, 2]
per_RMSE = (RMSE / mean(Y)) * 100.0

N; round(Bias, 2); round(per_Bias, 2); round(RMSE, 2); round(per_RMSE, 2); round(cor(X, Y), 3); round(coef(lmFit)[1], 2); round(coef(lmFit)[2], 2)


ggplot() +
  coord_fixed(ratio = 1) +
  theme_bw() +
  geom_point(aes(X, Y), size = 4) +
  # stat_bin2d(bins=200) +
  # scale_fill_gradientn(colours = myPalette(11), limits=c(0, 20), na.value="#9E0142") +
  # scale_fill_gradientn(colours=matlab.like(11), limits=c(0, 40), na.value="#AA0000") +
  # scale_fill_gradientn(colours=matlab.like2(11), limits=c(0, 40), na.value="#BF0000") +
  annotate("text", x = 255, y = 395, label = "(IEODO) = 1.04 x (CERES) - 14.97", size = 5, hjust = 0, color = "red", fontface = "bold", family = font) +
  annotate("text", x = 255, y = 385, label = "bold(R ~\"=\"~ \"0.998\"~\"(p < 0.001)\")", parse = T, size = 5, hjust = 0, color = "red", family = font) +
  annotate("text", x = 255, y = 375, label = "bold(Bias ~\"=\"~ \"- 3.24\"~Wm^\"-2\")", parse = T, size = 5, hjust = 0, family = font) +
  annotate("text", x = 255, y = 365, label = "bold(RMSE ~\"=\"~ \"1.22\"~Wm^\"-2\")", parse = T, size = 5, hjust = 0, family = font) +
  annotate("text", x = 255, y = 355, label = "bold(N ~\"=\"~ \"50\")", parse = T, size = 5, hjust = 0, family = font) +
  # annotate("text", x=200, y=980, label="bold(RMSE ~\"=\"~ \"63.22\" ~Wm^\"-2\"~\"(21.69\"~\"%)\")", parse=T, size=4.2, hjust=0, family=font) +
  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X, Y)) +
  scale_x_continuous(breaks = seq(250, 400, by = 30), expand = c(0, 0), limits = c(250, 400)) +
  scale_y_continuous(breaks = seq(250, 400, by = 30), expand = c(0, 0), limits = c(250, 400)) +
  labs(title = "") +
  labs(y = expression(paste(bold("Reference  Measurement  [Wm"^bold("-2") * "]")))) +
  labs(x = expression(paste(bold("IEODO  Measurement [Wm"^bold("-2") * "]")))) +
  # labs(fill = "Count") +
  labs(fill = NULL) +
  theme(plot.title = element_text(face = "bold", size = 20, color = "black")) +
  theme(axis.title.x = element_text(face = "bold", size = 20, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 20, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 20, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 20, colour = "black")) +
  theme(legend.title = element_text(face = "bold", size = 12, colour = "black")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 0.9)) +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  theme(legend.background = element_blank()) +
  theme(text = element_text(family = font)) +
  theme(plot.margin = unit(c(0, 7, 0, 0), "mm")) +
  # png("FIG/albedo.png", width=600, height=600, res=100)
  png("OUTPUT/Comparison_Obs_scatter_ggplot2.png", width = 600, height = 600, res = 100)
dev.off()


#==============================================================
# 운고_운량 분석.R
#==============================================================

library(forecast)

#===========================================
#     Option
#===========================================
cols = brewer.pal(8, 'Set2')

#============================================
#     Irr + Celio  -->  DATA merge
#============================================
data_irr = read.csv('./Irradiance/DATA_L1', header = F, sep = "")
data_irr = data_irr[, c(-8)]
colnames(data_irr) = c("year", "month", "day", "hour", "min", "sec", "DSR")
head(data_irr)

# data_cel = read.csv('./Celiometer/DATA', header=F, sep="")

# c01 = as.numeric(substr(data_cel[,1], 1, 4))
# c02 = as.numeric(substr(data_cel[,1], 6, 7))
# c03 = as.numeric(substr(data_cel[,1], 9, 10))
# c04 = as.numeric(substr(data_cel[,2], 1, 2))
# c05 = as.numeric(substr(data_cel[,2], 4, 5))
# c06 = as.numeric(substr(data_cel[,2], 7, 8))
# data2 = data.frame(c01, c02, c03, c04, c05, c06)

# data_cel = data.frame(data2, data_cel[ , c(-1,-2)])
# colnames(data_cel) = c("year", "month", "day", "hour", "min", "sec", "CA_1", "CH_1", "CA_2", "CH_2", "CA_3", "CH_3")
#
# data = merge(x=data_irr, y=data_cel, by=c("year", "month", "day", "hour", "min", "sec"), all.x=T)
# tbl_df(data)
#
# data[is.na(data)==T] = -999.0


tail(data)

#write.table(data, file='DATA_merge', sep=" ", row.names=F, col.names=F)


#========================================================================
#     data  ==>    data_L2  ==>  data_L3_not_cloudy (QC after)
#                           ==>  data_L3_cloudy (QC after + Celiometer)
#                           ==>  not_data_L3 (QC before == data_L2)
#     Fortran90, Csehll
#
#     2005.01.01 ~ 2015.12.31
#========================================================================
data_L2 = fread("not_data_L3", header = F, stringsAsFactors = T, data.table = F)
colnames(data_L2) = c("time", "xran", "DSR", "SUM_DSR", "CA_1", "CH_1", "per_1", "CA_2", "CH_2", "per_2", "CA_3", "CH_3", "per_3")
data_L2 = data_L2 %>%
  mutate(CH_1 = CH_1 / 1000.0, CH_2 = CH_2 / 1000.0, CH_3 = CH_3 / 1000.0) %>%
  arrange(time)
tbl_df(data_L2)


data_L3 = fread("data_L3_not_cloudy", header = F, stringsAsFactors = T, data.table = F)
colnames(data_L3) = c("time", "xran", "DSR", "SUM_DSR", "CA_1", "CH_1", "per_1", "CA_2", "CH_2", "per_2", "CA_3", "CH_3", "per_3")
data_L3 = data_L3 %>%
  mutate(CH_1 = CH_1 / 1000.0, CH_2 = CH_2 / 1000.0, CH_3 = CH_3 / 1000.0) %>%
  arrange(time)
tbl_df(data_L3)


data_L4 = fread("data_L3_cloudy", header = F, stringsAsFactors = T, data.table = F)
colnames(data_L4) = c("time", "xran", "DSR", "SUM_DSR", "CA_1", "CH_1", "per_1", "CA_2", "CH_2", "per_2", "CA_3", "CH_3", "per_3")
data_L4 = data_L4 %>%
  mutate(CH_1 = CH_1 / 1000.0, CH_2 = CH_2 / 1000.0, CH_3 = CH_3 / 1000.0) %>%
  arrange(time)
tbl_df(data_L4)


#===================================
#     data_L2, L3, L3 : Slope
#===================================
start = paste0(seq(2005, 2015, 1), "0101")
end = paste0(seq(2005, 2015, 1), "1231")


tbl_df(data_L2)
data_L2_irr = data_L2[, c(1:3)]
#   data_L2_irr = NaRV.omit(data_L2_irr)
tbl_df(data_L2_irr)

tbl_df(data_L3)
data_L3_irr = data_L3[, c(1:3)]
data_L3_irr = NaRV.omit(data_L3_irr)
tbl_df(data_L3_irr)

tbl_df(data_L4)
data_L4_irr = data_L4[, c(1:3)]
data_L4_irr = NaRV.omit(data_L4_irr)
tbl_df(data_L4_irr)


for (i in 1:length(start)) {

  Dat = data_L3_irr %>%
    filter(time >= start[i] & time <= end[i])

  lmFit = lm(Dat$DSR ~ Dat$xran)
  print(coef(lmFit))
  plot(Dat$xran, Dat$DSR)
}

#===================================
#     data_L2, L3, L3 : Count
#===================================
X = data_L2_irr$xran
X = data_L3_irr$xran

# X = data_L4_irr$xran

#png(file = paste("OUTPUT/Count_1", ".png", sep = ""), 1000, 600)
par(mar = c(5.1, 6, 5.1, 1.6), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.2, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
hist2 = hist(X, labels = F, ylim = c(0, 400), las = 1, breaks = seq(2005, 2016, 1), xaxs = "i", yaxs = "i",
             main = paste("\n\n\n Q.C after (N = ", length(X), ")"), axes = F, xlab = "", ylab = "", col = rainbow(12, alpha = 0.3), border = "white")
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(2005, 2016, 1)); mtext('Year  [Time]', side = 1, col = 'black', line = 3, cex = 1.8)
axis(2, col = 'black', col.axis = 'black', las = 1, at = seq(0, 365, 60)); mtext("Julian  Day  [Frequency]", side = 2, col = 'black', line = 4, cex = 1.8)
name = paste(hist2$counts, "\n", round((hist2$counts / 365) * 100.0, 1), "%")
text(hist2$mids, hist2$counts, label = name, adj = c(0.5, 1.5), cex = 1.3, col = 'black')
#dev.off()


#============================
#     DSR plot
#=============================
X = data_L2_irr$xran
Y = data_L2_irr$DSR

data_L2_irr_NaN = NaRV.omit(data_L2_irr)
# data_L2_irr_NaN =   na.omit(data_L2_irr)
XX = data_L2_irr_NaN$xran
YY = data_L2_irr_NaN$DSR
#png(file = paste("OUTPUT/QC_before", ".png", sep = ""), 1200, 600)
par(mar = c(5.1, 7, 5.1, 5.1), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
plot(X, Y, xlim = c(2005, 2016), ylim = c(0, 3500), pch = 21, bg = "white", col = 'black', cex = 1.3, xaxs = "i", yaxs = "i",
     xlab = "", ylab = "", yaxt = "n", xaxt = "n")
# plot(X,  Y,  xlim=c(2005, 2016), ylim=c(0, 3500), pch=21, bg="white", col='black', cex=1.1, xaxs="i", yaxs="i",
# xlab="", ylab="", yaxt="n", xaxt="n", main=paste('2005.01.01 ~ 2015.12.31 (Q.C x)') )
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(2005, 2016, 1)); mtext('Time  [Year]', side = 1, col = 'black', line = 3, cex = 1.8)
axis(2, col = "black", col.axis = "black", las = 1, at = seq(0, 3500, 500)); mtext(expression(paste(bold("Daily  Mean  Solar  Radiation  (IORS)  [W/m"^bold("2") * "]"))), side = 2, col = "black", line = 5, cex = 1.8)
abline(h = seq(500, 3000, 500), lw = 1, col = 'lightgrey', lty = "dotted"); abline(v = seq(2006, 2015, 1), lw = 1, col = 'lightgrey', lty = "dotted")
lmFit = lm(YY ~ XX)
abline(lmFit, col = 'red', font = 2, lw = 2)
text(2010.1, 3250, paste("(IORS) =", round(lmFit$coefficients[2], 3), "x (Year)", round(lmFit$coefficients[1], 3)), adj = 0, col = 'black', font = 2, cex = 1.6)
# text(2006, 2750, expression(bold(R^"2")), adj=0, font=2, col='red', cex=1.2)
text(2010.1, 2750, paste0("R = ", round(cor(XX, YY), 3), " (p<", round(cor.test(XX, YY)$p.value, 3), ")"), adj = 0, col = 'black', font = 2, cex = 1.6)
text(2010.1, 2250, paste("N = 3953"), adj = 0, col = 'black', font = 2, cex = 1.6)
#dev.off()


X = data_L3_irr$xran
Y = data_L3_irr$DSR
cor.test(X, Y)$p.value
#png(file = paste("OUTPUT//QC_after", ".png", sep = ""), 1200, 600)
par(mar = c(5.1, 7, 5.1, 5.1), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
# plot(X,  Y,  xlim=c(2005, 2016), ylim=c(0, 1200),  pch=21, bg="black", xaxs="i", yaxs="i", col='white', cex=1.1, xlab="", ylab="", yaxt="n",
#      xaxt="n", main=paste('2005.01.01 ~ 2015.12.31 (Q.C o)') )
plot(X, Y, xlim = c(2005, 2016), ylim = c(0, 1200), pch = 21, bg = "white", xaxs = "i", yaxs = "i", col = 'black',
     cex = 1.3, xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(2005, 2016, 1)); mtext('Time  [Year]', side = 1, col = 'black', line = 3, cex = 1.8)
axis(2, col = "black", col.axis = "black", las = 1, at = seq(0, 1400, 200)); mtext(expression(paste(bold("Daily  Mean  Solar  Radiation  (IORS)  [W/m"^bold("2") * "]"))), side = 2, col = "black", line = 5, cex = 1.8)
abline(h = seq(200, 1000, 200), lw = 1, col = 'lightgrey', lty = "dotted"); abline(v = seq(2006, 2015, 1), lw = 1, col = 'lightgrey', lty = "dotted")
lmFit = lm(Y ~ X)
abline(lmFit, col = 'red', font = 2, lw = 2)
text(2010.1, 1150, paste0("(IORS) = ", round(lmFit$coefficients[2], 3), " x (Year) + ", round(lmFit$coefficients[1], 3)), adj = 0, col = 'black', font = 2, cex = 1.6)
# text(2006, 1050, expression(bold(R^"2")), adj=0, font=2, col='red', cex=1.2)
text(2010.1, 1050, paste0("R = ", round(cor(X, Y), 3), " (p<", round(cor.test(X, Y)$p.value, 3), ")"), adj = 0, col = 'black', font = 2, cex = 1.6)
text(2010.1, 950, paste0("N = ", round(length(X), 3)), adj = 0, col = 'black', font = 2, cex = 1.6)
#dev.off()


X = data_L2_irr$xran
Y = data_L2_irr$DSR
data_L2_irr_NaN = NaRV.omit(data_L2_irr)
cor(data_L2_irr$xran, data_L2_irr$DSR)
XX = data_L2_irr_NaN$xran
YY = data_L2_irr_NaN$DSR
lmFit = lm(YY ~ XX)

N = length(X)
Bias = accuracy(XX, YY)[, 1]
per_Bias = accuracy(XX, YY)[, 4]
RMSE = accuracy(XX, YY)[, 2]
per_RMSE = (RMSE / mean(YY)) * 100.0

N; round(Bias, 3); round(per_Bias, 3); round(RMSE, 3); round(per_RMSE, 3); round(cor(XX, YY), 3); round(coef(lmFit)[1], 3); round(coef(lmFit)[2], 3)


ggplot() +
  # coord_fixed(ratio=1) +
  theme_bw() +
  geom_point(aes(X, Y)) +
  # stat_bin2d(bins=200) +
  # scale_fill_gradientn(colours = myPalette(11), limits=c(0, 20), na.value="#9E0142") +
  # scale_fill_gradientn(colours=matlab.like(11), limits=c(0, 40), na.value="#AA0000") +
  # scale_fill_gradientn(colours=matlab.like2(11), limits=c(0, 40), na.value="#BF0000") +
  annotate("text", x = 2005.1, y = 2700, label = "(IEODO) = 0.298 x (Year) - 185.163", size = 6, hjust = 0, color = "red", fontface = "bold", family = font) +
  annotate("text", x = 2005.1, y = 2400, label = "bold(R ~\"=\"~ \"0.004\"~\"(p < 0.001)\")", parse = T, size = 6, hjust = 0, color = "red", family = font) +
  # annotate("text", x=25, y=1040, label="bold(Bias ~\"=\"~ \"+ 7.72\"~Wm^\"-2\")", parse=T, size=4.2, hjust=0, family=font) +
  # annotate("text", x=25, y=970, label="bold(RMSE ~\"=\"~ \"132.51\"~Wm^\"-2\")", parse=T, size=4.2, hjust=0, family=font) +
  annotate("text", x = 2005.1, y = 2100, label = "bold(N ~\"=\"~ \"3953\")", parse = T, size = 6, hjust = 0, family = font) +
  # annotate("text", x=2005.1, y=2600, label="Increase Rate = 3.278 []0.298 x (Year) - 185.163", size=5, hjust=0, color="red", fontface="bold", family=font) +
  annotate("text", x = 2010.0, y = 2700, label = "bold(Increase ~ Rate ~\"=\"~ \"3.278\" ~ \"[\"*Wm^\"-2\"~ \"/ Year\"*\"]\")",
           parse = T, size = 8, hjust = 0, family = font, color = "blue") +
  # geom_abline(intercept=0, slope=1, linetype=1, color="black", size=1.0) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X, Y)) +
  scale_x_continuous(breaks = seq(2005, 2016, by = 1), expand = c(0, 0), limits = c(2005, 2016)) +
  scale_y_continuous(breaks = seq(0, 3000, by = 500), expand = c(0, 0), limits = c(0, 3200)) +
  labs(title = "") +
  labs(y = expression(paste(bold("IEODO  Measurement  [Wm"^bold("-2") * "]")))) +
  labs(x = expression(paste(bold("Time [Year]")))) +
  # labs(fill = "Count") +
  labs(fill = NULL) +
  theme(plot.title = element_text(face = "bold", size = 20, color = "black")) +
  theme(axis.title.x = element_text(face = "bold", size = 20, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 20, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 20, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 20, colour = "black")) +
  theme(legend.title = element_text(face = "bold", size = 12, colour = "black")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 0.9)) +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  theme(legend.background = element_blank()) +
  theme(text = element_text(family = font)) +
  theme(plot.margin = unit(c(0, 8, 0, 0), "mm")) #+
# png("FIG/albedo.png", width=600, height=600, res=100)
#png("OUTPUT/before_QC_IEODO.png", width = 1200, height = 600, res = 100)
#dev.off()

X = data_L3_irr$xran
Y = data_L3_irr$DSR


lmFit = lm(Y ~ X)

N = length(X)
Bias = accuracy(X, Y)[, 1]
per_Bias = accuracy(X, Y)[, 4]
RMSE = accuracy(X, Y)[, 2]
per_RMSE = (RMSE / mean(Y)) * 100.0

N; round(Bias, 3); round(per_Bias, 3); round(RMSE, 3); round(per_RMSE, 3); round(cor(X, Y), 3); round(coef(lmFit)[1], 3); round(coef(lmFit)[2], 3)


ggplot() +
  # coord_fixed(ratio=1) +
  theme_bw() +
  geom_point(aes(X, Y)) +
  # stat_bin2d(bins=200) +
  # scale_fill_gradientn(colours = myPalette(11), limits=c(0, 20), na.value="#9E0142") +
  # scale_fill_gradientn(colours=matlab.like(11), limits=c(0, 40), na.value="#AA0000") +
  # scale_fill_gradientn(colours=matlab.like2(11), limits=c(0, 40), na.value="#BF0000") +
  annotate("text", x = 2005.1, y = 1150, label = "(IEODO) = - 3.795 x (Year) + 8071.253", size = 6, hjust = 0, color = "red", fontface = "bold", family = font) +
  annotate("text", x = 2005.1, y = 1050, label = "bold(R ~\"=\"~ \"- 0.056\"~\"(p < 0.001)\")", parse = T, size = 6, hjust = 0, color = "red", family = font) +
  # annotate("text", x=25, y=1040, label="bold(Bias ~\"=\"~ \"+ 7.72\"~Wm^\"-2\")", parse=T, size=4.2, hjust=0, family=font) +
  # annotate("text", x=25, y=970, label="bold(RMSE ~\"=\"~ \"132.51\"~Wm^\"-2\")", parse=T, size=4.2, hjust=0, family=font) +
  annotate("text", x = 2005.1, y = 950, label = "bold(N ~\"=\"~ \"3186\")", parse = T, size = 6, hjust = 0, family = font) +
  # annotate("text", x=2005.1, y=2600, label="Increase Rate = 3.278 []0.298 x (Year) - 185.163", size=5, hjust=0, color="red", fontface="bold", family=font) +
  annotate("text", x = 2009.7, y = 1050, label = "bold(Increase ~ Rate ~\"=\"~ \"- 41.745\" ~\"[\"*Wm^\"-2\"~ \"/ Year\"*\"]\")",
           parse = T, size = 8, hjust = 0, family = font, color = "blue") +
  # geom_abline(intercept=0, slope=1, linetype=1, color="black", size=1.0) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X, Y)) +
  scale_x_continuous(breaks = seq(2005, 2016, by = 1), expand = c(0, 0), limits = c(2005, 2016)) +
  scale_y_continuous(breaks = seq(0, 1200, by = 200), expand = c(0, 0), limits = c(0, 1200)) +
  labs(title = "") +
  labs(y = expression(paste(bold("IEODO  Measurement  [Wm"^bold("-2") * "]")))) +
  labs(x = expression(paste(bold("Time [Year]")))) +
  # labs(fill = "Count") +
  labs(fill = NULL) +
  theme(plot.title = element_text(face = "bold", size = 20, color = "black")) +
  theme(axis.title.x = element_text(face = "bold", size = 20, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 20, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 20, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 20, colour = "black")) +
  theme(legend.title = element_text(face = "bold", size = 12, colour = "black")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 0.9)) +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  theme(legend.background = element_blank()) +
  theme(text = element_text(family = font)) +
  theme(plot.margin = unit(c(0, 8, 0, 0), "mm")) #+
# png("FIG/albedo.png", width=600, height=600, res=100)
#png("OUTPUT/ater_QC_IEODO.png", width = 1200, height = 600, res = 100)
#dev.off()


#======================================================
#     Celiometer plot (first, second, third) : dot
#======================================================

X = data_L2$xran
Y = data_L2$CH_1
Z = data_L2$CA_1

X = data_L3$xran
Y = data_L3$CH_1
Z = data_L3$CA_1

X = data_L4$xran
Y = data_L4$CH_1
Z = data_L4$CA_1

par(mar = c(5.1, 5.1, 5.1, 5.1), cex.main = 1.6, cex.lab = 1.5, cex.axis = 1.5, cex = 1.1, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
plot(X, Y, xlim = c(2005, 2016), ylim = c(0, 9), pch = 21, bg = cols[2], col = 'white', cex = 1.1, xlab = "", ylab = "", yaxt = "n", xaxt = "n", main = 'Cloud Height, Amount')
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(2005, 2016, 1)); mtext('Hour  [Time]', side = 1, col = 'black', line = 3, cex = 1.5)
axis(2, col = cols[2], col.axis = cols[2], las = 1, at = seq(0, 8, 1)); mtext("Cloud Height  [km]", side = 2, col = cols[2], line = 3, cex = 1.5)
grid(nx = 0, ny = NULL, lw = 1, col = 'lightgrey', lty = "dotted"); abline(v = seq(2005, 2016, 1), lw = 1, col = 'lightgrey', lty = "dotted")
lmFit = lm(Y ~ X)
abline(lmFit, col = 'red', font = 2, lw = 2)
text(2005, 8.5, paste("(Cloud Height) =", round(lmFit$coefficients[2], 2), "x (Hour) +", round(lmFit$coefficients[1], 2)), adj = 0, col = 'red', font = 2, cex = 1.2)
text(2005, 7.5, expression(bold(R^"2")), adj = 0, font = 2, col = 'red', cex = 1.2)
text(2005, 7.5, paste("      =", round((cor(X, Y)^2), 3)), adj = 0, col = 'red', font = 2, cex = 1.2)
par(new = T)
plot(X, Z, pch = 21, xlim = c(2005, 2016), ylim = c(0, 9), bg = cols[3], col = 'white', cex = 1.1, yaxt = "n", xlab = "", ylab = "", xaxt = "n", bty = "n")
axis(4, col = cols[3], col.axis = cols[3], las = 1, at = seq(0, 8, 1)); mtext('Cloud  Amount', side = 4, col = cols[3], line = 3, cex = 1.6)
lm.fit2 = lm(Z ~ X)
abline(lm.fit2, col = 'blue', font = 2, lw = 2)
text(2010.5, 8.5, paste("(Cloud Height) =", round(lm.fit2$coefficients[2], 2), "x (Hour) +", round(lm.fit2$coefficients[1], 2)), adj = 0, col = 'blue', font = 2, cex = 1.2)
text(2010.5, 7.5, expression(bold(R^"2")), adj = 0, font = 2, col = 'blue', cex = 1.2)
text(2010.5, 7.5, paste("      =", round((cor(X, Z)^2), 3)), adj = 0, col = 'blue', font = 2, cex = 1.2)

#======================================================
#     Celiometer plot (first, second, third) + interp
#======================================================
data_L2 = na.omit(data_L2)
data_L3 = na.omit(data_L3)
data_L4 = na.omit(data_L4)

X = data_L4$xran
Y = data_L4$CH_1
Z = data_L4$CA_1

X = na.omit(X)
Y = na.omit(Y)
Z = na.omit(Z)

first = interp(X, Y, Z, nx = 10000, ny = 10000)
second = interp(data_L3$xran, data_L3$CH_2, data_L3$CA_2, nx = 1000, ny = 1000)
third = interp(data_L3$xran, data_L3$CH_3, data_L3$CA_3, nx = 1000, ny = 1000)

first_raster = raster(first)
second_raster = raster(second)
third_raster = raster(third)

#===================
#     First
#====================

par(mar = c(5.1, 5.1, 5.1, 5.1), cex.main = 1.8, cex.lab = 1.6, cex.axis = 1.6, cex = 1.1, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
plot(first_raster, breaks = c(0:8), col = terrain.colors(9), main = "Celiometer : First",
     xlab = "", ylab = "", yaxt = "n", xaxt = "n", xlim = c(2005, 2015), las = 1)
points(X, Y, xlim = c(2005, 2015), ylim = c(0, 8), pch = 1, col = 'red', cex = 1.4, xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(2005, 2015, 1)); mtext('Hour  [Time]', side = 1, col = 'black', line = 3, cex = 1.5)
axis(2, col = 'red', col.axis = 'red', las = 1, at = seq(0, 8, 1)); mtext("Cloud  Height [km]", side = 2, col = 'red', line = 2.5, cex = 1.5)
grid(nx = 0, ny = NULL, lw = 1, col = 'lightgrey', lty = "dotted"); abline(v = seq(8, 18, 1), lw = 1, col = 'lightgrey', lty = "dotted")
points(X, Z, pch = 1, xlim = c(2005, 2015), ylim = c(0, 8), col = 'blue', cex = 1.4, yaxt = "n", xlab = "", ylab = "", xaxt = "n", bty = "n")
axis(4, col = 'blue', col.axis = 'blue', las = 1, at = seq(0, 8, 1)); mtext('Cloud  Amount', side = 4, col = 'blue', line = 2.5, cex = 1.6)

lmFit = lm(Y ~ X)
abline(lmFit, col = 'red', font = 2, lw = 2)
text(2005, 5.0, paste("(Cloud Height) =", round(lmFit$coefficients[2], 2), "x (Hour) +", round(lmFit$coefficients[1], 2)), adj = 0, col = 'red', font = 2, cex = 1.2)
text(2005, 4.5, expression(bold(R^"2")), adj = 0, font = 2, col = 'red', cex = 1.2)
text(2005, 4.5, paste("      =", round((cor(X, Y)^2), 3)), adj = 0, col = 'red', font = 2, cex = 1.2)

lm.fit2 = lm(Z ~ X)
abline(lm.fit2, col = 'blue', font = 2, lw = 2)
text(2005, 3.5, paste("(Cloud Amount) =", round(lm.fit2$coefficients[2], 2), "x (Hour) +", round(lm.fit2$coefficients[1], 2)), adj = 0, col = 'blue', font = 2, cex = 1.2)
text(2005, 3.0, expression(bold(R^"2")), adj = 0, font = 2, col = 'blue', cex = 1.2)
text(2005, 3.0, paste("      =", round((cor(X, Z)^2), 3)), adj = 0, col = 'blue', font = 2, cex = 1.2)

#===================
#     Second
#====================
X = data_L1$xran2
Y = data_L1$CH_2
YY = data_L1$CA_2
par(mar = c(5.1, 5.1, 5.1, 5.1), cex.main = 1.8, cex.lab = 1.6, cex.axis = 1.6, cex = 1.1, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
plot(second_raster, breaks = c(0:8), col = terrain.colors(9), main = "Celiometer : Second",
     xlab = "", ylab = "", yaxt = "n", xaxt = "n", xlim = c(9, 17), las = 1)
points(X, Y, xlim = c(9, 17), ylim = c(0, 8), pch = 1, col = 'red', cex = 1.4, xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(9, 17, 1)); mtext('Hour  [Time]', side = 1, col = 'black', line = 3, cex = 1.5)
axis(2, col = 'red', col.axis = 'red', las = 1, at = seq(0, 8, 1)); mtext("Cloud  Height [km]", side = 2, col = 'red', line = 2.5, cex = 1.5)
grid(nx = 0, ny = NULL, lw = 1, col = 'lightgrey', lty = "dotted"); abline(v = seq(8, 18, 1), lw = 1, col = 'lightgrey', lty = "dotted")
points(X, YY, pch = 1, xlim = c(9, 17), ylim = c(0, 8), col = 'blue', cex = 1.4, yaxt = "n", xlab = "", ylab = "", xaxt = "n", bty = "n")
axis(4, col = 'blue', col.axis = 'blue', las = 1, at = seq(0, 8, 1)); mtext('Cloud  Amount', side = 4, col = 'blue', line = 2.5, cex = 1.6)

lmFit = lm(Y ~ X)
abline(lmFit, col = 'red', font = 2, lw = 2)
text(9, 5.5, paste("(Cloud Height) =", round(lmFit$coefficients[2], 2), "x (Hour) +", round(lmFit$coefficients[1], 2)), adj = 0, col = 'red', font = 2, cex = 1.2)
text(9, 5.0, expression(bold(R^"2")), adj = 0, font = 2, col = 'red', cex = 1.2)
text(9, 5.0, paste("      =", round((cor(data_L1$xran2, data_L1$CH_1)^2), 3)), adj = 0, col = 'red', font = 2, cex = 1.2)

lm.fit2 = lm(YY ~ X)
abline(lm.fit2, col = 'blue', font = 2, lw = 2)
text(9, 4.0, paste("(Cloud Amount) =", round(lm.fit2$coefficients[2], 2), "x (Hour) +", round(lm.fit2$coefficients[1], 2)), adj = 0, col = 'blue', font = 2, cex = 1.2)
text(9, 3.5, expression(bold(R^"2")), adj = 0, font = 2, col = 'blue', cex = 1.2)
text(9, 3.5, paste("      =", round((cor(data_L1$xran2, data_L1$CA_1)^2), 3)), adj = 0, col = 'blue', font = 2, cex = 1.2)


#===================
#     Third
#====================
X = data_L1$xran2
Y = data_L1$CH_3
YY = data_L1$CA_3
par(mar = c(5.1, 5.1, 5.1, 5.1), cex.main = 1.8, cex.lab = 1.6, cex.axis = 1.6, cex = 1.1, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
plot(third_raster, breaks = c(0:8), col = terrain.colors(9), main = "Celiometer : Third",
     xlab = "", ylab = "", yaxt = "n", xaxt = "n", xlim = c(9, 17), las = 1)
points(X, Y, xlim = c(9, 17), ylim = c(0, 8), pch = 1, col = 'red', cex = 1.4, xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(9, 17, 1)); mtext('Hour  [Time]', side = 1, col = 'black', line = 3, cex = 1.5)
axis(2, col = 'red', col.axis = 'red', las = 1, at = seq(0, 8, 1)); mtext("Cloud  Height [km]", side = 2, col = 'red', line = 2.5, cex = 1.5)
grid(nx = 0, ny = NULL, lw = 1, col = 'lightgrey', lty = "dotted"); abline(v = seq(8, 18, 1), lw = 1, col = 'lightgrey', lty = "dotted")
points(X, YY, pch = 1, xlim = c(9, 17), ylim = c(0, 8), col = 'blue', cex = 1.4, yaxt = "n", xlab = "", ylab = "", xaxt = "n", bty = "n")
axis(4, col = 'blue', col.axis = 'blue', las = 1, at = seq(0, 8, 1)); mtext('Cloud  Amount', side = 4, col = 'blue', line = 2.5, cex = 1.6)

lmFit = lm(Y ~ X)
abline(lmFit, col = 'red', font = 2, lw = 2)
text(9, 3.0, paste("(Cloud Height) =", round(lmFit$coefficients[2], 2), "x (Hour) +", round(lmFit$coefficients[1], 2)), adj = 0, col = 'red', font = 2, cex = 1.2)
text(9, 5.0, expression(bold(R^"2")), adj = 0, font = 2, col = 'red', cex = 1.2)
text(9, 5.0, paste("      =", round((cor(data_L1$xran2, data_L1$CH_1)^2), 3)), adj = 0, col = 'red', font = 2, cex = 1.2)

lm.fit2 = lm(YY ~ X)
abline(lm.fit2, col = 'blue', font = 2, lw = 2)
text(9, 4.0, paste("(Cloud Amount) =", round(lm.fit2$coefficients[2], 2), "x (Hour) +", round(lm.fit2$coefficients[1], 2)), adj = 0, col = 'blue', font = 2, cex = 1.2)
text(9, 3.5, expression(bold(R^"2")), adj = 0, font = 2, col = 'blue', cex = 1.2)
text(9, 3.5, paste("      =", round((cor(data_L1$xran2, data_L1$CA_1)^2), 3)), adj = 0, col = 'blue', font = 2, cex = 1.2)


#============================================
#     Pie grahy
#============================================
point = c(3186, 293, 237, 224, 7, 6)
sum = sum(point)
percent = (point / sum) * 100.0

name2 = paste(round(point), paste0("[", round(percent, 2), " %]"))
name2[6] = "6 [0.15%] \n\n"

#png(file = paste("OUTPUT/pie_1", ".png", sep = ""), 600, 600)
par(mar = c(5, 8, 5, 8), cex.main = 2.2, cex.lab = 1.6, cex.axis = 1.6, cex = 1.1, font.axis = 2, font.lab = 2, font.main = 2, font = 1, family = font)
pie(point, labels = name2, radius = 1.0, col = cols, border = 'white', lwd = 2, cex = 1.6,
    main = "2005.01.01 ~ 2015.12.31", font = 2, family = "New Century Schoolbook")
par(new = TRUE)
pie(1, radius = 0.5, col = 'white', border = 'white', labels = '')
titleText = paste0("N = 3953 \n [100 %]")
text(0, 0, labels = titleText, cex = 1.9, col = par('col.main'), font = 2, family = "New Century Schoolbook")
#dev.off()

name = c("Normal", "Repect", "Minimum", "Missing", "Irregular", "Maximum")
# par(mar=c(5, 8, 5, 8), cex.main=2.2, cex.lab=1.6, cex.axis=1.6, cex=1.1, font.axis=2, font.lab=2, font.main=2, font=1, family = "NanumBarunGothic" )
png(file = paste("OUTPUT//pie_2", ".png", sep = ""), 600, 600)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), cex = 1.8, mar = c(0, 0, 0, 0), new = TRUE, font = 1, family = font)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend(0, 0, name, cex = 1.3, fill = cols, box.col = 'white', border = "white", x.intersp = 0.5)
dev.off()


#========================================================
#     IEODO : Comparison Observation [2016.07.17]
#========================================================

data_obs = read.csv('Ieodo_2016.07.17.dat', header = T, sep = "")
tbl_df(data_obs)

data_obs = data_obs %>%
  mutate(
    xran = ISOdatetime(year, month, day, hour, min, sec = 00, tz = "")
  )


X = data_obs$xran

Y = data_obs$ieodo
YY = data_obs$gwnu
mean(((YY - Y) / YY) * 100)
#========================================
#     Time series (dot)
#========================================
summary(data_obs)

xran_min = as.POSIXct("2016-07-18 12:10:00")
xran_max = as.POSIXct("2016-07-18 13:00:00")

png(file = paste("OUTPUT/Comparison_Obs_dot_diff", ".png", sep = ""), 900, 600)
par(mar = c(5.1, 7, 5.1, 7), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.5, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
plot(X, Y, ylim = c(250, 400), pch = 21, bg = 'white', col = 'black', cex = 1.6, xlab = "", ylab = "", yaxt = "n", xaxt = "n", xaxs = "i", yaxs = "i", xlim = c(xran_min, xran_max))
axis.POSIXct(1, X, format = "%H:%M", at = seq(xran_min, xran_max, by = "10 min"), las = 1)
mtext('Time  [Hour : Minute]', side = 1, col = 'black', line = 3, cex = 2.2)
axis(2, col = 'black', col.axis = 'black', las = 1, at = seq(250, 500, 30)); mtext(expression(paste(bold("1-Minute  Solar  Radiation  [W/m"^bold("2") * "]"))), side = 2, col = 'black', line = 4.5, cex = 2.2)
# grid(nx=0, ny=NULL, lw=1, col='lightgrey', lty="dotted") ;
abline(h = seq(280, 370, 30), lw = 1, col = 'lightgrey', lty = "dotted")
abline(v = seq(as.POSIXct("2016-07-18 12:20:00"), as.POSIXct("2016-07-18 12:50:00"), by = "10 min"), lw = 1, col = 'lightgrey', lty = "dotted")
points(X, YY, pch = 21, bg = 'blue', col = 'blue', type = 'l', lwd = 2, cex = 1.5)
# par(new=T)
# plot(X, ((YY-Y)/YY)*100.0, col='green', pch=21, bg='green', cex=1.5, ylim=c(-4,4), yaxs="i",xaxs="i", xaxt="n", yaxt="n", xlab='', ylab=' ', type='l', lwd=3)
# axis(4,  col='green',col.axis='green', las=1) ; mtext( expression(paste(bold("%  Diff.  [Wm"^bold("-2")*"]"))), side=4,  line=3, cex=2.2, col='green')
# legend( "topright",  legend=c("IEODO", "GWNU"), pch=c(21, 21), col=c('black', 'red'),
# cex=1.4,  pt.cex=1.5, text.font=2, pt.bg=c('black', 'red'), bty="n", xpd=T, text.col=c('black', 'red'))
legend("topright", legend = c("IORS"), pch = c(21), col = c('black'),
       cex = 1.4, pt.cex = 1.3, text.font = 1, pt.bg = c('white'), bty = "n", xpd = T, text.col = c('black'))
legend("topright", legend = c("GWNU"), col = c('blue'), text.col = c('blue'),
       cex = 1.4, pt.cex = 1.3, text.font = 1, lwd = 2, bty = "n", xpd = T, y.intersp = 4)
dev.off()

#========================================
#     Diff.  % Diff.
#========================================
diff = YY - Y
per_diff = ((YY - Y) / YY) * 100.0
summary(diff)

png(file = paste("OUTPUT//Comparison_Obs_Diff", ".png", sep = ""), 800, 600)
par(mar = c(5.1, 7, 5.1, 7), cex.main = 1.8, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
plot(X, diff, pch = 21, bg = cols[3], col = 'white', cex = 2.3, xlab = "", ylab = "", yaxt = "n",
     xaxt = "n", main = paste('Diff. : GWNU - IEODO\n % Diff. : ((GWNU-IEODO) / GWNU)*100.0'))
axis.POSIXct(1, X, format = "%H:%M", at = seq(as.POSIXct("2016-07-18 12:10:00"), as.POSIXct("2016-07-18 13:00:00"), by = "10 min"), las = 1)
mtext('Year  [Time]', side = 1, col = 'black', line = 3, cex = 1.8)
axis(2, col = cols[3], col.axis = cols[3], las = 1); mtext(expression(paste(bold("Diff.  [Wm"^bold("-2") * "]"))), side = 2, col = cols[3], line = 3, cex = 2.0)
grid(nx = 0, ny = NULL, lw = 1, col = 'lightgrey', lty = "dotted"); abline(v = seq(as.POSIXct("2016-07-18 12:10:00"), as.POSIXct("2016-07-18 13:00:00"), by = "10 min"), lw = 1, col = 'lightgrey', lty = "dotted")
points(X, YY, pch = 21, bg = cols[2], col = 'white', cex = 2.3)
par(new = T)
plot(X, per_diff, pch = 21, bg = cols[4], col = 'white', cex = 2.3, yaxt = "n", xlab = "", ylab = "", xaxt = "n", bty = "n")
axis(4, col = cols[4], col.axis = cols[4], las = 1); mtext(expression(paste(bold("% Diff.  [%]"))), side = 4, col = cols[4], line = 4, cex = 2.0)
legend("topright", legend = c("Diff.", "% Diff."), pch = c(21, 21), col = c('white', 'white'),
       cex = 1.3, pt.cex = 2.3, text.font = 1, pt.bg = c(cols[c(3, 4)]))
dev.off()

#========================================
#     1:1 scatter plot
#========================================
X = data_obs$ieodo  # IEODO
Y = data_obs$gwnu   # Reference

lmFit = lm(Y ~ X)

N = length(X)
Bias = accuracy(X, Y)[, 1]
per_Bias = (Bias / mean(Y)) * 100.0
RMSE = accuracy(X, Y)[, 2]
per_RMSE = (RMSE / mean(Y)) * 100.0

N; round(Bias, 2); round(per_Bias, 2); round(RMSE, 2); round(per_RMSE, 2); round(cor(X, Y), 3); round(coef(lmFit)[1], 2); round(coef(lmFit)[2], 2)

#png(file = paste("OUTPUT/Comparison_Obs_scatter_ggplot2", ".png", sep = ""), 700, 700)
par(mar = c(5, 7, 5, 5), cex.main = 1.8, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
plot(X, Y, xlab = "", ylab = "", cex = 2.0, col = 'black', pch = 21, bg = "white",
     xlim = c(250, 400), ylim = c(250, 400), yaxt = "n", xaxt = "n", xaxs = "i", yaxs = "i")
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(250, 500, 30)); mtext(expression(paste(bold("1-Minute  Solar  Radiation  (IORS)  [W/m"^bold("2") * "]"))), side = 1, col = 'black', line = 3, cex = 2.2)
axis(2, col = 'black', col.axis = 'black', las = 1, at = seq(250, 500, 30)); mtext(expression(paste(bold("1-Minute  Solar  Radiation  (GWNU)  [W/m"^bold("2") * "]"))), side = 2, col = 'black', line = 4, cex = 2.2)
# grid(nx=0, ny=NULL, lw=1, col='lightgrey', lty="dotted") ;
abline(h = seq(280, 370, 30), lw = 1, col = 'lightgrey', lty = "dotted")
abline(v = seq(280, 370, 30), lw = 1, col = 'lightgrey', lty = "dotted")
lmFit = lm(Y ~ X)
abline(lmFit, col = 'red', font = 2, lw = 2); abline(a = 0, b = 1, lty = 1, lw = 2, col = 'black')
text(255, 395, paste("(GWNU) =", round(lmFit$coefficients[2], 2), "x (IORS)", round(lmFit$coefficients[1], 2)), adj = 0, col = 'black', font = 2, cex = 1.6)
# text(-4, 750, expression(bold(R^"2")), adj=0, font=2, col='black', cex=1.3)
text(255, 385, paste("R =", round(cor(X, Y), 3), "(p<0.001)"), adj = 0, col = 'black', font = 2, cex = 1.6)
text(255, 375, paste("Bias =", round(Bias, 2), "(", round(per_Bias, 2), "% )"), adj = 0, col = 'black', font = 2, cex = 1.6)
text(255, 365, paste("RMSE =", round(RMSE, 2), "(", round(per_RMSE, 2), "% )"), adj = 0, col = 'black', font = 2, cex = 1.6)
text(255, 355, paste("N = ", round(N, 0)), adj = 0, col = 'black', font = 2, cex = 1.6)
#dev.off()


ggplot() +
  coord_fixed(ratio = 1) +
  theme_bw() +
  geom_point(aes(X, Y), size = 5, shape = 21) +
  # stat_bin2d(bins=200) +
  # scale_fill_gradientn(colours = myPalette(11), limits=c(0, 20), na.value="#9E0142") +
  # scale_fill_gradientn(colours=matlab.like(11), limits=c(0, 40), na.value="#AA0000") +
  # scale_fill_gradientn(colours=matlab.like2(11), limits=c(0, 40), na.value="#BF0000") +
  annotate("text", x = 255, y = 395, label = "(GWNU) = 1.04 x (IEODO) - 14.97", size = 6, hjust = 0, fontface = "bold", family = font) +
  annotate("text", x = 255, y = 385, label = "bold(R ~\"=\"~ \"0.998\"~\"(p < 0.001)\")", parse = T, size = 6, hjust = 0, family = font) +
  annotate("text", x = 255, y = 375, label = "bold(Bias ~\"=\"~ \"- 3.24\"~Wm^\"-2\")", parse = T, size = 6, hjust = 0, family = font) +
  annotate("text", x = 255, y = 365, label = "bold(RMSE ~\"=\"~ \"3.72\"~Wm^\"-2\")", parse = T, size = 6, hjust = 0, family = font) +
  annotate("text", x = 255, y = 355, label = "bold(N ~\"=\"~ \"50\")", parse = T, size = 6, hjust = 0, family = font) +
  # annotate("text", x=200, y=980, label="bold(RMSE ~\"=\"~ \"63.22\" ~Wm^\"-2\"~\"(21.69\"~\"%)\")", parse=T, size=4.2, hjust=0, family=font) +
  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X, Y)) +
  scale_x_continuous(breaks = seq(250, 400, by = 30), expand = c(0, 0), limits = c(250, 400)) +
  scale_y_continuous(breaks = seq(250, 400, by = 30), expand = c(0, 0), limits = c(250, 400)) +
  labs(title = "") +
  labs(y = expression(paste(bold("Surface  Solar  Radiation  (GWNU)  [Wm"^bold("-2") * "]")))) +
  labs(x = expression(paste(bold("Surface  Solar  Radiation  (IEODO) [Wm"^bold("-2") * "]")))) +
  # labs(fill = "Count") +
  labs(fill = NULL) +
  theme(plot.title = element_text(face = "bold", size = 20, color = "black")) +
  theme(axis.title.x = element_text(face = "bold", size = 20, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 20, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 20, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 20, colour = "black")) +
  theme(legend.title = element_text(face = "bold", size = 12, colour = "black")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 0.9)) +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  theme(legend.background = element_blank()) +
  theme(text = element_text(family = font)) +
  theme(plot.margin = unit(c(0, 7, 0, 0), "mm")) # +
# png("FIG/albedo.png", width=600, height=600, res=100)
#png("OUTPUT/Comparison_Obs_scatter_ggplot2.png", width = 700, height = 700, res = 100)
#dev.off()


#============================================
#     RTM(GWNU), CERES(Terra, Aqua, Npp), Obs
#============================================
#======================================
#     Obs
#======================================
tbl_df(data_L3_irr)
c01 = as.numeric(substr(data_L3_irr[, 1], 1, 4))
c02 = as.numeric(substr(data_L3_irr[, 1], 5, 6))
c03 = as.numeric(substr(data_L3_irr[, 1], 7, 8))
data2 = data.frame(c01, c02, c03)
data_L5_irr = data.frame(data2, data_L3_irr[, c(-1)])

tbl_df(data_L5_irr)
colnames(data_L5_irr) = c("year", "month", "day", "xran", "mean_obs")
tbl_df(data_L5_irr)

data_obs = merge(x = data_L5_irr, y = data_irr, by = c("year", "month", "day"), all = F)
tbl_df(data_obs)
colnames(data_obs) = c("year", "month", "day", "xran", "mean_obs", "hour", "min", "sec", "obs")

ref_month = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

data_obs_L1 = data_obs %>%
  filter(hour >= 9 & hour <= 16) %>%
  filter(year >= 2013 & year <= 2015) %>%
  filter(obs > 0 & obs < 1400) %>%
  arrange(year, month, day, hour, min) %>%
  mutate(xran_KST = ISOdatetime(year = year, month = month, day = day, hour = hour, min = min, sec = sec, tz = ""),
         xran2 = year +
           ((month - 1) / 12) +
           ((day - 1) / (12 * ref_month[month])) +
           ((hour) / (12 * ref_month[month] * 24)) +
           ((min) / (12 * ref_month[month] * 24 * 60)))

X = data_obs_L1$xran2
Y = data_obs_L1$obs
plot(X, Y, xaxs = "i", yaxs = "i")
lmFit = lm(Y ~ X)
round(coef(lmFit), 3)
abline(lmFit, col = 'red', font = 2, lw = 2); abline(a = 0, b = 1, lty = 2, lw = 2, col = 'blue')
summary(lmFit)
accuracy(Y, X)

#======================================
#     Model
#======================================
data_gwnu = read.csv('./INPUT/IEODO_UTC.dat', header = F, sep = "")
data_gwnu = data_gwnu[, c(1:5, 20, 22)]
colnames(data_gwnu) = c("year", "month", "day", "hour", "min", "cloud", "model")
tbl_df(data_gwnu)

data_gwnu_L1 = data_gwnu %>%
  mutate(xran_UTC = ISOdatetime(year = year, month = month, day = day, hour = hour, min = min, sec = 00, tz = "UTC"),
         xran_KST = with_tz(xran_UTC, ""))

convert_time = as.character(data_gwnu_L1$xran_KST)
c01 = as.numeric(substr(convert_time, 1, 4))
c02 = as.numeric(substr(convert_time, 6, 7))
c03 = as.numeric(substr(convert_time, 9, 10))
c04 = as.numeric(substr(convert_time, 12, 13))
c05 = as.numeric(substr(convert_time, 15, 16))
c06 = as.numeric(substr(convert_time, 18, 19))
data2 = data.frame(c01, c02, c03, c04, c05, c06)
colnames(data2) = c("year2", "month2", "day2", "hour2", "min2", "sec2")

data_gwnu_L2 = data.frame(data_gwnu_L1, data2)

data_gwnu_L3 = data_gwnu_L2 %>%
  mutate(xran2 = year2 +
    ((month2 - 1) / 12) +
    ((day2 - 1) / (12 * ref_month[month2])) +
    ((hour2) / (12 * ref_month[month2] * 24)) +
    ((min2) / (12 * ref_month[month2] * 24 * 60))) %>%
  filter(model > 0 & model < 1400) %>%
  # filter(cloud==0)  # clear
  filter(cloud == 1)  # cloudy

X = data_gwnu_L3$xran2
Y = data_gwnu_L3$model
plot(X, Y)
lmFit = lm(Y ~ X)
round(coef(lmFit), 3)
abline(lmFit, col = 'red', font = 2, lw = 2); abline(a = 0, b = 1, lty = 2, lw = 2, col = 'blue')
summary(lmFit)
accuracy(Y, X)

#======================================
#     Satellite
#======================================
terra = read.csv('./INPUT/Terra_ceres.dat', header = F, sep = "")
colnames(terra) = c("year", "month", "day", "hour", "min", "sec", "lon", "lat", "SZA", "ceres_obs", "ceres_obs_clear", "surface_albedo", "clear_fraction")
aqua = read.csv('./INPUT/Aqua_ceres.dat', header = F, sep = "")
colnames(aqua) = c("year", "month", "day", "hour", "min", "sec", "lon", "lat", "SZA", "ceres_obs", "ceres_obs_clear", "surface_albedo", "clear_fraction")
npp = read.csv('./INPUT/Npp_ceres.dat', header = F, sep = "")
colnames(npp) = c("year", "month", "day", "hour", "min", "sec", "lon", "lat", "SZA", "ceres_obs", "ceres_obs_clear", "surface_albedo", "clear_fraction")

data_ceres = rbind(terra, aqua, npp)
# data_ceres = npp

data_ceres_L1 = data_ceres %>%
  mutate(dist = sqrt((lon - 125.18)^2 + (iorsGeoLat - 32.12)^2)) %>%
  filter(ceres_obs > 0 & ceres_obs < 1400) %>%
  filter(ceres_obs_clear > 0 & ceres_obs_clear < 1400) %>%
  filter(dist <= 0.1) %>%  # 20 km
  mutate(xran_UTC = ISOdatetime(year = year, month = month, day = day, hour = hour, min = min, sec = 00, tz = "UTC"),
         xran_KST = with_tz(xran_UTC, ""))

convert_time = as.character(data_ceres_L1$xran_KST)
c01 = as.numeric(substr(convert_time, 1, 4))
c02 = as.numeric(substr(convert_time, 6, 7))
c03 = as.numeric(substr(convert_time, 9, 10))
c04 = as.numeric(substr(convert_time, 12, 13))
c05 = as.numeric(substr(convert_time, 15, 16))
c06 = as.numeric(substr(convert_time, 18, 19))
data2 = data.frame(c01, c02, c03, c04, c05, c06)
colnames(data2) = c("year2", "month2", "day2", "hour2", "min2", "sec2")

data_ceres_L2 = data.frame(data_ceres_L1, data2)

data_ceres_L3 = data_ceres_L2 %>%
  mutate(xran2 = year2 +
    ((month2 - 1) / 12) +
    ((day2 - 1) / (12 * ref_month[month2])) +
    ((hour2) / (12 * ref_month[month2] * 24)) +
    ((min2) / (12 * ref_month[month2] * 24 * 60))) %>%
  # filter(hour2>=9 & hour2<=16) %>%
  # filter(year2>=2013 & year2<=2015) %>%
  # filter(clear_fraction>=95  &  clear_fraction<=100)  # clear
  filter(clear_fraction >= 0 & clear_fraction < 95)  # cloudy

data_ceres_L4 = data_ceres_L2 %>%
  mutate(xran2 = year2 +
    ((month2 - 1) / 12) +
    ((day2 - 1) / (12 * ref_month[month2])) +
    ((hour2) / (12 * ref_month[month2] * 24)) +
    ((min2) / (12 * ref_month[month2] * 24 * 60))) %>%
  filter(clear_fraction >= 95 & clear_fraction <= 100)  # clear


X = data_ceres_L3$xran2
Y = data_ceres_L3$ceres_obs
plot(X, Y, xaxs = "r", yaxs = "r")
plot(X, Y)
lmFit = lm(Y ~ X)
round(coef(lmFit), 3)
abline(lmFit, col = 'red', font = 2, lw = 2); abline(a = 0, b = 1, lty = 2, lw = 2, col = 'blue')
summary(lmFit)
accuracy(Y, X)


#======================================
#     obs vs gwnu
#======================================

data_obs_gwnu = merge(x = data_obs_L1, y = data_gwnu_L3, by = c("xran_KST"), all = F)
tbl_df(data_obs_gwnu)

X = data_obs_gwnu$model
Y = data_obs_gwnu$obs

lmFit = lm(Y ~ X)
#===============================
#  Outlier Test
#===============================
outlierTest(lmFit, cutoff = 0.10)[1]
# all
X[c(20159, 6886, 5075, 2994, 11714, 14815, 3876, 15405, 4782, 2495)] = NA
Y[c(20159, 6886, 5075, 2994, 11714, 14815, 3876, 15405, 4782, 2495)] = NA
# clear
X[c(5654, 5653, 5656, 4942, 716, 5685, 343, 5862, 2877, 3252)] = NA
Y[c(5654, 5653, 5656, 4942, 716, 5685, 343, 5862, 2877, 3252)] = NA
# cloudy
X[c(11383, 3497, 2452, 1557, 8490, 8156, 8442, 1863, 11881, 11537)] = NA
Y[c(11383, 3497, 2452, 1557, 8490, 8156, 8442, 1863, 11881, 11537)] = NA

X = NaRV.omit(X)
Y = NaRV.omit(Y)

N = length(X)
Bias = accuracy(X, Y)[, 1]
per_Bias = accuracy(X, Y)[, 4]
RMSE = accuracy(X, Y)[, 2]
per_RMSE = (RMSE / mean(Y)) * 100.0
N; round(Bias, 2); round(per_Bias, 2); round(RMSE, 2); round(per_RMSE, 2); round(cor(X, Y), 2); round(coef(lmFit)[1], 2); round(coef(lmFit)[2], 2)

ycord = seq(1360, 200, -90)

summary(X)


ggplot() +
  coord_fixed(ratio = 1) +
  theme_bw() +
  stat_bin2d(bins = 200, aes(X, Y)) +
  scale_fill_gradientn(colours = myPalette(11), limits = c(0, 20), na.value = "#9E0142") +
  # scale_fill_gradientn(colours=matlab.like(11), limits=c(0, 20), na.value="#AA0000") +
  # scale_fill_gradientn(colours=matlab.like2(11), limits=c(0, 20), na.value="#BF0000") +
  annotate("text", x = 250, y = ycord[1], label = "(IEODO) = 0.90 x (GWNU) + 31.43", size = 5, hjust = 0, color = "red", fontface = "bold", family = font) +
  annotate("text", x = 250, y = ycord[2], label = "bold(R ~\"=\"~ \"0.88\"~\"(p < 0.001)\")", parse = T, size = 5, hjust = 0, color = "red", family = font) +
  annotate("text", x = 250, y = ycord[3], label = "bold(Bias ~\"=\"~ \"- 13.36\"~Wm^\"-2\")", parse = T, size = 5, hjust = 0, family = font) +
  annotate("text", x = 250, y = ycord[4], label = "bold(RMSE ~\"=\"~ \"136.88\"~Wm^\"-2\")", parse = T, size = 5, hjust = 0, family = font) +
  annotate("text", x = 250, y = ycord[5], label = "bold(N ~\"=\"~ \"23335\")", parse = T, size = 5, hjust = 0, family = font) +
  # annotate("text", x=200, y=980, label="bold(RMSE ~\"=\"~ \"63.22\" ~Wm^\"-2\"~\"(21.69\"~\"%)\")", parse=T, size=4.2, hjust=0, family=font) +
  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X, Y)) +
  scale_x_continuous(breaks = seq(0, 1400, by = 200), expand = c(0, 0), limits = c(0, 1400)) +
  scale_y_continuous(breaks = seq(0, 1400, by = 200), expand = c(0, 0), limits = c(0, 1400)) +
  labs(title = "") +
  labs(y = expression(paste(bold("IEODO  Measurement  [Wm"^bold("-2") * "]")))) +
  labs(x = expression(paste(bold("GWNU  Model  [Wm"^bold("-2") * "]")))) +
  labs(fill = "Count") +
  theme(plot.title = element_text(face = "bold", size = 20, color = "black")) +
  theme(axis.title.x = element_text(face = "bold", size = 19, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 19, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 19, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 19, colour = "black")) +
  theme(legend.title = element_text(face = "bold", size = 12, colour = "black")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 0.9)) +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 13, face = "bold")) +
  theme(legend.background = element_blank()) +
  theme(text = element_text(family = font)) +
  theme(plot.margin = unit(c(0, 8, 0, 0), "mm")) +
  # png("FIG/albedo.png", width=600, height=600, res=100)
  png("OUTPUT/GWNU_IEODO.png", width = 600, height = 600, res = 100)
dev.off()


length(X)
plot(X, Y)
lmFit = lm(Y ~ X)
abline(lmFit, col = 'red', font = 2, lw = 2); abline(a = 0, b = 1, lty = 2, lw = 2, col = 'blue')
summary(lmFit)
coef(lmFit)
cor(X, Y)
accuracy(Y, X)
# plot(Y-X)

#======================================
#     obs vs satellite
#======================================

data_sate_obs = merge(x = data_obs_L1, y = data_ceres_L3, by = c("xran_KST"), all = F)
data_sate_obs_L1 = merge(x = data_obs_L1, y = data_ceres_L4, by = c("xran_KST"), all = F)

X = data_sate_obs$ceres_obs
Y = data_sate_obs$obs
lmFit = lm(Y ~ X)

X2 = data_sate_obs_L1$ceres_obs
Y2 = data_sate_obs_L1$obs
lmFit = lm(Y2 ~ X2)


#===============================
#  Outlier Test
#===============================
outlierTest(lmFit, cutoff = 0.10)[1]
# all
X[c(47, 1157, 1296, 1266, 1272)] = NA
Y[c(47, 1157, 1296, 1266, 1272)] = NA
# clear
X[c(87, 191)] = NA
Y[c(87, 191)] = NA
# cloudy
X[c(41, 989, 1108, 1083)] = NA
Y[c(41, 989, 1108, 1083)] = NA

X = NaRV.omit(X)
Y = NaRV.omit(Y)

X2[c(87, 191)] = NA
Y2[c(87, 191)] = NA
X2 = NaRV.omit(X2)
Y2 = NaRV.omit(Y2)

N = length(X)
Bias = accuracy(X, Y)[, 1]
per_Bias = accuracy(X, Y)[, 4]
RMSE = accuracy(X, Y)[, 2]
per_RMSE = (RMSE / mean(Y)) * 100.0
N; round(Bias, 2); round(per_Bias, 2); round(RMSE, 2); round(per_RMSE, 2); round(cor(X, Y), 2); round(coef(lmFit)[1], 2); round(coef(lmFit)[2], 2)

N = length(X2)
Bias = accuracy(X2, Y2)[, 1]
per_Bias = accuracy(X2, Y2)[, 4]
RMSE = accuracy(X2, Y2)[, 2]
per_RMSE = (RMSE / mean(Y2)) * 100.0

N; round(Bias, 2); round(per_Bias, 2); round(RMSE, 2); round(per_RMSE, 2); round(cor(X2, Y2), 2); round(coef(lmFit)[1], 2); round(coef(lmFit)[2], 2)
ycord = seq(1360, 200, -85)
ycord2 = seq(480, 0, -85)
text_sz = 4.5

ggplot() +
  coord_fixed(ratio = 1) +
  theme_bw() +
  geom_point(aes(X, Y), colour = '#F6756E', alpha = 0.4) +
  geom_point(aes(X2, Y2), colour = '#629AFA', alpha = 0.4) +
  # stat_bin2d(bins=200) +
  # scale_fill_gradientn(colours = myPalette(11), limits=c(0, 20), na.value="#9E0142") +
  # scale_fill_gradientn(colours=matlab.like(11), limits=c(0, 40), na.value="#AA0000") +
  # scale_fill_gradientn(colours=matlab.like2(11), limits=c(0, 40), na.value="#BF0000") +
  annotate("text", x = 25, y = ycord[1], label = "[ All sky ]", size = 6, hjust = 0, color = "magenta", fontface = "bold", family = font) +
  annotate("text", x = 25, y = ycord[2], label = "Y = 1.02 X - 0.62", size = text_sz, hjust = 0, color = "#F6756E", fontface = "bold", family = font) +
  annotate("text", x = 25, y = ycord[3], label = "bold(R ~\"=\"~ \"0.91\"~\"(p < 0.001)\")", parse = T, size = text_sz, hjust = 0, color = "#F6756E", family = font) +
  annotate("text", x = 25, y = ycord[4], label = "bold(Bias ~\"=\"~ \"8.86\"~Wm^\"-2\")", parse = T, size = text_sz, hjust = 0, family = font, color = "#F6756E") +
  annotate("text", x = 25, y = ycord[5], label = "bold(RMSE ~\"=\"~ \"119.94\"~Wm^\"-2\")", parse = T, size = text_sz, hjust = 0, family = font, color = "#F6756E") +
  annotate("text", x = 25, y = ycord[6], label = "bold(N ~\"=\"~ \"1465\")", parse = T, size = 5, hjust = 0, family = font, color = "#F6756E") +

  annotate("text", x = 860, y = ycord2[1], label = "[ Clear sky ]", size = 6, hjust = 0, color = "magenta", fontface = "bold", family = font) +
  annotate("text", x = 860, y = ycord2[2], label = "Y = 0.95 X + 73.24", size = text_sz, hjust = 0, color = "#629AFA", fontface = "bold", family = font) +
  annotate("text", x = 860, y = ycord2[3], label = "bold(R ~\"=\"~ \"0.97\"~\"(p < 0.001)\")", parse = T, size = text_sz, hjust = 0, color = "#629AFA", family = font) +
  annotate("text", x = 860, y = ycord2[4], label = "bold(Bias ~\"=\"~ \"36.54\"~Wm^\"-2\")", parse = T, size = text_sz, hjust = 0, family = font, color = "#629AFA") +
  annotate("text", x = 860, y = ycord2[5], label = "bold(RMSE ~\"=\"~ \"51.01\"~Wm^\"-2\")", parse = T, size = text_sz, hjust = 0, family = font, color = "#629AFA") +
  annotate("text", x = 860, y = ycord2[6], label = "bold(N ~\"=\"~ \"204\")", parse = T, size = text_sz, hjust = 0, family = font, color = "#629AFA") +
  # annotate("text", x=200, y=980, label="bold(RMSE ~\"=\"~ \"63.22\" ~Wm^\"-2\"~\"(21.69\"~\"%)\")", parse=T, size=4.2, hjust=0, family=font) +
  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X, Y), size = 1.2) +
  stat_smooth(method = "lm", color = "blue", se = F, aes(X2, Y2), size = 1.2) +
  scale_x_continuous(breaks = seq(0, 1400, by = 200), expand = c(0, 0), limits = c(0, 1400)) +
  scale_y_continuous(breaks = seq(0, 1400, by = 200), expand = c(0, 0), limits = c(0, 1400)) +
  labs(title = "") +
  labs(y = expression(paste(bold("IEODO  Measurement  [Wm"^bold("-2") * "]")))) +
  labs(x = expression(paste(bold("TERRA, AQUA, NPP  CERES [Wm"^bold("-2") * "]")))) +
  # labs(fill = "Count") +
  labs(fill = NULL) +
  theme(plot.title = element_text(face = "bold", size = 20, color = "black")) +
  theme(axis.title.x = element_text(face = "bold", size = 19, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 19, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 19, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 19, colour = "black")) +
  theme(legend.title = element_text(face = "bold", size = 12, colour = "black")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 0.9)) +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  theme(legend.background = element_blank()) +
  theme(text = element_text(family = font)) +
  theme(plot.margin = unit(c(0, 8, 0, 0), "mm")) +
  # png("FIG/albedo.png", width=600, height=600, res=100)
  png("OUTPUT/CERES_IEODO.png", width = 600, height = 600, res = 100)
dev.off()

# boxplot(X, Y)
# length(X)
# plot(X, Y)
# lm.fit = lm(Y ~ X)
# abline(lm.fit, col='red', font=2, lw=2)  ;  abline(a=0, b=1, lty=2, lw=2, col='blue')
# summary(lm.fit)
# cor(X, Y)
# accuracy(Y, X)

#======================================
#     satellite vs gwnu
#======================================
data_sate_gwnu = merge(x = data_ceres_L3, y = data_gwnu_L3, by = c("xran_KST"), all = F)

X = data_sate_gwnu$ceres_obs
Y = data_sate_gwnu$model
length(X)
plot(X, Y)
lmFit = lm(Y ~ X)
abline(lmFit, col = 'red', font = 2, lw = 2); abline(a = 0, b = 1, lty = 2, lw = 2, col = 'blue')
summary(lmFit)
accuracy(Y, X)


#======================================
#     Satellite vs obs vs gwnu (boxplot)
#======================================
X = data_obs_L1$obs
Y = data_gwnu_L3$model
Z = data_ceres_L3$ceres_obs
Xmean = mean(X)
Ymean = mean(Y)
Zmean = mean(Z)

X = data_sate_obs_gwnu$obs
Y = data_sate_obs_gwnu$model
Z = data_sate_obs_gwnu$ceres_obs
Xmean = mean(X)
Ymean = mean(Y)
Zmean = mean(Z)


summary(Z)
sd(X)

ggplot() +
  theme_bw() +
  # theme_grey()+
  geom_boxplot(aes('IEODO', X), colour = '#F8877F', fill = '#F8877F', alpha = 0.2, width = 0.35) +
  geom_boxplot(aes('CERES', Z), colour = '#39C864', fill = '#39C864', alpha = 0.2, width = 0.35) +
  geom_boxplot(aes('GWNU', Y), colour = '#87B3FE', fill = '#87B3FE', alpha = 0.2, width = 0.35) +
  geom_point(aes('IEODO', Xmean), colour = '#C57BFA', size = 4) +
  geom_point(aes('CERES', Xmean), colour = '#C57BFA', size = 4) +
  geom_point(aes('GWNU', Ymean), colour = '#C57BFA', size = 4) +
  annotate("text", x = 'CERES', y = 45, label = "     Min.", size = 5.5, hjust = 0, vjust = 0.5, fontface = "bold", family = font) +
  annotate("text", x = 'CERES', y = 193.3317, label = "     25 %", size = 5.5, hjust = 0, vjust = 0.5, fontface = "bold", family = font) +
  annotate("text", x = 'CERES', y = 419.6302, label = "     50 %", size = 5.5, hjust = 0, vjust = 0.5, fontface = "bold", family = font) +
  annotate("text", x = 'CERES', y = 505, label = "     Mean", size = 5.5, hjust = 0, vjust = 0.5, fontface = "bold", family = font, colour = '#C57BFA') +
  annotate("text", x = 'CERES', y = 737.4104, label = "     75 %", size = 5.5, hjust = 0, vjust = 0.5, fontface = "bold", family = font) +
  annotate("text", x = 'CERES', y = 987.2539, label = "    Max.", size = 5.5, hjust = 0, vjust = 0.5, fontface = "bold", family = font) +
  # geom_jitter(aes('IEODO', X), alpha=0.1) +
  # geom_jitter(aes('GWNU', Y), alpha=0.1) +
  # geom_jitter(aes('CERES', Z), alpha=0.1) +
  scale_y_continuous(breaks = seq(0, 1200, by = 200), expand = c(0, 0), limits = c(0, 1200)) +
  labs(title = "") +
  labs(x = expression(paste(bold("")))) +
  labs(y = expression(paste(bold("Downward  Shortwave  Radiation  [Wm"^bold("-2") * "]")))) +
  # labs(fill = "Count") +
  labs(fill = NULL) +
  theme(plot.title = element_text(face = "bold", size = 20, color = "black")) +
  theme(axis.title.x = element_text(face = "bold", size = 19, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 19, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 19, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 19, colour = "black")) +
  theme(legend.title = element_text(face = "bold", size = 12, colour = "black")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 0.9)) +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  theme(legend.background = element_blank()) +
  theme(text = element_text(family = font)) +
  theme(plot.margin = unit(c(0, 2, 0, 0), "mm")) +
  # png("FIG/albedo.png", width=600, height=600, res=100)
  png("OUTPUT/CERES_IEODO_GWNU_boxplot.png", width = 600, height = 600, res = 100)
dev.off()


boxplot(X, Y, Z, names = c("IEODO", "GWNU", "CERES"), ylim = c(0, 1400), las = 1)
boxplot(X, Y, Z, names = c("IEODO", "GWNU", "CERES"), ylim = c(0, 1400), horizontal = TRUE, las = 1)

X1 = data_obs_L1$xran_KST
Y1 = data_obs_L1$obs
X2 = data_gwnu_L3$xran_KST
Y2 = data_gwnu_L3$model
X3 = data_ceres_L3$xran_KST
Y3 = data_ceres_L3$ceres_obs

#======================================
#     Satellite vs obs vs gwnu
#======================================
data_sate_obs = merge(x = data_obs_L1, y = data_ceres_L3, by = c("xran_KST"), all = F)
data_sate_obs_gwnu = merge(x = data_sate_obs, y = data_gwnu_L3, by = c("xran_KST"), all = F)

X = data_sate_obs_gwnu$ceres_obs
Y = data_sate_obs_gwnu$model
Z = data_sate_obs_gwnu$obs

boxplot(X, Y, Z)

X = data_sate_obs_gwnu$ceres_obs
X = data_sate_obs_gwnu$model
Y = data_sate_obs_gwnu$obs
length(X)
plot(X, Y)
lmFit = lm(Y ~ X)
abline(lmFit, col = 'red', font = 2, lw = 2); abline(a = 0, b = 1, lty = 2, lw = 2, col = 'blue')
summary(lmFit)
accuracy(Y, X)

plot(data_sate_obs_gwnu$xran2, data_sate_obs_gwnu$obs, type = 'l', ylim = c(0, 1200))
points(data_sate_obs_gwnu$xran2, data_sate_obs_gwnu$model, col = 'red', type = 'l')
points(data_sate_obs_gwnu$xran2, data_sate_obs_gwnu$ceres_obs, col = 'blue', type = 'l')
par(new = T)
plot(data_sate_obs_gwnu$xran2, data_sate_obs_gwnu$clear_fraction, col = 'green', type = 'l', yaxt = "n", xlab = "", ylab = "", xaxt = "n", bty = "n")
axis(4, col = 'green', col.axis = 'green', las = 1)

data_sate_obs_gwnu_L1 = data_sate_obs_gwnu %>%
  filter(clear_fraction >= 95 & clear_fraction <= 100)

plot(data_sate_obs_gwnu_L1$xran2, data_sate_obs_gwnu_L1$obs, type = 'l')
points(data_sate_obs_gwnu_L1$xran2, data_sate_obs_gwnu_L1$model, col = 'red', type = 'l')
points(data_sate_obs_gwnu_L1$xran2, data_sate_obs_gwnu_L1$ceres_obs, col = 'blue', type = 'l')
par(new = T)
plot(data_sate_obs_gwnu_L1$xran2, data_sate_obs_gwnu_L1$clear_fraction, col = 'green', type = 'l', yaxt = "n", xlab = "", ylab = "", xaxt = "n", bty = "n")
axis(4, col = 'green', col.axis = 'green', las = 1)

accuracy(data_sate_obs_gwnu_L1$obs, data_sate_obs_gwnu_L1$ceres_obs)
accuracy(data_sate_obs_gwnu_L1$obs, data_sate_obs_gwnu_L1$model)


#============================================
#     Climate_change
#============================================
# IEODO
data_cel = read.csv('./Climate_change/Celiometer_raw.dat', header = F, sep = "")

c01 = as.numeric(substr(data_cel[, 1], 1, 4))
c02 = as.numeric(substr(data_cel[, 1], 6, 7))
c03 = as.numeric(substr(data_cel[, 1], 9, 10))
c04 = as.numeric(substr(data_cel[, 2], 1, 2))
c05 = as.numeric(substr(data_cel[, 2], 4, 5))
c06 = as.numeric(substr(data_cel[, 2], 7, 8))
data2 = data.frame(c01, c02, c03, c04, c05, c06)

data_cel = data.frame(data2, data_cel[, c(-1, -2)])
colnames(data_cel) = c("year", "month", "day", "hour", "min", "sec", "CA_1", "CH_1", "CA_2", "CH_2", "CA_3", "CH_3")

tbl_df(data_cel)

data_cel_L2 = data_cel %>%
  filter(hour >= 9 & hour <= 16) %>%
  filter(year >= 2005 & year <= 2015) %>%
  filter(CA_1 >= 0 & CA_1 <= 8) %>%
  filter(CA_2 >= 0 & CA_2 <= 8) %>%
  mutate(xran = (year + ((month - 1) / 12))) %>%
  mutate(xran2 = ISOdate(year, month, day, hour = 00, min = 00, sec = 00, tz = ""))

tbl_df(data_cel_L2)

IEODO_CA = data_cel_L2 %>%
  group_by(year, month) %>%
  summarise(CA_1_mean = mean(CA_1), CA_2_mean = mean(CA_2), count = n(), xran = mean(xran), xran2 = mean(xran2))

tbl_df(IEODO_CA)

# KOREA (13 point)
data_korea = read.csv('./Climate_change/point.dat', header = F, sep = "")

c01 = as.numeric(substr(data_korea[, 1], 1, 4))
c02 = as.numeric(substr(data_korea[, 1], 6, 7))

data2 = data.frame(c01, c02)

data_korea = data.frame(data2, data_korea[, c(-1)])
colnames(data_korea) = c("year", "month", "temp", "amount", "sw")

tbl_df(data_korea)

# NA
loc1 = which(data_korea[, 3] == -999.0)
data_korea[loc1, 3] = NA
loc2 = which(data_korea[, 4] == -999.0)
data_korea[loc2, 4] = NA
loc3 = which(data_korea[, 5] == -999.0)
data_korea[loc3, 5] = NA

data_korea_L1 = data_korea %>%
  group_by(year, month) %>%
  summarise(mean_temp = mean(temp, na.rm = T), mean_amount = mean(amount, na.rm = T), mean_sw = mean(sw, na.rm = T))

data_korea_L2 = data_korea_L1 %>%
  mutate(xran = (year + ((month - 1) / 12)), mean_amount = (mean_amount / 10) * 8)

tbl_df(data_korea_L2)


#============================
#     CA : Time series plot
#=============================

XX = data_korea_L2$xran
YY = data_korea_L2$mean_amount

png(file = paste("OUTPUT/Climate_change_CA_KOREA", ".png", sep = ""), 1200, 600)
par(mar = c(5.1, 7, 5.1, 5.1), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
plot(XX, YY, xlim = c(2005, 2016), ylim = c(0, 8), type = "l", cex = 1.1, lwd = 2, col = cols[1],
     xlab = "", ylab = "", yaxt = "n", xaxt = "n", main = paste('2005.01.01 ~ 2015.12.31 (Cloud Amount)'))
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(2005, 2016, 1)); mtext('Year  [Time]', side = 1, col = 'black', line = 3, cex = 1.8)
axis(2, col = cols[1], col.axis = cols[1], las = 1, at = seq(0, 8, 1)); mtext(expression(paste(bold("Cloud Amount"))), side = 2, col = cols[1], line = 3, cex = 1.8)
abline(h = seq(0, 8, 1), lw = 1, col = 'lightgrey', lty = "dotted"); abline(v = seq(2005, 2016, 1), lw = 1, col = 'lightgrey', lty = "dotted")
lmFit = lm(YY ~ XX)
abline(lmFit, col = 'red', font = 2, lw = 2)
text(2005, 7.5, paste("(Cloud Amount) =", round(lmFit$coefficients[2], 3), "x (Year) ", round(lmFit$coefficients[1], 3)), adj = 0, col = 'red', font = 2, cex = 1.3)
text(2005, 7.0, paste("R =", round(cor(XX, YY), 3), "(p<0.001)"), adj = 0, col = 'red', font = 2, cex = 1.3)
legend("topright", legend = c("KOREA"), lty = 1, col = cols[1], cex = 1.2, text.font = 1, lwd = 2, bty = "n")
dev.off()

X = IEODO_CA$xran
Y = IEODO_CA$CA_1_mean
X2 = IEODO_CA$xran
Y2 = IEODO_CA$CA_2_mean

png(file = paste("OUTPUT/Climate_change_CA_IEODO", ".png", sep = ""), 1200, 600)
par(mar = c(5.1, 7, 5.1, 5.1), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
plot(X, Y, xlim = c(2005, 2016), ylim = c(0, 8), type = "l", cex = 1.1, lwd = 2, col = cols[2],
     xlab = "", ylab = "", yaxt = "n", xaxt = "n", main = paste('2005.01.01 ~ 2015.12.31 (Cloud Amount)'))
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(2005, 2016, 1)); mtext('Year  [Time]', side = 1, col = 'black', line = 3, cex = 1.8)
axis(2, col = cols[2], col.axis = cols[2], las = 1, at = seq(0, 8, 1)); mtext(expression(paste(bold("Cloud Amount"))), side = 2, col = cols[2], line = 3, cex = 1.8)
abline(h = seq(0, 8, 1), lw = 1, col = 'lightgrey', lty = "dotted"); abline(v = seq(2005, 2016, 1), lw = 1, col = 'lightgrey', lty = "dotted")
lmFit = lm(Y ~ X)
abline(lmFit, col = 'blue', font = 2, lw = 2)
text(2005, 7.5, paste("(First) =", round(lmFit$coefficients[2], 3), "x (Year) +", round(lmFit$coefficients[1], 3)), adj = 0, col = 'blue', font = 2, cex = 1.3)
text(2005, 7.0, paste("R =", round(cor(X, Y), 3), "(p<0.001)"), adj = 0, col = 'blue', font = 2, cex = 1.3)

lines(X2, Y2, type = 'l', col = cols[4])
lmFit = lm(Y2 ~ X2)
abline(lmFit, col = 'lightgreen', font = 2, lw = 2)
text(2005, 6.0, paste("(Second) =", round(lmFit$coefficients[2], 3), "x (Year) +", round(lmFit$coefficients[1], 3)), adj = 0, col = 'lightgreen', font = 2, cex = 1.3)
text(2005, 5.5, paste("R =", round(cor(X2, Y2), 3), "(p<0.001)"), adj = 0, col = 'lightgreen', font = 2, cex = 1.3)
legend("topright", legend = c("IEODO (First)", "IEODO (Second)"), lty = 1, col = cols[3:4], cex = 1.2, text.font = 1, lwd = 2, bty = "n")
dev.off()

#================================
#     TEMP : Time series plot
#================================

IEODO_TEMP = read.csv('./Climate_change/IEODO_TEMP.dat', header = F, sep = "")
colnames(IEODO_TEMP) = c("year", "month", "TEMP")
IEODO_TEMP = IEODO_TEMP %>%
  filter(TEMP != -999.0) %>%
  mutate(xran = (year + ((month - 1) / 12)))
tbl_df(IEODO_TEMP)

#================================
#     TEMP : IEODO ~ xran
#================================
X1 = IEODO_TEMP$xran[1:86]
Y1 = IEODO_TEMP$TEMP[1:86]
X2 = IEODO_TEMP$xran[87:125]
Y2 = IEODO_TEMP$TEMP[87:125]
XX = IEODO_TEMP$xran
YY = IEODO_TEMP$TEMP

png(file = paste("OUTPUT/Climate_change_TEMP_IEODO", ".png", sep = ""), 1200, 600)
par(mar = c(5.1, 7, 5.1, 5.1), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
plot(X1, Y1, xlim = c(2005, 2016), ylim = c(-5, 40), type = "l", cex = 1.1, lwd = 2, col = "black",
     xlab = "", ylab = "", yaxt = "n", xaxt = "n", xaxs = "i", yaxs = "i")
# xlab="", ylab="", yaxt="n", xaxt="n", , xaxs="i", yaxs="i", main=paste('2005.01.01 ~ 2015.12.31 (Temperature)') )
points(X2, Y2, xlim = c(2005, 2016), ylim = c(-5, 40), type = "l", cex = 1.1, lwd = 2, col = "black",
       xlab = "", ylab = "", yaxt = "n", xaxt = "n", xaxs = "i", yaxs = "i")
# xlab="", ylab="", yaxt="n", xaxt="n", main=paste('2005.01.01 ~ 2015.12.31 (Temperature)'), xaxs="i", yaxs="i"  )
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(2005, 2016, 1)); mtext('Year  [Time]', side = 1, col = 'black', line = 3, cex = 1.8)
axis(2, col = "black", col.axis = "black", las = 1, at = seq(-5, 40, 5)); mtext(expression(paste(bold("Monthly  Mean  Temperature  (IORS)  [℃]"))), side = 2, col = "black", line = 3, cex = 1.8)
abline(h = seq(0, 35, 5), lw = 1, col = 'lightgrey', lty = "dotted"); abline(v = seq(2006, 2015, 1), lw = 1, col = 'lightgrey', lty = "dotted")
lmFit = lm(YY ~ XX)
# summary(lm.fit)
abline(lmFit, col = "red", font = 2, lw = 2)
text(2005.1, 37.5, paste("(IORS) =", round(lmFit$coefficients[2], 3), "x (Year) ", round(lmFit$coefficients[1], 3)), adj = 0, col = "black", font = 2, cex = 1.6)
text(2005.1, 32.5, paste("R =", round(cor(XX, YY), 3), "(p<0.001)"), adj = 0, col = "black", font = 2, cex = 1.6)
# legend( "topright", legend=c("IEODO"), lty=1, col="black", cex=1.2, text.font=1, lwd=2, bty="n")
dev.off()

#================================
#     TEMP : KOREA ~ xran
#================================
XX = data_korea_L2$xran
YY = data_korea_L2$mean_temp

png(file = paste("OUTPUT/Climate_change_TEMP_KOR", ".png", sep = ""), 1200, 600)
par(mar = c(5.1, 7, 5.1, 5.1), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
plot(XX, YY, xlim = c(2005, 2016), ylim = c(-5, 40), type = "l", cex = 1.1, lwd = 2, col = "black",
     # xlab="", ylab="", yaxt="n", xaxt="n", main=paste('2005.01.01 ~ 2015.12.31 (Temperature)') )
     xlab = "", ylab = "", yaxt = "n", xaxt = "n", xaxs = "i", yaxs = "i")
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(2005, 2016, 1)); mtext('Year  [Time]', side = 1, col = 'black', line = 3, cex = 1.8)
axis(2, col = "black", col.axis = "black", las = 1, at = seq(-5, 40, 5)); mtext(expression(paste(bold("Monthly  Mean  Temperature  (Korean.)  [℃]"))), side = 2, col = "black", line = 3, cex = 1.8)
abline(h = seq(0, 35, 5), lw = 1, col = 'lightgrey', lty = "dotted"); abline(v = seq(2006, 2015, 1), lw = 1, col = 'lightgrey', lty = "dotted")
lmFit = lm(YY ~ XX)
abline(lmFit, col = 'red', font = 2, lw = 2)
text(2005.1, 37.5, paste("(Korean.) =", round(lmFit$coefficients[2], 3), "x (Year) ", round(lmFit$coefficients[1], 3)), adj = 0, col = "black", font = 2, cex = 1.6)
text(2005.1, 32.5, paste("R =", round(cor(XX, YY), 3), "(p<0.001)"), adj = 0, col = "black", font = 2, cex = 1.6)
# legend( "topright", legend=c("KOREA"), lty=1, col="black", cex=1.2, text.font=1, lwd=2, bty="n")
dev.off()


#================================
#     TEMP : KOREA ~ IEODO
#================================

data_TEMP = merge(x = IEODO_TEMP, y = data_korea_L2, by = c("xran", "year", "month"), all = F)
colnames(data_TEMP) = c("xran", "year", "month", "IEODO", "KOREA_temp", "KOREA_amount", "KOREA_sw")
tbl_df(data_TEMP)


X = data_TEMP$IEODO
Y = data_TEMP$KOREA_temp

png(file = paste("OUTPUT/Climate_change_TEMP_KOR_IEODO", ".png", sep = ""), 700, 700)
par(mar = c(5, 5, 5, 5), cex.main = 1.8, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
plot(X, Y, xlab = 'Temperature  (IEODO) [℃]', ylab = 'Temperature  (KOREA)  [℃]', main = paste('2005.01.01 ~ 2015.12.31 (Temperature)'),
     cex = 2.3, col = 'white', pch = 21, bg = cols[1], xlim = c(-5, 30), ylim = c(-5, 30))
lmFit = lm(Y ~ X)
abline(lmFit, col = 'red', font = 2, lw = 2); abline(a = 0, b = 1, lty = 2, lw = 2, col = 'blue')
grid(lw = 1, col = 'lightgrey', lty = "dotted")
text(-4, 30, paste("(KOREA) =", round(lmFit$coefficients[2], 3), "x (IEODO) ", round(lmFit$coefficients[1], 3)), adj = 0, col = 'red', font = 2, cex = 1.3)
# text(-4, 28, expression(bold(R^"2")), adj=0, font=2, col='red', cex=1.3)
text(-4, 28, paste("R =", round((cor(X, Y)), 3), "(p<0.001)"), adj = 0, col = 'red', font = 2, cex = 1.3)
text(-4, 26, paste("Bias =", round(accuracy(X, Y)[, 1], 3), "(", round(accuracy(X, Y)[, 4], 1), "% )"), adj = 0, col = 'orange', font = 2, cex = 1.3)
text(-4, 24, paste("RMSE =", round(accuracy(X, Y)[, 2], 3), "(", round((accuracy(X, Y)[, 2] / mean(Y)) * 100.0, 2), "% )"), adj = 0, col = 'orange', font = 2, cex = 1.3)
text(-4, 22, paste("N = ", round(length(X), 0)), adj = 0, col = 'black', font = 2, cex = 1.3)
dev.off()

#============================
#     SW : Time series plot
#=============================

IEODO_SW = data_L3_irr     # IEODO QC o (3186)
tbl_df(IEODO_SW)

IEODO_SW = fread("data_L3_not_cloudy", header = F, stringsAsFactors = T, data.table = F)

tbl_df(IEODO_SW)
c01 = as.numeric(substr(IEODO_SW[, 1], 1, 4))
c02 = as.numeric(substr(IEODO_SW[, 1], 5, 6))
c03 = as.numeric(substr(IEODO_SW[, 1], 7, 8))
data2 = data.frame(c01, c02, c03)

IEODO_SW_L1 = data.frame(data2, IEODO_SW[, c(-1, -2, -4:-13)])
colnames(IEODO_SW_L1) = c("year", "month", "day", "DSR")

IEODO_SW_L1 = IEODO_SW_L1 %>% mutate(xran = (year + ((month - 1) / 12)))

IEODO_SW_L2 = IEODO_SW_L1 %>%
  group_by(year, month) %>%
  summarise(DSR_mean = mean(DSR), xran = mean(xran))

tbl_df(IEODO_SW_L2)

data_SW = merge(x = IEODO_SW_L2, y = data_korea_L2, by = c("xran", "year", "month"), all = F)
colnames(data_SW) = c("xran", "year", "month", "IEODO", "KOREA_temp", "KOREA_amount", "KOREA_sw")
tbl_df(data_SW)

#================================
#     DSR : IEODO ~ xran
#================================
X = data_SW$xran
Y = data_SW$IEODO

png(file = paste("OUTPUT/Climate_change_SW_IEODO", ".png", sep = ""), 1200, 600)
par(mar = c(5.1, 7, 5.1, 5.1), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
# plot(X,  Y,  xlim=c(2005, 2016), ylim=c(0, 800), type="l", cex=1.1, lwd=2, col="black",
#      xlab="", ylab="", yaxt="n", xaxt="n", main=paste('2005.01.01 ~ 2015.12.31 (DSR)') )
plot(X, Y, xlim = c(2005, 2016), ylim = c(0, 1000), type = "l", cex = 1.1, lwd = 2, col = "black",
     xlab = "", ylab = "", yaxt = "n", xaxt = "n", xaxs = "i", yaxs = "i")
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(2005, 2016, 1)); mtext('Year  [Time]', side = 1, col = 'black', line = 3, cex = 1.8)
axis(2, col = "black", col.axis = "black", las = 1, at = seq(0, 1000, 200)); mtext(expression(paste(bold("Monthly  Mean  Solar  Radiation  (IORS)  [W/m"^bold("2") * "]"))), side = 2, col = "black", line = 4, cex = 1.8)
abline(h = seq(100, 900, 100), lw = 1, col = 'lightgrey', lty = "dotted"); abline(v = seq(2006, 2015, 1), lw = 1, col = 'lightgrey', lty = "dotted")
lmFit = lm(Y ~ X)
abline(lmFit, col = 'red', font = 2, lw = 2)
text(2005.1, 950, paste("(IORS) =", round(lmFit$coefficients[2], 3), "x (Year) +", round(lmFit$coefficients[1], 3)), adj = 0, col = "black", font = 2, cex = 1.6)
text(2005.1, 850, paste("R =", round(cor(X, Y), 3), "(p<0.001)"), adj = 0, col = "black", font = 2, cex = 1.6)
# legend( "topright", legend=c("IORS"), lty=1, col="black", cex=1.2, text.font=1, lwd=2, bty="n")
dev.off()

#================================
#     DSR : KOREA ~ xran
#================================
X = data_SW$xran
Y = data_SW$KOREA_sw

png(file = paste("OUTPUT/Climate_change_SW_KOREA", ".png", sep = ""), 1200, 600)
par(mar = c(5.1, 7, 5.1, 5.1), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
# plot(X,  Y,  xlim=c(2005, 2016), ylim=c(0, 800), type="l", cex=1.1, lwd=2, col="black",
#      xlab="", ylab="", yaxt="n", xaxt="n", main=paste('2005.01.01 ~ 2015.12.31 (DSR)') )
plot(X, Y, xlim = c(2005, 2016), ylim = c(0, 1000), type = "l", cex = 1.1, lwd = 2, col = "black",
     xlab = "", ylab = "", yaxt = "n", xaxt = "n", xaxs = "i", yaxs = "i")
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(2005, 2016, 1)); mtext('Year  [Time]', side = 1, col = 'black', line = 3, cex = 1.8)
axis(2, col = "black", col.axis = "black", las = 1, at = seq(0, 1000, 200)); mtext(expression(paste(bold("Monthly  Mean  Solar  Radiation  (Korean.)  [W/m"^bold("2") * "]"))), side = 2, col = "black", line = 4, cex = 1.8)
abline(h = seq(100, 900, 100), lw = 1, col = 'lightgrey', lty = "dotted"); abline(v = seq(2006, 2015, 1), lw = 1, col = 'lightgrey', lty = "dotted")
lmFit = lm(Y ~ X)
abline(lmFit, col = 'red', font = 2, lw = 2)
text(2005.1, 950, paste("(Korean.) =", round(lmFit$coefficients[2], 3), "x (Year) +", round(lmFit$coefficients[1], 3)), adj = 0, col = "black", font = 2, cex = 1.6)
text(2005.1, 850, paste("R =", round(cor(X, Y), 3), "(p<0.001)"), adj = 0, col = "black", font = 2, cex = 1.6)
# legend( "topright", legend=c("Korea"), lty=1, col="black", cex=1.2, text.font=1, lwd=2, bty="n")
dev.off()

#================================
#     DSR : KOREA ~ IEODO
#================================
X = data_SW$IEODO
Y = data_SW$KOREA_sw

png(file = paste("OUTPUT/Climate_change_SW_KOR_IEODO", ".png", sep = ""), 700, 700)
par(mar = c(5, 5, 5, 5), cex.main = 1.8, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
# plot( X, Y, xlab=expression(paste(bold("DSR  (IEODO)  [Wm"^bold("-2")*"]"))), ylab=expression(paste(bold("DSR  (KOREA)  [Wm"^bold("-2")*"]"))),
#       main=paste('2005.01.01 ~ 2015.12.31 (DSR)') ,
#       cex=2.3, col='white', pch=21, bg="black", xlim=c(0, 800), ylim=c(0, 800) )
plot(X, Y, xaxs = "i", yaxs = "i", xlab = expression(paste(bold("Monthly  Mean  Solar  Radiation  (IORS)  [W/m"^bold("2") * "]"))),
     ylab = expression(paste(bold("Monthly  Mean  Solar  Radiation  (Korean.)  [W/m"^bold("2") * "]"))),
     cex = 2.0, col = 'black', pch = 21, bg = "white", xlim = c(0, 1000), ylim = c(0, 1000))
lmFit = lm(Y ~ X)
abline(h = seq(200, 800, 200), lw = 1, col = 'lightgrey', lty = "dotted")
abline(v = seq(200, 800, 200), lw = 1, col = 'lightgrey', lty = "dotted")
abline(lmFit, col = 'red', font = 2, lw = 2); abline(a = 0, b = 1, lty = 1, lw = 2, col = 'black')
text(20, 970, paste("(Korean.) =", round(lmFit$coefficients[2], 3), "x (IORS) +", round(lmFit$coefficients[1], 3)), adj = 0, col = 'black', font = 2, cex = 1.5)
# text(-4, 750, expression(bold(R^"2")), adj=0, font=2, col='black', cex=1.3)
text(20, 900, paste("R =", round(cor(X, Y), 2), "(p<0.001)"), adj = 0, col = 'black', font = 2, cex = 1.5)
text(20, 830, paste("Bias =", round(accuracy(X, Y)[, 1], 2), "(", round(accuracy(X, Y)[, 4], 1), "% )"), adj = 0, col = 'black', font = 2, cex = 1.5)
text(20, 760, paste("RMSE =", round(accuracy(X, Y)[, 2], 2), "(", round((accuracy(X, Y)[, 2] / mean(Y)) * 100.0, 2), "% )"), adj = 0, col = 'black', font = 2, cex = 1.5)
text(20, 690, paste("N = ", round(length(X), 0)), adj = 0, col = 'black', font = 2, cex = 1.5)
dev.off()


data_cel = read.csv('./Climate_change/point.dat', header = F, sep = "")
# data_cel = na.omit(data_cel)

c01 = as.numeric(substr(data_cel[, 1], 1, 4))
c02 = as.numeric(substr(data_cel[, 1], 6, 7))

data2 = data.frame(c01, c02)

data_cel = data.frame(data2, data_cel[, c(-1)])
colnames(data_cel) = c("year", "month", "temp", "amount", "sw")

tbl_df(data_cel)

# NA
loc1 = which(data_cel[, 3] == -999.0)
data_cel[loc1, 3] = NA
loc2 = which(data_cel[, 4] == -999.0)
data_cel[loc2, 4] = NA
loc3 = which(data_cel[, 5] == -999.0)
data_cel[loc3, 5] = NA

data_cel_L2 = data_cel %>%
  group_by(year, month) %>%
  summarise(mean_temp = mean(temp, na.rm = T), mean_amount = mean(amount, na.rm = T), mean_sw = mean(sw, na.rm = T))

data_cel_L2 = data_cel_L2 %>%
  mutate(xran = (year + ((month - 1) / 12)))

tbl_df(data_cel_L2)

X = data_cel_L2$xran
Y = data_cel_L2$mean_sw
Y = data_cel_L2$mean_amount
Y = data_cel_L2$mean_temp

plot(X, Y, type = "l")
lmFit = lm(Y ~ X)
abline(lmFit, col = 'red', font = 2, lw = 2)
lmFit
23 * 2 + 3


#==================================
#     Google Map point, Marker
#==================================
iorsGeoLat = c(32.955, 31.1277, 33.12061, 32.1229)
lon = c(129.0814, 120.3954, 126.26712, 125.18)
marker = data.frame(lon, iorsGeoLat)
center = c(125.5, 32)

map = get_googlemap(center, zoom = 6, maptype = 'hybrid', markers = marker)
ggmap(map, extent = 'device') +
  png("OUTPUT/Map_IEODO.png", width = 800, height = 800, res = 100)
dev.off()


#==============================================================
# Paper.R
#==============================================================
#=====================================================
#   URL-API Time setting
#=====================================================

# kst = seq(ISOdatetime(year=2015, month=1, day=1, hour=00, min=00, sec=00),
#           ISOdatetime(year=2017, month=12, day=31, hour=23, min=59, sec=59), by="1 day", tz="")
#
# year = as.numeric(str_sub(kst, 1, 4))
# month = as.numeric(str_sub(kst, 6, 7))
# day = as.numeric(str_sub(kst, 9, 10))
# time = data.frame(c01, c02, c03)
#
# time_L1 = time %>%
#    dplyr::mutate( fn = sprintf("%04d%02d%02d", year, month, day) ) %>%
#    dplyr::select( fn )
#
# write.table(time_L1, file='C:/Users/admin/Desktop/2015_2017_1day.Time', sep=" ", row.names=F, col.names=F)


#=====================================================
#   Time 설정
#=====================================================

kst = seq(ISOdatetime(year = 2014, month = 1, day = 1, hour = 00, min = 00, sec = 00),
          ISOdatetime(year = 2016, month = 12, day = 31, hour = 23, min = 59, sec = 00), by = "1 min", tz = "")

c01 = as.numeric(substr(kst, 1, 4))
c02 = as.numeric(substr(kst, 6, 7))
c03 = as.numeric(substr(kst, 9, 10))
c04 = as.numeric(substr(kst, 12, 13))
c05 = as.numeric(substr(kst, 15, 16))
c06 = as.numeric(substr(kst, 18, 19))
time = data.frame(c01, c02, c03, c04, c05, c06)

time_L1 = time %>%
  mutate(kst = ISOdatetime(year = c01, month = c02, day = c03, hour = c04, min = c05, sec = c06, tz = ""),
         utc = with_tz(kst, "utc"))

colnames(time_L1) = c("year", "month", "day", "hour", "minute", "sec", "kst", "utc")

head(time_L1)
tail(time_L1)


#===========================================================
#   IORS observation
#===========================================================

iors_L1 = read.csv('2nd plan/IORS_20041125-20171231', header = F, sep = "")
colnames(iors_L1) = c("year", "month", "day", "hour", "minute", "sec", "iors_dsr", "sun_shine")

iorsGeoLat = 32.12295277777780
iorsGeoLon = 125.182447222222

head(iors_L1)
tail(iors_L1)


## 원시자료
MONTH = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

iors_L2 = iors_L1 %>%
  dplyr::filter(2014 <= year & year <= 2016) %>%
  dplyr::filter(5 <= hour & hour <= 19) %>%
  dplyr::full_join(time_L1, by = c("year", "month", "day", "hour", "minute", "sec")) %>%
  dplyr::mutate(xran = year +
    ((month - 1) / 12.) +
    ((day - 1) / (12. * MONTH[month])) +
    (hour / (12. * MONTH[month] * 24.)) +
    (minute / (12. * MONTH[month] * 24. * 60.)) +
    (sec / (12. * MONTH[month] * 24. * 60. * 60.)),
                xran2 = Xran(year, month, day, hour, minute, sec, 2),
                fn = as.numeric(sprintf("%04d%02d%02d", year, month, day)),
                kst = ISOdatetime(year = year, month = month, day = day, hour = hour, min = minute, sec = sec, tz = ""),
                utc = with_tz(kst, tz = "UTC"),
                sza = SZA(kst, Lat = iorsGeoLat, Lon = iorsGeoLon),
                jd = yday(kst),
                ga = 2 * pi * (jd - 1) / 365.0,
                Eo = (1.000110 +
                  0.034221 * cos(ga) +
                  0.001280 * sin(ga) +
                  0.000719 * cos(2 * ga) +
                  0.000077 * sin(2 * ga)),
                Max_PPL = (1360.8 * Eo * 1.5 * (cos(sza * pi / 180)^1.2)) + 100,
                Min_PPL = -4,
                Max_ERL = (1360.8 * Eo * 1.2 * (cos(sza * pi / 180)^1.2)) + 50,
                Min_ERL = -2,
                iors_dsr2 = ifelse(iors_dsr == -999.0, NA, iors_dsr)) %>%
  dplyr::filter(!duplicated(kst)) %>%
  dplyr::filter(Max_PPL != "NaN") %>%
  dplyr::filter(Max_ERL != "NaN")

iors_L2[iors_L2 == -999.0] = NA

# n_distinct(a, na.rm=TRUE)  # 중복없는 유일한 관측치 개수 계산
length(a[!is.na(a)]) # 중복있는 관측치 개수 계산 (NA 포함 X)
length(a[is.na(a)])  # 중복있는 관측치 개수 계산 (NA만)

QC = iors_L2 %>%
  dplyr::group_by(year, month, day) %>%
  dplyr::summarise(All = length(Max_PPL[!is.na(Max_PPL)]) + length(Max_PPL[is.na(Max_PPL)]),
                   All_N = length(iors_dsr[!is.na(iors_dsr)]),
                   File_exist = (All_N / All) * 100) %>%
  dplyr::filter(File_exist < 60)

iors_L2_QC = iors_L2 %>%
  dplyr::filter(iors_dsr != -999.0) %>%
  dplyr::anti_join(QC, by = c("year", "month", "day"))

iors_L2_QC2 = iors_L2_QC %>%
  dplyr::filter(9 <= hour & hour <= 16)

iors_L2_QC3 = iors_L2_QC2 %>%
  dplyr::filter(Min_PPL <= iors_dsr & iors_dsr <= Max_PPL) %>%
  dplyr::filter(Min_ERL <= iors_dsr & iors_dsr <= Max_ERL)


## Day time series
YEAR = seq(2014, 2016, 1)
MONTH = seq(1, 12, 1)
DAY = seq(1, 31, 1)

for (i in 1:length(YEAR)) {
  for (j in 1:length(MONTH)) {
    for (k in 1:length(DAY)) {

      # df = iors_L2 %>%
      # df = iors_L2_QC %>%
      # df = iors_L2_QC2 %>%
      df = iors_L2_QC3 %>%
        dplyr::filter(year == YEAR[i]) %>%
        dplyr::filter(month == MONTH[j]) %>%
        dplyr::filter(day == DAY[k]) %>%
        # dplyr::filter( year == 2016 ) %>%
        # dplyr::filter( month == 11 ) %>%
        # dplyr::filter( day == 11 )  %>%
        dplyr::filter(5 <= hour & hour <= 19)

      df2 = na.omit(df)

      X = df$xran2
      Y = df$iors_dsr

      All = length(df$Max_ERL[!is.na(df$Max_ERL)]) + length(df$Max_ERL[is.na(df$Max_ERL)])
      All_N = length(Y[!is.na(Y)])
      All_NA = length(Y[is.na(Y)])

      X2 = df2$xran2
      Y2 = df2$iors_dsr

      cat(YEAR[i], MONTH[j], DAY[k], All, "\n")

      if (length(Y2) > 0) {
        fn = sprintf("%04d%02d%02d", YEAR[i], MONTH[j], DAY[k])

        png(paste0("FIG/QC3/", fn, ".png"), 1300, 650)
        par(mar = c(5.1, 7, 5.1, 5), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
        plot(X, df$Max_PPL, xlim = c(5, 20), col = 'red', ylim = c(0, 1400), xlab = "", ylab = "", yaxt = "n", xaxt = "n", xaxs = "i", yaxs = "i")
        points(X, df$Max_ERL, col = "blue", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
        points(X, Y, pch = 21, bg = "white", col = 'black', xlab = "", ylab = "", yaxt = "n", xaxt = "n")
        axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(5, 20, 1))
        mtext('Time  [Year]', side = 1, col = 'black', line = 3, cex = 2.2)
        axis(2, col = "black", col.axis = "black", las = 1, at = seq(0, 1400, 200))
        mtext(expression(paste(bold("1-Minute  Solar  Radiation  (IORS)  [Wm"^bold("-2") * "]"))), side = 2, col = "black", line = 5, cex = 2.2)
        # abline(h=seq(200, 1000, 200), lw=1, col='lightgrey', lty="dotted")  ;  abline(v=seq(2006, 2015, 1), lw=1, col='lightgrey', lty="dotted")
        lmFit = lm(Y2 ~ X2)
        abline(lmFit, col = 'orange', font = 2, lw = 3)
        text(5.2, 1350, paste0("(IORS) = ", round(lmFit$coefficients[2], 2), " x (Year) + ", round(lmFit$coefficients[1], 2)), adj = 0, col = 'orange', font = 2, cex = 1.5)
        # text(2005.2, 2250, paste0("R = ", round(cor(X, Y),2), " (p<", round(cor.test(X, Y)$p.value,2), ")" ), adj=0, col='orange', font=2, cex=1.5)
        text(5.2, 1250, paste0("R = ", round(cor(X2, Y2), 2), " (p-value < ", round(cor.test(X2, Y2)$p.value, 3), ")"), adj = 0, col = 'orange', font = 2, cex = 1.5)
        # text(2005.2, 2400, paste0("N = ", length(X)), adj=0, col='orange', font=2, cex=1.5)
        text(5.2, 1150, paste0("All = ", All), adj = 0, col = 'orange', font = 2, cex = 1.5)
        text(5.2, 1050, paste0("N = ", All_N), adj = 0, col = 'orange', font = 2, cex = 1.5)
        text(5.2, 950, paste0("NA = ", All_NA), adj = 0, col = 'orange', font = 2, cex = 1.5)
        text(5.2, 850, paste0("%N = ", round(All_N / All * 100, 2)), adj = 0, col = 'orange', font = 2, cex = 1.5)
        text(5.2, 750, paste0("%NA = ", round(All_NA / All * 100, 2)), adj = 0, col = 'orange', font = 2, cex = 1.5)
        dev.off()
      }
    }
  }
}


## Fig.
X = iors_L2$xran
Y = iors_L2$iors_dsr

cor.test(X, Y)

tiff("FIG/GWNU_ori.tif", 1300, 650)
par(mar = c(5.1, 7, 5.1, 5), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
# plot(X,  iors_L2$Max_PPL,  xlim=c(2014, 2017), col='red', ylim=c(0, 2500), xlab="", ylab="", yaxt="n", xaxt="n", xaxs="i", yaxs="i")
# points(X, iors_L2$Max_ERL, col="blue", xlab="", ylab="", yaxt="n", xaxt="n")
# points(X, Y, pch=21, bg="white", col='black', xlab="", ylab="", yaxt="n", xaxt="n")
plot(X, Y, pch = 21, bg = "white", col = 'black', xlab = "", ylab = "", yaxt = "n", xaxt = "n", xlim = c(2014, 2017), ylim = c(0, 2500), xaxs = "i", yaxs = "i")
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(2005, 2017, 1))
mtext('Date  [Year]', side = 1, col = 'black', line = 3, cex = 2.2)
axis(2, col = "black", col.axis = "black", las = 1, at = seq(0, 2500, 500))
mtext(expression(paste(bold("1-Minute  Solar  Radiation  (IORS)  [Wm"^bold("-2") * "]"))), side = 2, col = "black", line = 5, cex = 2.2)
# abline(h=seq(200, 1000, 200), lw=1, col='lightgrey', lty="dotted")  ;  abline(v=seq(2006, 2015, 1), lw=1, col='lightgrey', lty="dotted")
lmFit = lm(Y ~ X)
abline(lmFit, col = 'orange', font = 2, lw = 3)
text(2014.1, 2400, paste0("(IORS) = ", round(lmFit$coefficients[2], 2), " x (Year) ", round(lmFit$coefficients[1], 2)), adj = 0, col = 'orange', font = 2, cex = 1.6)
# text(2005.2, 2250, paste0("R = ", round(cor(X, Y),2), " (p<", round(cor.test(X, Y)$p.value,2), ")" ), adj=0, col='orange', font=2, cex=1.5)
text(2014.1, 2200, paste0("R = ", round(cor.test(X, Y)$estimate, 3), " (p-value < 0.001)"), adj = 0, col = 'orange', font = 2, cex = 1.6)
text(2014.1, 2000, paste0("N = ", length(X)), adj = 0, col = 'orange', font = 2, cex = 1.6)
# text(2005.2, 2400, paste0("N = 1,185,772"), adj=0, col='orange', font=2, cex=1.5)
dev.off()


X = iors_L2_QC$xran
Y = iors_L2_QC$iors_dsr

tiff("FIG/GWNU_QC.tif", 1300, 650)
par(mar = c(5.1, 7, 5.1, 5), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
# plot(X,  iors_L2_QC$Max_PPL,  xlim=c(2014, 2017), col='red', ylim=c(0, 2500), xlab="", ylab="", yaxt="n", xaxt="n", xaxs="i", yaxs="i")
# points(X, iors_L2_QC$Max_ERL, col="blue", xlab="", ylab="", yaxt="n", xaxt="n")
# points(X, Y, pch=21, bg="white", col='black', xlab="", ylab="", yaxt="n", xaxt="n")
plot(X, Y, pch = 21, bg = "white", col = 'black', xlab = "", ylab = "", yaxt = "n", xaxt = "n", xlim = c(2014, 2017), ylim = c(0, 2500), xaxs = "i", yaxs = "i")
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(2014, 2017, 1))
mtext('Date  [Year]', side = 1, col = 'black', line = 3, cex = 2.2)
axis(2, col = "black", col.axis = "black", las = 1, at = seq(0, 2500, 500))
mtext(expression(paste(bold("1-Minute  Solar  Radiation  (IORS)  [Wm"^bold("-2") * "]"))), side = 2, col = "black", line = 5, cex = 2.2)
# abline(h=seq(200, 1000, 200), lw=1, col='lightgrey', lty="dotted")  ;  abline(v=seq(2006, 2015, 1), lw=1, col='lightgrey', lty="dotted")
lmFit = lm(Y ~ X)
abline(lmFit, col = 'orange', font = 2, lw = 3)
text(2014.1, 2400, paste0("(IORS) = ", round(lmFit$coefficients[2], 2), " x (Year) ", round(lmFit$coefficients[1], 2)), adj = 0, col = 'orange', font = 2, cex = 1.6)
# text(2005.2, 2250, paste0("R = ", round(cor(X, Y),2), " (p<", round(cor.test(X, Y)$p.value,3), ")" ), adj=0, col='orange', font=2, cex=1.5)
text(2014.1, 2200, paste0("R = ", round(cor.test(X, Y)$estimate, 3), " (p-value < 0.001)"), adj = 0, col = 'orange', font = 2, cex = 1.6)
text(2014.1, 2000, paste0("N = ", length(X)), adj = 0, col = 'orange', font = 2, cex = 1.6)
# text(2014.2, 2400, paste0("N = 393,883"), adj=0, col='orange', font=2, cex=1.5)
dev.off()


X = iors_L2_QC2$xran
Y = iors_L2_QC2$iors_dsr

tiff("FIG/GWNU_QC2.tif", 1300, 650)
par(mar = c(5.1, 7, 5.1, 5), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
# plot(X,  iors_L2_QC2$Max_PPL,  xlim=c(2014, 2017), col='red', ylim=c(0, 2500), xlab="", ylab="", yaxt="n", xaxt="n", xaxs="i", yaxs="i")
# points(X, iors_L2_QC2$Max_ERL, col="blue", xlab="", ylab="", yaxt="n", xaxt="n")
# points(X, Y, pch=21, bg="white", col='black', xlab="", ylab="", yaxt="n", xaxt="n")
plot(X, Y, pch = 21, bg = "white", col = 'black', xlab = "", ylab = "", yaxt = "n", xaxt = "n", xlim = c(2014, 2017), ylim = c(0, 2500), xaxs = "i", yaxs = "i")
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(2014, 2017, 1))
mtext('Date  [Year]', side = 1, col = 'black', line = 3, cex = 2.2)
axis(2, col = "black", col.axis = "black", las = 1, at = seq(0, 2500, 500))
mtext(expression(paste(bold("1-Minute  Solar  Radiation  (IORS)  [Wm"^bold("-2") * "]"))), side = 2, col = "black", line = 5, cex = 2.2)
# abline(h=seq(200, 1000, 200), lw=1, col='lightgrey', lty="dotted")  ;  abline(v=seq(2006, 2015, 1), lw=1, col='lightgrey', lty="dotted")
lmFit = lm(Y ~ X)
abline(lmFit, col = 'orange', font = 2, lw = 3)
text(2014.1, 2400, paste0("(IORS) = ", round(lmFit$coefficients[2], 2), " x (Year) ", round(lmFit$coefficients[1], 2)), adj = 0, col = 'orange', font = 2, cex = 1.6)
# text(2005.2, 2250, paste0("R = ", round(cor(X, Y),2), " (p<", round(cor.test(X, Y)$p.value,3), ")" ), adj=0, col='orange', font=2, cex=1.5)
text(2014.1, 2200, paste0("R = ", round(cor.test(X, Y)$estimate, 3), " (p-value < 0.001)"), adj = 0, col = 'orange', font = 2, cex = 1.6)
text(2014.1, 2000, paste0("N = ", length(X)), adj = 0, col = 'orange', font = 2, cex = 1.6)
# text(2014.2, 2400, paste0("N = 393,883"), adj=0, col='orange', font=2, cex=1.5)
dev.off()


X = iors_L2_QC3$xran
Y = iors_L2_QC3$iors_dsr

tiff("FIG/GWNU_QC3.tif", 1300, 650)
par(mar = c(5.1, 7, 5.1, 5), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
plot(X, iors_L2_QC3$Max_PPL, xlim = c(2014, 2017), col = 'red', ylim = c(0, 2500), xlab = "", ylab = "", yaxt = "n", xaxt = "n", xaxs = "i", yaxs = "i")
points(X, iors_L2_QC3$Max_ERL, col = "blue", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
points(X, Y, pch = 21, bg = "white", col = 'black', xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(2014, 2017, 1))
mtext('Date  [Year]', side = 1, col = 'black', line = 3, cex = 2.2)
axis(2, col = "black", col.axis = "black", las = 1, at = seq(0, 2500, 500))
mtext(expression(paste(bold("1-Minute  Solar  Radiation  (IORS)  [Wm"^bold("-2") * "]"))), side = 2, col = "black", line = 5, cex = 2.2)
# abline(h=seq(200, 1000, 200), lw=1, col='lightgrey', lty="dotted")  ;  abline(v=seq(2006, 2015, 1), lw=1, col='lightgrey', lty="dotted")
lmFit = lm(Y ~ X)
abline(lmFit, col = 'orange', font = 2, lw = 3)
text(2014.1, 2400, paste0("(IORS) = ", round(lmFit$coefficients[2], 2), " x (Year) ", round(lmFit$coefficients[1], 2)), adj = 0, col = 'orange', font = 2, cex = 1.6)
# text(2005.2, 2250, paste0("R = ", round(cor(X, Y),2), " (p<", round(cor.test(X, Y)$p.value,3), ")" ), adj=0, col='orange', font=2, cex=1.5)
text(2014.1, 2200, paste0("R = ", round(cor.test(X, Y)$estimate, 3), " (p-value < 0.001)"), adj = 0, col = 'orange', font = 2, cex = 1.6)
text(2014.1, 2000, paste0("N = ", length(X)), adj = 0, col = 'orange', font = 2, cex = 1.6)
# text(2014.2, 2400, paste0("N = 393,883"), adj=0, col='orange', font=2, cex=1.5)
dev.off()


## Fig. -----
X = iors_L2$sza
Y = iors_L2$iors_dsr


png("Fig/GWNU_ori_SZA.png", 1300, 650)
# par(mar = c(5.1, 7, 5.1, 5), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2, family = "New Century Schoolbook")
par(mar = c(5.1, 7, 5.1, 5), cex.main = 2.0, cex.lab = 1.6, cex.axis = 1.6, cex = 1.3, font.axis = 2, font.lab = 2, font.main = 2, font = 2)
plot(X, iors_L2$Max_ERL, xlim = c(0, 90), col = 'blue', ylim = c(0, 3000), xlab = "", ylab = "", yaxt = "n", xaxt = "n", xaxs = "i", yaxs = "i")
points(X, iors_L2$Max_PPL, col = "red", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
points(X, Y, xlab = "", ylab = "", yaxt = "n", xaxt = "n", pch = 21, bg = "white", col = 'black')
axis(1, col = 'black', col.axis = 'black', las = 1, at = seq(0, 90, 10))
mtext('Solar  Zenith  Angle', side = 1, col = 'black', line = 3, cex = 2.2)
axis(2, col = "black", col.axis = "black", las = 1, at = seq(0, 3000, 500))
mtext(expression(paste(bold("1/10-Minute  Solar  Radiation  (IORS)  [Wm"^bold("-2") * "]"))), side = 2, col = "black", line = 5, cex = 2.2)
dev.off()


## COMS/MI DSR
data = read.csv("2nd plan/COMS_IEODO_INS_20150101-20170825.OUT", sep = "", head = F)
head(data)

c01 = as.numeric(substr(data[, 1], 1, 4))
c02 = as.numeric(substr(data[, 1], 5, 6))
c03 = as.numeric(substr(data[, 1], 7, 8))
c04 = as.numeric(substr(data[, 1], 9, 10))
c05 = as.numeric(substr(data[, 1], 11, 12))
data2 = data.frame(c01, c02, c03, c04, c05)
coms_ins = data.frame(data2, data[, -1])

colnames(coms_ins) = c("year", "month", "day", "hour", "minute", "coms_dsr")

coms_ins_L1 = coms_ins %>%
  dplyr::mutate(kst = ISOdatetime(year = year, month = month, day = day, hour = hour, min = minute, sec = 00, tz = ""),
                utc = with_tz(kst, "utc")) %>%
  dplyr::filter(!duplicated(kst))


head(coms_ins_L1)


## COMS/MI CA
data = read.csv("2nd plan/COMS_IEODO_CA_20121202-20170825.OUT", sep = "", head = F)
head(data)

c01 = as.numeric(substr(data[, 1], 1, 4))
c02 = as.numeric(substr(data[, 1], 5, 6))
c03 = as.numeric(substr(data[, 1], 7, 8))
c04 = as.numeric(substr(data[, 1], 9, 10))
c05 = as.numeric(substr(data[, 1], 11, 12))
data2 = data.frame(c01, c02, c03, c04, c05)
coms_ca = data.frame(data2, data[, -1])

colnames(coms_ca) = c("year", "month", "day", "hour", "minute", "coms_ca")

coms_ca_L1 = coms_ca %>%
  dplyr::mutate(kst = ISOdatetime(year = c01, month = c02, day = c03, hour = c04, min = c05, sec = 00, tz = ""),
                utc = with_tz(kst, "utc"))
dplyr::filter(!duplicated(kst))


head(coms_ca_L1)


## GWNU Model
gwnu1 = read.csv("2nd plan/GWNU_IORS_20130101-20151231.OUT", sep = "", head = F)
colnames(gwnu1) = c("year", "month", "day", "hour", "minute", "lat", "lon", "sza", "cos_sza",
                    "ext", "alt", "alb", "oz", "aer", "temp", "slp", "sp", "tpw", "cm", "cf",
                    "vis", "gwnu_dsr", "mj", "tot", "dir", "dif")

head(gwnu1)

gwnu2 = read.csv("2nd plan/GWNU_IORS_20160101-20161231.dat", sep = "", head = F)
colnames(gwnu2) = c("year", "month", "day", "hour", "minute", "lat", "lon", "sza", "cos_sza",
                    "ext", "alt", "alb", "oz", "aer", "temp", "slp", "sp", "tpw", "cm", "cf",
                    "vis", "gwnu_dsr", "mj", "tot", "dir", "dif")


gwnu2_L1 = gwnu2 %>%
  dplyr::mutate(utc = ISOdatetime(year = year, month = month, day = day, hour = hour, min = minute, sec = 00, tz = "utc"),
                kst = with_tz(utc, ""),
                year = as.numeric(format(kst, "%Y")),
                month = as.numeric(format(kst, "%m")),
                day = as.numeric(format(kst, "%d")),
                hour = as.numeric(format(kst, "%H")),
                minute = as.numeric(format(kst, "%M"))) %>%
  # sec    = as.numeric(format(kst, "%S")) )
  dplyr::select(-utc, -kst)


gwnu = rbind(gwnu1, gwnu2_L1)

gwnu_L1 = gwnu %>%
  # dplyr::filter( between(year, 2016, 2016) )  %>%
  dplyr::filter(between(hour, 9, 15)) %>%
  dplyr::mutate(kst = ISOdatetime(year = year, month = month, day = day, hour = hour, min = minute, sec = 00, tz = ""),
                xran = year +
                  ((month - 1) / 12.) +
                  ((day - 1) / (12. * MONTH[month])) +
                  (hour / (12. * MONTH[month] * 24.)) +
                  (minute / (12. * MONTH[month] * 24. * 60.)),
                utc = with_tz(kst, "")) %>%
  dplyr::filter(!duplicated(kst))


gwnu_L2 = gwnu_L1 %>%
  dplyr::filter(between(hour, 9, 15)) %>%
  filter(year >= 2014)

head(gwnu_L2)


#================================================
#     Obs. merge  &  Moving average
#================================================
data_L1 = time_L1 %>%
  dplyr::full_join(iors_L2_QC3, by = c("kst", "utc")) %>%
  dplyr::full_join(coms_ins_L1, by = c("kst", "utc")) %>%
  dplyr::full_join(coms_ca_L1, by = c("kst", "utc")) %>%
  dplyr::full_join(gwnu_L1, by = c("kst", "utc")) %>%
  dplyr::select(year.x, month.x, day.x, hour.x, minute.x, sec.x, kst, utc, coms_dsr, coms_ca, gwnu_dsr, iors_dsr, cm, xran.x) %>%
  dplyr::rename(year = year.x, month = month.x, hour = hour.x, minute = minute.x, sec = sec.x, xran = xran.x)

head(data_L1)

write.table(data_L1, file = '2015_2017_COMS_GWNU_IEODO_DATA_L1.DSR', sep = " ", row.names = F, col.names = F)

data_L2 = na.omit(data_L1)

write.table(data_L2, file = '2015_2017_COMS_GWNU_IEODO_DATA_L2.DSR', sep = " ", row.names = F, col.names = F)

QC = data_L2 %>%
  dplyr::filter(abs(iors_dsr - gwnu_dsr) > 300) %>%
  dplyr::filter(abs(iors_dsr - gwnu_dsr) > 300)

index = sample(1:nrow(QC), length(QC[, 1]) * 0.8, replace = F)
QC2 = QC[index,]


data_L4_clear = data_L2 %>%
  dplyr::anti_join(QC2, by = c("kst", "utc")) %>%
  dplyr::filter(cm == 0) %>%
  dplyr::filter(coms_ca == 0)

data_L4 = data_L2 %>%
  dplyr::anti_join(QC2, by = c("kst", "utc"))


X = data_L4$gwnu_dsr
Y = data_L4$iors_dsr

X2 = data_L4_clear$gwnu_dsr
Y2 = data_L4_clear$iors_dsr


val = perfEval(X, Y); sprintf("%.2f", val)
val2 = perfEval(X2, Y2); sprintf("%.2f", val2)

xcord = 20
ycord = seq(1370, 0, -80)

xcord2 = 620
ycord2 = seq(440, 0, -80)


## clear (red), cloudy (blue)
ggplot() +
  coord_fixed(ratio = 1) +
  theme_bw() +
  geom_point(aes(X, Y), shape = 21, size = 4, colour = "blue", fill = "white", alpha = 1) +
  geom_point(aes(X2, Y2), shape = 21, size = 4, colour = "red", fill = "white", alpha = 1) +
  # stat_bin2d(bins=200) +
  # scale_fill_gradientn(colours = myPalette(11), limits=c(0, 20), na.value="#9E0142") +
  # scale_fill_gradientn(colours=matlab.like(11), limits=c(0, 40), na.value="#AA0000") +
  # scale_fill_gradientn(colours=matlab.like2(11), limits=c(0, 40), na.value="#BF0000") +
  annotate("text", x = xcord, y = ycord[1], label = "――――― All sky ―――――", size = 5.5, hjust = 0, colour = "blue", fontface = "bold", family = font) +
  annotate("text", x = xcord, y = ycord[2], label = paste0("(IORS) = ", sprintf("%.2f", val[1]), " x (GWNU) ", sprintf("%.2f", val[2])), size = 5, hjust = 0, colour = "blue", fontface = "bold", family = font) +
  annotate("text", x = xcord, y = ycord[3], label = paste0("R = ", sprintf("%.2f", val[12]), " (p-value < 0.001)"), size = 5, hjust = 0, colour = "blue", family = font, fontface = "bold") +
  annotate("text", x = xcord, y = ycord[4], label = paste0("Bias = ", sprintf("%.2f", val[8]), " (", sprintf("%.2f", val[9]), " %)"), parse = F, size = 5, hjust = 0, family = font, fontface = "bold", color = "black") +
  annotate("text", x = xcord, y = ycord[5], label = paste0("RMSE = ", sprintf("%.2f", val[10]), " (", sprintf("%.2f", val[11]), " %)"), parse = F, size = 5, hjust = 0, family = font, fontface = "bold", color = "black") +
  annotate("text", x = xcord, y = ycord[6], label = paste0("N = ", sprintf("%.0f", val[7])), size = 5, hjust = 0, colour = "black", family = font, fontface = "bold", colour = "black") +
  # geom_abline(intercept=0, slope=1, linetype=1, colour="black", size=1) +
  # stat_smooth(method="lm", colour="blue", se=F, aes(X, Y)) +
  scale_x_continuous(minor_breaks = seq(0, 1400, by = 200), breaks = seq(0, 1400, by = 200), expand = c(0, 0), limits = c(0, 1400)) +
  scale_y_continuous(minor_breaks = seq(0, 1400, by = 200), breaks = seq(0, 1400, by = 200), expand = c(0, 0), limits = c(0, 1400)) +

  annotate("text", x = xcord2, y = ycord2[1], label = "――――― Clear sky ―――――", size = 5.5, hjust = 0, colour = "red", fontface = "bold", family = font) +
  annotate("text", x = xcord2, y = ycord2[2], label = paste0("(IORS) = ", sprintf("%.2f", val2[1]), " x (GWNU) +", sprintf("%.2f", val2[2])), size = 5, hjust = 0, colour = "red", fontface = "bold", family = font) +
  annotate("text", x = xcord2, y = ycord2[3], label = paste0("R = ", sprintf("%.2f", val2[12]), " (p-value < 0.001)"), size = 5, hjust = 0, colour = "red", family = font, fontface = "bold") +
  annotate("text", x = xcord2, y = ycord2[4], label = paste0("Bias = ", sprintf("%.2f", val2[8]), " (", sprintf("%.2f", val2[9]), " %)"), parse = F, size = 5, hjust = 0, family = font, fontface = "bold", color = "black") +
  annotate("text", x = xcord2, y = ycord2[5], label = paste0("RMSE = ", sprintf("%.2f", val2[10]), " (", sprintf("%.2f", val2[11]), " %)"), parse = F, size = 5, hjust = 0, family = font, fontface = "bold", color = "black") +
  annotate("text", x = xcord2, y = ycord2[6], label = paste0("N = ", sprintf("%.0f", val2[7])), size = 5, hjust = 0, colour = "black", family = font, fontface = "bold", colour = "black") +
  # annotate("text", x=200, y=980, label="bold(RMSE ~\"=\"~ \"63.22\" ~Wm^\"-2\"~\"(21.69\"~\"%)\")", parse=T, size=4.2, hjust=0, family=font) +
  geom_abline(intercept = 0, slope = 1, linetype = 1, colour = "black", size = 1) +
  stat_smooth(method = "lm", colour = "blue", se = F, aes(X, Y)) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X2, Y2), size = 1) +
  labs(title = "") +
  labs(x = expression(paste(bold("GWNU  Radiation  Model  DSR  [Wm"^bold("-2") * "]")))) +
  labs(y = expression(paste(bold("IORS  Observatory  DSR  [Wm"^bold("-2") * "]")))) +
  # labs(fill = "Count") +
  labs(fill = NULL) +
  theme(plot.title = element_text(face = "bold", size = 20, color = "black")) +
  theme(axis.title.x = element_text(face = "bold", size = 19, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 19, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 19, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 19, colour = "black")) +
  theme(legend.title = element_text(face = "bold", size = 12, colour = "black")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 0.96)) +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  theme(legend.background = element_blank()) +
  theme(text = element_text(family = font)) +
  theme(plot.margin = unit(c(0, 8, 0, 0), "mm")) +
  ggsave(filename = paste0('FIG/scatterplot_GWNU_IORS.tif'), device = "tiff", width = 7, height = 7, dpi = 600)


X = data_L4$coms_dsr
Y = data_L4$iors_dsr

X2 = data_L4_clear$coms_dsr
Y2 = data_L4_clear$iors_dsr

val = perfEval(X, Y); sprintf("%.2f", val)
val2 = perfEval(X2, Y2); sprintf("%.2f", val2)

xcord = 20
ycord = seq(1370, 0, -80)

xcord2 = 620
ycord2 = seq(440, 0, -80)

ggplot() +
  coord_fixed(ratio = 1) +
  theme_bw() +
  geom_point(aes(X, Y), shape = 21, size = 4, colour = "blue", fill = "white", alpha = 1) +
  geom_point(aes(X2, Y2), shape = 21, size = 4, colour = "red", fill = "white", alpha = 1) +
  # stat_bin2d(bins=200) +
  # scale_fill_gradientn(colours = myPalette(11), limits=c(0, 20), na.value="#9E0142") +
  # scale_fill_gradientn(colours=matlab.like(11), limits=c(0, 40), na.value="#AA0000") +
  # scale_fill_gradientn(colours=matlab.like2(11), limits=c(0, 40), na.value="#BF0000") +
  annotate("text", x = xcord, y = ycord[1], label = "――――― All sky ―――――", size = 5.5, hjust = 0, colour = "blue", fontface = "bold", family = font) +
  annotate("text", x = xcord, y = ycord[2], label = paste0("(IORS) = ", sprintf("%.2f", val[1]), " x (COMS) ", sprintf("%.2f", val[2])), size = 5, hjust = 0, colour = "blue", fontface = "bold", family = font) +
  annotate("text", x = xcord, y = ycord[3], label = paste0("R = ", sprintf("%.2f", val[12]), " (p-value < 0.001)"), size = 5, hjust = 0, colour = "blue", family = font, fontface = "bold") +
  annotate("text", x = xcord, y = ycord[4], label = paste0("Bias = ", sprintf("%.2f", val[8]), " (", sprintf("%.2f", val[9]), " %)"), parse = F, size = 5, hjust = 0, family = font, fontface = "bold", color = "black") +
  annotate("text", x = xcord, y = ycord[5], label = paste0("RMSE = ", sprintf("%.2f", val[10]), " (", sprintf("%.2f", val[11]), " %)"), parse = F, size = 5, hjust = 0, family = font, fontface = "bold", color = "black") +
  annotate("text", x = xcord, y = ycord[6], label = paste0("N = ", sprintf("%.0f", val[7])), size = 5, hjust = 0, colour = "black", family = font, fontface = "bold", colour = "black") +
  # geom_abline(intercept=0, slope=1, linetype=1, colour="black", size=1) +
  # stat_smooth(method="lm", colour="blue", se=F, aes(X, Y)) +
  scale_x_continuous(minor_breaks = seq(0, 1400, by = 200), breaks = seq(0, 1400, by = 200), expand = c(0, 0), limits = c(0, 1400)) +
  scale_y_continuous(minor_breaks = seq(0, 1400, by = 200), breaks = seq(0, 1400, by = 200), expand = c(0, 0), limits = c(0, 1400)) +

  annotate("text", x = xcord2, y = ycord2[1], label = "――――― Clear sky ―――――", size = 5.5, hjust = 0, colour = "red", fontface = "bold", family = font) +
  annotate("text", x = xcord2, y = ycord2[2], label = paste0("(IORS) = ", sprintf("%.2f", val2[1]), " x (COMS) +", sprintf("%.2f", val2[2])), size = 5, hjust = 0, colour = "red", fontface = "bold", family = font) +
  annotate("text", x = xcord2, y = ycord2[3], label = paste0("R = ", sprintf("%.2f", val2[12]), " (p-value < 0.001)"), size = 5, hjust = 0, colour = "red", family = font, fontface = "bold") +
  annotate("text", x = xcord2, y = ycord2[4], label = paste0("Bias = ", sprintf("%.2f", val2[8]), " (", sprintf("%.2f", val2[9]), " %)"), parse = F, size = 5, hjust = 0, family = font, fontface = "bold", color = "black") +
  annotate("text", x = xcord2, y = ycord2[5], label = paste0("RMSE = ", sprintf("%.2f", val2[10]), " (", sprintf("%.2f", val2[11]), " %)"), parse = F, size = 5, hjust = 0, family = font, fontface = "bold", color = "black") +
  annotate("text", x = xcord2, y = ycord2[6], label = paste0("N = ", sprintf("%.0f", val2[7])), size = 5, hjust = 0, colour = "black", family = font, fontface = "bold", colour = "black") +
  # annotate("text", x=200, y=980, label="bold(RMSE ~\"=\"~ \"63.22\" ~Wm^\"-2\"~\"(21.69\"~\"%)\")", parse=T, size=4.2, hjust=0, family=font) +
  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1) +
  stat_smooth(method = "lm", colour = "blue", se = F, aes(X, Y)) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X2, Y2), size = 1) +
  labs(title = "") +
  labs(x = expression(paste(bold("COMS / MI  DSR  [Wm"^bold("-2") * "]")))) +
  labs(y = expression(paste(bold("IORS  Observatory  DSR  [Wm"^bold("-2") * "]")))) +
  # labs(fill = "Count") +
  labs(fill = NULL) +
  theme(plot.title = element_text(face = "bold", size = 20, color = "black")) +
  theme(axis.title.x = element_text(face = "bold", size = 19, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 19, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 19, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 19, colour = "black")) +
  theme(legend.title = element_text(face = "bold", size = 12, colour = "black")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 0.96)) +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  theme(legend.background = element_blank()) +
  theme(text = element_text(family = font)) +
  theme(plot.margin = unit(c(0, 8, 0, 0), "mm")) +
  ggsave(filename = paste0('FIG/scatterplot_COMS_IORS.tif'), device = "tiff", width = 7, height = 7, dpi = 600)


#====================================================
#  Time series
#====================================================

X = iors_L3_QC$xran
Y = iors_L3_QC$iors_dsr

X = gwnu_L2$xran
Y = gwnu_L2$gwnu_dsr

# plot(X, Y)

val = perfEval(X, Y); sprintf("%.2f", val)

cor.test(X, Y)

xcord = 2013.05
ycord = seq(1350, 0, -100)

ggplot() +
  # coord_fixed(ratio=1) +
  theme_bw() +
  # geom_point(aes(X, Y)) +
  geom_point(aes(X, Y), shape = 21, colour = "black", fill = "white", size = 2.5) +
  annotate("text", x = xcord, y = ycord[1], label = paste0("(DSR) = ", sprintf("%.2f", val[1]), " x (Year) + ", sprintf("%.2f", val[2])), size = 6, hjust = 0, color = "red", fontface = "bold", family = font) +
  # annotate("text", x=250, y=ycord[2], label=paste0("R = ", sprintf("%.2f",val[12]), " (p < 0.001) | Stdev = ", sprintf("%.2f",val[14])), size=5, hjust=0, color="red", family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", val[12]), " (p-value < 0.001)"), size = 6, hjust = 0, colour = "red", family = font, fontface = "bold") +
  # annotate("text", x=250, y=ycord[3], label=paste0("Stdev = ", sprintf("%.2f",val[14])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=250, y=ycord[4], label=paste0("AMI = ", sprintf("%.3f",val[3]), " | CERES = ", sprintf("%.3f",val[4])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[3], label=paste0("Bias = ", sprintf("%.2f",val[8]), " (", sprintf("%.2f",val[9])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[4], label=paste0("RMSE = ", sprintf("%.2f",val[10]), " (", sprintf("%.2f",val[11])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[3], label=paste0("N = ", sprintf("%.0f",val[7]) ), size=6, hjust=0, color="black", family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[3], label = paste0("N = 32,311"), size = 6, hjust = 0, color = "black", family = font, fontface = "bold") +
  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X, Y)) +
  scale_x_continuous(minor_breaks = seq(2013, 2017, by = 1), breaks = seq(2013, 2017, by = 1), expand = c(0, 0), limits = c(2013, 2017)) +
  scale_y_continuous(minor_breaks = seq(0, 1400, by = 200), breaks = seq(0, 1400, by = 200), expand = c(0, 0), limits = c(0, 1400)) +
  # labs(title = "Sunshine Duration  by  Sunshine Recorder  of  2013/01/01-2016/12/31") +
  labs(title = "") +
  labs(y = expression(paste(bold("GWNU  Radiation  Model  DSR  [Wm"^bold("-2") * "]")))) +
  # labs(y = expression(paste(bold("IORS  Observatory  DSR  [Wm"^bold("-2")*"]")))) +
  labs(x = expression(paste(bold("Time  [Year]")))) +
  labs(fill = NULL) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(face = "bold", size = 20, color = "black")) +
  theme(axis.title.x = element_text(face = "bold", size = 20, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 20, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 20, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 20, colour = "black")) +
  theme(legend.title = element_text(face = "bold", size = 12, colour = "black")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 0.9)) +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  theme(legend.background = element_blank()) +
  theme(text = element_text(family = font)) +
  theme(plot.margin = unit(c(0, 8, 0, 0), "mm")) +
  ggsave(filename = paste0('Fig/GWNU_DSR.png'), width = 12, height = 6, dpi = 600)
# ggsave(filename=paste0('Fig/IORS_DSR.png'), width=12, height=6, dpi=600 )


############
X = gwnu_L2$xran
Y = gwnu_L2$aer

val = perfEval(X, Y); sprintf("%.2f", val)

xcord = 2013.05
ycord = seq(2.9, 0, -0.25)

ggplot() +
  # coord_fixed(ratio=1) +
  theme_bw() +
  # geom_point(aes(X, Y)) +
  geom_point(aes(X, Y), shape = 21, colour = "black", fill = "white", size = 2.5) +
  annotate("text", x = xcord, y = ycord[1], label = paste0("(AOD) = ", sprintf("%.2f", val[1]), " x (Year) + ", sprintf("%.2f", val[2])), size = 6, hjust = 0, color = "red", fontface = "bold", family = font) +
  # annotate("text", x=250, y=ycord[2], label=paste0("R = ", sprintf("%.2f",val[12]), " (p < 0.001) | Stdev = ", sprintf("%.2f",val[14])), size=5, hjust=0, color="red", family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", val2[12]), " (p-value < 0.001)"), size = 6, hjust = 0, colour = "red", family = font, fontface = "bold") +
  # annotate("text", x=250, y=ycord[3], label=paste0("Stdev = ", sprintf("%.2f",val[14])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=250, y=ycord[4], label=paste0("AMI = ", sprintf("%.3f",val[3]), " | CERES = ", sprintf("%.3f",val[4])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[3], label=paste0("Bias = ", sprintf("%.2f",val[8]), " (", sprintf("%.2f",val[9])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[4], label=paste0("RMSE = ", sprintf("%.2f",val[10]), " (", sprintf("%.2f",val[11])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[3], label=paste0("N = ", sprintf("%.0f",val[7]) ), size=6, hjust=0, color="black", family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[3], label = paste0("N = 32,311"), size = 6, hjust = 0, color = "black", family = font, fontface = "bold") +
  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X, Y)) +
  scale_x_continuous(minor_breaks = seq(2013, 2017, by = 1), breaks = seq(2013, 2017, by = 1), expand = c(0, 0), limits = c(2013, 2017)) +
  scale_y_continuous(minor_breaks = seq(0, 3, by = 0.5), breaks = seq(0, 3, by = 0.5), expand = c(0, 0), limits = c(0, 3)) +
  # labs(title = "Sunshine Duration  by  Sunshine Recorder  of  2013/01/01-2016/12/31") +
  labs(title = "") +
  labs(y = expression(paste(bold("Aerosol  Optical  Depth by GWNU")))) +
  labs(x = expression(paste(bold("Time  [Year]")))) +
  labs(fill = NULL) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(face = "bold", size = 20, color = "black")) +
  theme(axis.title.x = element_text(face = "bold", size = 20, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 20, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 20, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 20, colour = "black")) +
  theme(legend.title = element_text(face = "bold", size = 12, colour = "black")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 0.9)) +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  theme(legend.background = element_blank()) +
  theme(text = element_text(family = font)) +
  theme(plot.margin = unit(c(0, 8, 0, 2), "mm")) +
  ggsave(filename = paste0('Fig/GWNU_AOD.png'), width = 12, height = 6, dpi = 600)


############
X = gwnu_L2$xran
Y = gwnu_L2$tpw

# plot(X, Y)

val = perfEval(X, Y); sprintf("%.2f", val)

xcord = 2014.05
ycord = seq(9.5, 0, -1)

ggplot() +
  # coord_fixed(ratio=1) +
  theme_bw() +
  # geom_point(aes(X, Y)) +
  geom_point(aes(X, Y), shape = 21, colour = "black", fill = "white", size = 2.5) +
  annotate("text", x = xcord, y = ycord[1], label = paste0("(TPW) = ", sprintf("%.2f", val[1]), " x (Year) + ", sprintf("%.2f", val[2])), size = 6, hjust = 0, color = "red", fontface = "bold", family = font) +
  # annotate("text", x=250, y=ycord[2], label=paste0("R = ", sprintf("%.2f",val[12]), " (p < 0.001) | Stdev = ", sprintf("%.2f",val[14])), size=5, hjust=0, color="red", family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", val2[12]), " (p-value < 0.001)"), size = 6, hjust = 0, colour = "red", family = font, fontface = "bold") +
  # annotate("text", x=250, y=ycord[3], label=paste0("Stdev = ", sprintf("%.2f",val[14])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=250, y=ycord[4], label=paste0("AMI = ", sprintf("%.3f",val[3]), " | CERES = ", sprintf("%.3f",val[4])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[3], label=paste0("Bias = ", sprintf("%.2f",val[8]), " (", sprintf("%.2f",val[9])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[4], label=paste0("RMSE = ", sprintf("%.2f",val[10]), " (", sprintf("%.2f",val[11])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[3], label=paste0("N = ", sprintf("%.0f",val[7]) ), size=6, hjust=0, color="black", family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[3], label = paste0("N = 32,311"), size = 6, hjust = 0, color = "black", family = font, fontface = "bold") +
  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X, Y)) +
  scale_x_continuous(minor_breaks = seq(2014, 2017, by = 1), breaks = seq(2014, 2017, by = 1), expand = c(0, 0), limits = c(2014, 2017)) +
  scale_y_continuous(minor_breaks = seq(0, 10, by = 2), breaks = seq(0, 10, by = 2), expand = c(0, 0), limits = c(0, 10)) +
  # labs(title = "Sunshine Duration  by  Sunshine Recorder  of  2013/01/01-2016/12/31") +
  labs(title = "") +
  labs(y = expression(paste(bold("Total  Precipitable  Water by GWNU  [cm]")))) +
  labs(x = expression(paste(bold("Date  [Year]")))) +
  labs(fill = NULL) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(face = "bold", size = 19, color = "black")) +
  theme(axis.title.x = element_text(face = "bold", size = 19, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 19, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 19, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 19, colour = "black")) +
  theme(legend.title = element_text(face = "bold", size = 12, colour = "black")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 0.9)) +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  theme(legend.background = element_blank()) +
  theme(text = element_text(family = font)) +
  theme(plot.margin = unit(c(9.5, 8, 0, 2), "mm")) +
  ggsave(filename = paste0('Fig/GWNU_TPW.png'), width = 12, height = 6, dpi = 600)


############
X = gwnu_L2$xran
Y = 1 - (gwnu_L2$gwnu_dsr / gwnu_L1$tot)

# plot(X, Y)

val = perfEval(X, Y); sprintf("%.2f", val)

xcord = 2013.05
ycord = seq(0.97, 0, -0.1)

ggplot() +
  # coord_fixed(ratio=1) +
  theme_bw() +
  # geom_point(aes(X, Y)) +
  geom_point(aes(X, Y), shape = 21, colour = "black", fill = "white", size = 2.5) +
  annotate("text", x = xcord, y = ycord[1], label = paste0("(CA) = ", sprintf("%.2f", val[1]), " x (Year) + ", sprintf("%.2f", val[2])), size = 6, hjust = 0, color = "red", fontface = "bold", family = font) +
  # annotate("text", x=250, y=ycord[2], label=paste0("R = ", sprintf("%.2f",val[12]), " (p < 0.001) | Stdev = ", sprintf("%.2f",val[14])), size=5, hjust=0, color="red", family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", val2[12]), " (p-value < 0.001)"), size = 6, hjust = 0, colour = "red", family = font, fontface = "bold") +
  # annotate("text", x=250, y=ycord[3], label=paste0("Stdev = ", sprintf("%.2f",val[14])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=250, y=ycord[4], label=paste0("AMI = ", sprintf("%.3f",val[3]), " | CERES = ", sprintf("%.3f",val[4])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[3], label=paste0("Bias = ", sprintf("%.2f",val[8]), " (", sprintf("%.2f",val[9])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[4], label=paste0("RMSE = ", sprintf("%.2f",val[10]), " (", sprintf("%.2f",val[11])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[3], label=paste0("N = ", sprintf("%.0f",val[7]) ), size=6, hjust=0, color="black", family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[3], label = paste0("N = 32,311"), size = 6, hjust = 0, color = "black", family = font, fontface = "bold") +
  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X, Y)) +
  scale_x_continuous(minor_breaks = seq(2013, 2017, by = 1), breaks = seq(2013, 2017, by = 1), expand = c(0, 0), limits = c(2013, 2017)) +
  scale_y_continuous(minor_breaks = seq(0, 1, by = 0.2), breaks = seq(0, 1, by = 0.2), expand = c(0, 0), limits = c(0, 1)) +
  # labs(title = "Sunshine Duration  by  Sunshine Recorder  of  2013/01/01-2016/12/31") +
  labs(title = "") +
  labs(y = expression(paste(bold("Cloud  Amount by GWNU")))) +
  labs(x = expression(paste(bold("Time  [Year]")))) +
  labs(fill = NULL) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(face = "bold", size = 20, color = "black")) +
  theme(axis.title.x = element_text(face = "bold", size = 20, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 20, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 20, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 20, colour = "black")) +
  theme(legend.title = element_text(face = "bold", size = 12, colour = "black")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 0.9)) +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  theme(legend.background = element_blank()) +
  theme(text = element_text(family = font)) +
  theme(plot.margin = unit(c(0, 8, 0, 2), "mm")) +
  ggsave(filename = paste0('Fig/GWNU_CA.png'), width = 12, height = 6, dpi = 600)


X = gwnu_L2$xran
Y = gwnu_L2$tpw
Y = gwnu_L2$cf
Y = gwnu_L2$gwnu_dsr / gwnu_L1$tot
Y = gwnu_L2$oz
head(gwnu_L2)

plot(X, Y)


#======================================
#     Satellite vs obs vs gwnu (boxplot)
#======================================
X = data_L4$iors_dsr
Y = data_L4$coms_dsr
Z = data_L4$gwnu_dsr

Xmean = mean(X)
Ymean = mean(Y)
Zmean = mean(Z)

summary(X)
summary(Y)
summary(Z)

ggplot() +
  theme_bw() +
  # geom_jitter(aes('IORS', X), alpha=0.05) +
  # geom_jitter(aes('GWNU', Y), alpha=0.05) +
  # geom_jitter(aes('COMS', Z), alpha=0.05) +
  geom_boxplot(aes('IORS', X), colour = '#F8877F', fill = '#F8877F', alpha = 0.2, width = 0.35) +
  geom_boxplot(aes('COMS', Y), colour = '#87B3FE', fill = '#87B3FE', alpha = 0.2, width = 0.35) +
  geom_boxplot(aes('GWNU', Z), colour = '#39C864', fill = '#39C864', alpha = 0.2, width = 0.35) +
  geom_point(aes('IORS', Xmean), colour = '#C57BFA', size = 4) +
  geom_point(aes('COMS', Ymean), colour = '#C57BFA', size = 4) +
  geom_point(aes('GWNU', Zmean), colour = '#C57BFA', size = 4) +
  annotate("text", x = 'COMS', y = 20.2340, label = "      Min.", size = 5.5, hjust = 0, vjust = 0.5, fontface = "bold", family = font) +
  annotate("text", x = 'COMS', y = 169.0000, label = "      25 %", size = 5.5, hjust = 0, vjust = 0.5, fontface = "bold", family = font) +
  annotate("text", x = 'COMS', y = 368.5000, label = "      50 %", size = 5.5, hjust = 0, vjust = 0.5, fontface = "bold", family = font) +
  annotate("text", x = 'COMS', y = 465.8417, label = "      Mean", size = 5.5, hjust = -0.4, vjust = 0.5, fontface = "bold", family = font, colour = '#C57BFA') +
  annotate("text", x = 'COMS', y = 594.6900, label = "      75 %", size = 5.5, hjust = 0, vjust = 0.5, fontface = "bold", family = font) +
  annotate("text", x = 'COMS', y = 1106.7800, label = "     Max.", size = 5.5, hjust = 0, vjust = 0.5, fontface = "bold", family = font) +
  scale_y_continuous(breaks = seq(0, 1200, by = 200), expand = c(0, 0), limits = c(0, 1200)) +
  labs(title = "") +
  labs(x = expression(paste(bold("")))) +
  labs(y = expression(paste(bold("Downward  Shortwave  Radiation  (DSR)  [Wm"^bold("-2") * "]")))) +
  labs(fill = NULL) +
  theme(plot.title = element_text(face = "bold", size = 20, color = "black")) +
  theme(axis.title.x = element_text(face = "bold", size = 19, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 19, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 19, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 19, colour = "black")) +
  theme(legend.title = element_text(face = "bold", size = 12, colour = "black")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 0.9)) +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 12, face = "bold")) +
  theme(legend.background = element_blank()) +
  theme(text = element_text(family = font)) +
  theme(plot.margin = unit(c(0, 8, 0, 0), "mm")) +
  ggsave(filename = paste0('Fig/Boxplot.tif'), device = "tiff", width = 7, height = 7, dpi = 600)
      
