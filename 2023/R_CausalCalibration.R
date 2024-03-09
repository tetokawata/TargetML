
# devtools::install_github("larsvanderlaan/causalcalibration")

pacman::p_load(
  causalCalibration,
  grf,
  tidyverse,
  recipes
)


Raw <- arrow::read_parquet("~/Dropbox/DataClean/RetailJPN/AllPrefectture.parquet") |> 
  filter(今後の利用目的 == "住宅") |> 
  filter(用途 == "住宅") |> 
  filter(改装 != "") |> 
  filter(間取り != "") |> 
  filter(都市計画 != "") |> 
  filter(最寄駅.名称 != "") |> 
  filter(都道府県名 == "東京都") |> 
  filter(str_detect(取引時点,"４")) |> 
  filter(str_detect(市区町村名, "区")) |> 
  filter(
    建物の構造 == "ＳＲＣ" |
      建物の構造 == "ＲＣ"
  ) |> 
  filter(
    間取り != "オープンフロア"
  ) |> 
  filter(
    間取り != "スタジオ"
  ) |> 
  filter(
    間取り != "メゾネット"
  ) |> 
  mutate(TradePeriod = str_sub(取引時点,1,4) |> 
           as.numeric()
  ) |> 
  filter(TradePeriod == 2016 |
           TradePeriod == 2021) |> 
  mutate(
    TempBuildYear = 
      str_sub(建築年,3,-2) |> 
      as.numeric(),
    TempGengo = str_sub(建築年,1,2)
  ) |> 
  mutate(
    BuildYear = if_else(
      TempGengo == "令和",
      TempBuildYear + 2018,
      TempBuildYear
    )
  ) |> 
  mutate(
    BuildYear = if_else(
      TempGengo == "平成",
      TempBuildYear + 1988,
      BuildYear
    )
  ) |> 
  mutate(
    BuildYear = if_else(
      TempGengo == "昭和",
      TempBuildYear + 1925,
      BuildYear
    )
  ) |> 
  mutate(
    Price = 取引価格.総額./(10^6),
    Tenure = TradePeriod - BuildYear,
    DistanceStation = 最寄駅.距離.分. |> as.numeric(),
    Size = 面積... |> as.numeric(),
    Youseki = 容積率... |> as.numeric(),
    Kenpei = 建ぺい率... |> as.numeric(),
    RoomNum = if_else(str_detect(間取り,"１"),1,0),
    RoomNum = if_else(str_detect(間取り,"２"),2,RoomNum),
    RoomNum = if_else(str_detect(間取り,"３"),3,RoomNum),
    RoomNum = if_else(str_detect(間取り,"４"),4,RoomNum),
    RoomNum = if_else(str_detect(間取り,"５"),5,RoomNum),
    RoomNum = if_else(str_detect(間取り,"６"),6,RoomNum),
    ZoneHouse = if_else(str_detect(都市計画,"住居"),1,0),
    ZoneBusiness = if_else(str_detect(都市計画,"商業"),1,0),
    ZoneFactory = if_else(str_detect(都市計画,"工業"),1,0),
    RoomK = if_else(str_detect(間取り,"Ｋ"),1,0),
    RoomL = if_else(str_detect(間取り,"Ｌ"),1,0),
    RoomD = if_else(str_detect(間取り,"Ｄ"),1,0),
    RoomS = if_else(str_detect(間取り,"Ｓ"),1,0),
    StructureSRC = if_else(建物の構造 == "ＳＲＣ",1,0),
    After = if_else(
      TradePeriod == 2021,
      1,
      0
    ),
    Reform = if_else(改装 == "改装済",1,0)
  ) |> 
  mutate(
    District = if_else(
      市区町村名 == "中央区" |
        市区町村名 == "港区" |
        市区町村名 == "千代田区",
      "CBD",
      市区町村名)
  ) |> 
  select(Price:District) |> 
  na.omit()

Data <- recipe(
  ~.,
  Raw
) |> 
  step_scale(
    Tenure,
    DistanceStation,
    Size,
    Youseki,
    Kenpei,
    RoomNum
  ) |> 
  step_dummy(District) |> 
  prep() |> 
  bake(new_data = NULL)


X <- Data |> 
  select(
    -Price,
    -After
  )

Y <- Data$Price

D <- Data$After

Fit <- causal_forest(
  X = X,
  Y = Y,
  W = D
  )


causalCalibrate(
  tau = Fit$predictions[,1],
  Y = Y,
  A = D,
  
)
