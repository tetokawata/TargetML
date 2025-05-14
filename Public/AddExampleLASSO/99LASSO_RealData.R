set.seed(1)

MinGroup <- 10

library(tidyverse)
library(recipes)

R2 <- function(pred, y) {
  score <- 1 - mean((pred - y)^2) / var(y)
  return(score)
}

nanoparquet::read_parquet_schema(
  "Data/Tokyo_20191_20243.parquet"
) |>
  magrittr::extract2("name")

Raw <- nanoparquet::read_parquet(
  "Data/Tokyo_20191_20243.parquet"
)

Raw$今後の利用目的 |> table(useNA = "always")
Raw$建物の構造 |> table(useNA = "always")

Raw <- nanoparquet::read_parquet(
  "Data/Tokyo_20191_20243.parquet",
  col_select = c(
    "価格情報区分",
    "取引時期",
    "市区町村名",
    "取引価格（総額）",
    "建築年",
    "最寄駅：距離（分）",
    "面積",
    "都市計画",
    "地区名",
    "間取り",
    "容積率",
    "建ぺい率",
    "改装"
  )
) |>
  filter(
    価格情報区分 == "不動産取引価格情報",
    str_detect(取引時期, "第1四半期"),
    str_detect(市区町村名, "区"),
    !str_detect(間取り, "オープンフロア"),
    !str_detect(間取り, "メゾネット"),
    !str_detect(間取り, "スタジオ")
  ) |>
  mutate(
    TradeYear = str_sub(取引時期, 1, 4) |> as.numeric(),
    Price = `取引価格（総額）` / 1000000,
    Size = 面積,
    SmallArea = 地区名,
    District = 市区町村名,
    BuildYear = str_sub(建築年, 1, 4) |> as.numeric(),
    Tenure = TradeYear - BuildYear,
    Distance = `最寄駅：距離（分）` |> as.numeric(),
    Zone = 都市計画,
    Room = 間取り,
    RoomNumber = case_when(
      str_detect(Room, "１") ~ 1,
      str_detect(Room, "２") ~ 2,
      str_detect(Room, "３") ~ 3,
      str_detect(Room, "４") ~ 4,
      str_detect(Room, "５") ~ 5,
      str_detect(Room, "６") ~ 6
    ),
    RoomK = if_else(
      str_detect(Room, "Ｋ"), 1, 0
    ),
    RoomD = if_else(
      str_detect(Room, "Ｄ"), 1, 0
    ),
    RoomL = if_else(
      str_detect(Room, "Ｌ"), 1, 0
    ),
    Kenpei = 建ぺい率,
    Youseki = 容積率,
    Reform = if_else(改装 == "改装済み", 1, 0)
  ) |>
  select(TradeYear:Reform) |>
  na.omit() |>
  filter(TradeYear %in% c(2024))

Raw |>
  nanoparquet::write_parquet("Public/AddExampleLASSO/data.pq")

Y <- Raw$Price

X <- Raw |>
  recipe(
    ~ Size + SmallArea + District + Tenure + Distance + Zone +
      RoomNumber + RoomK + RoomD + RoomL + Kenpei + Youseki +
      Reform
  ) |>
  step_other(
    all_nominal_predictors(),
    threshold = MinGroup
  ) |>
  step_interact(
    ~ all_numeric_predictors():all_numeric_predictors()
  ) |>
  step_dummy(
    all_nominal_predictors(),
    one_hot = TRUE
  ) |>
  step_interact(
    ~ starts_with("District"):Size +
      starts_with("District"):Distance +
      starts_with("District"):Tenure +
      starts_with("District"):RoomNumber +
      starts_with("District"):RoomK +
      starts_with("District"):RoomD +
      starts_with("District"):RoomL +
      starts_with("District"):Kenpei +
      starts_with("District"):Youseki +
      starts_with("District"):Reform +
      starts_with("Zone"):Size +
      starts_with("Zone"):Distance +
      starts_with("Zone"):Tenure +
      starts_with("Zone"):RoomNumber +
      starts_with("Zone"):RoomK +
      starts_with("Zone"):RoomD +
      starts_with("Zone"):RoomL +
      starts_with("Zone"):Kenpei +
      starts_with("Zone"):Youseki +
      starts_with("Zone"):Reform +
      starts_with("SmallArea"):Size +
      starts_with("SmallArea"):Distance +
      starts_with("SmallArea"):Tenure +
      starts_with("SmallArea"):RoomNumber +
      starts_with("SmallArea"):RoomK +
      starts_with("SmallArea"):RoomD +
      starts_with("SmallArea"):RoomL +
      starts_with("SmallArea"):Kenpei +
      starts_with("SmallArea"):Youseki +
      starts_with("SmallArea"):Reform +
      starts_with("SmallArea"):starts_with("Zone") +
      starts_with("District"):starts_with("Zone")
  ) |>
  step_poly(
    Size,
    Distance,
    Tenure,
    RoomNumber,
    Kenpei,
    Youseki,
    degree = 2
  ) |>
  step_zv(
    all_numeric_predictors()
  ) |>
  step_normalize(
    all_numeric_predictors()
  ) |>
  prep() |>
  bake(
    new_data = NULL
  )

X_Simple <- Raw |>
  recipe(
    ~ Size + SmallArea + District + Tenure + Distance + Zone +
      RoomNumber + RoomK + RoomD + RoomL + Kenpei + Youseki +
      Reform
  ) |>
  step_other(
    all_nominal_predictors(),
    threshold = MinGroup
  ) |>
  step_dummy(
    all_nominal_predictors(),
    one_hot = TRUE
  ) |>
  step_zv(
    all_numeric_predictors()
  ) |>
  step_normalize(
    all_numeric_predictors()
  ) |>
  prep() |>
  bake(
    new_data = NULL
  )


Group <- sample(
  1:2,
  nrow(X),
  replace = TRUE,
  prob = c(0.8, 0.2)
)


Model <- gamlr::gamlr(
  x = X[Group == 1, ],
  y = Y[Group == 1]
)

coef <- tibble(
  coef = coef(Model)[-1] |> as.numeric(),
  name = names(X),
  score = abs(coef)
)

(Model |>
  coef() != 0) |>
  as.numeric() |>
  table()

gamlr::gamlr(
  x = X[Group == 1, ],
  y = Y[Group == 1]
) |>
  predict(X[Group == 2, ]) |>
  as.numeric() |>
  R2(Y[Group == 2])

options(warn = -1)

lm(Y ~ .,
  X,
  subset = Group == 1
) |>
  predict(X[Group == 2, ]) |>
  R2(Y[Group == 2])


lm(Y ~ .,
  X,
  subset = Group == 1
) |>
  predict(X[Group == 1, ]) |>
  R2(Y[Group == 1])

lm(Y ~ .,
  X_Simple,
  subset = Group == 1
) |>
  predict(X_Simple[Group == 2, ]) |>
  R2(Y[Group == 2])
