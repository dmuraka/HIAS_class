
##################### load packages
# install.packages(c("spatialreg","spdep")) 
library(spatialreg)
library(spdep)

##################### データの読み込み
d        <-read.csv("https://www.dropbox.com/scl/fi/sxo52u5bbgojof4pncffs/larceny_SanFran2019.csv?rlkey=8nbwccol6u3o7qtddhkbk4oav&dl=1")
poly     <-st_read(dsn="https://www.dropbox.com/scl/fi/v19zceex1hdod0mvpsqo2/sf_polygon.geojson?rlkey=dbrjoii0lo5k17ki2tgvfx8qx&dl=1")
### 参考: 通常の読込の場合
#d        <-read.csv("larceny_SanFran2019.csv")
#poly     <-st_read("sf_polygon.geojson")

### 4近傍に基づく空間重み行列
coords    <-d[,c("px","py")]                  # 街区の重心点の緯度経度
W_nn_nb   <-knn2nb( knearneigh(coords, k=4) ) # 空間重み行列（前チャプターと同様4近傍）
listw     <-nb2listw(W_nn_nb)

### 参考: ポリゴンの隣接に基づく空間重み行列(クイーン型)
#W_poly_nb <-poly2nb( poly )
#listw     <-nb2listw(W_poly_nb )

# 回帰式の定義
formula   <-log(y) ~ log(y0) + pop_thou+age_med+pov_rat+race_div# 

##################### 線形回帰モデルの推定
mod0      <-lm(formula,data=d)
summary(mod0)

lm.morantest(mod0,listw=listw) # 残差の空間相関の検定

##################### 空間計量経済モデルの推定
###### 空間エラーモデル
sem    <-errorsarlm(formula,data=d, listw=listw, tol.solve=1.0e-20)
summary(sem)

###### 空間ラグモデル
slm    <-lagsarlm(formula,data=d, listw=listw, tol.solve=1.0e-20)
summary(slm)

###### 空間ダービンモデル
sdm    <-lagsarlm(formula,data=d, listw=listw, tol.solve=1.0e-20,
                  type="mixed")
summary(sdm)

###### Akaike Information Criterion(AIC: 精度指標で小さいほど良い)の比較
AIC(sem)
AIC(slm)
AIC(sdm)

##################### 直接・間接効果の推定（有意性の評価なし）
impacts(slm,listw=listw) # 空間ラグモデル
impacts(sdm,listw=listw) # 空間ダービンモデル

##################### 直接・間接効果の推定（有意性の評価あり）
listw2  <- as(listw, "CsparseMatrix")    # 高速化のため、疎行列に変換
trMat   <- trW(listw2, type="mult")
ires_sdm<-impacts(sdm, tr=trMat, R=1000) # 空間ダービンモデル
summary(ires_sdm)

