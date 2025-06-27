
##################### パッケージのインストール(初回のみ)
#install.packages("GWmodel") 
#install.packages("sf")     
#install.packages("mapview")

##################### パッケージの読込(毎回)
library(GWmodel);library(sf);library(mapview)

############## 住宅地公示地価データ #################
d<-read.csv("https://www.dropbox.com/scl/fi/2eml268xr38btb0tjl298/price_6cities.csv?rlkey=3s2kuj9tobumotpf22l7hg8xf&dl=1",fileEncoding="Shift-JIS")
d[1:4,]                  # 最初の4行
d$tdist_k <-d$tdist/1000 # m単位のtdist(最寄駅距離)をkm単位へ
d$bdist_k <-d$bdist/1000 # m単位のbdist(バス停駅距離)をkm単位へ
d$tuser_k <-d$tuser/1000 # 人単位のtuser(駅利用者数)を千人単位へ

#### data.frame形式からsf形式に変換
d_s       <- st_as_sf(d,coords=c("X","Y")) 
st_crs(d_s)<-6677        # 今回のデータの座標系は平面直角座標系第9系
                         # EPSGは6677(3回目の講義資料参照)
                         # コード入力が必要なのは、mapview関数で、
                         # 背景地図との重ね合わせを行うため

##################### GWRの推定 #####################
#### 引数
formula <- log(price)~tdist_k+tuser_k+bdist_k# 回帰式
kernel  <- "gaussian"    # ガウスカーネルを用いる
adaptive<- FALSE         # 適応型ではないバンド幅

#### 推定
bw      <-bw.gwr(formula,adaptive=adaptive, data=d_s, kernel = kernel)#バンド幅の推定
mod1    <-gwr.basic(formula, data=d_s, bw=bw,adaptive=adaptive,kernel = kernel)#GWRモデル推定
mod1
summary(mod1$SDF)        # 回帰係数等の要約統計量
                         # 説明変数名:回帰係数、説明変数名_SE:標準誤差、説明変数名_TV:t値

#### 回帰係数の空間分布の確認
# sfパッケージの場合（とりあえずプロットするならこちら）
plot(mod1$SDF[,"tdist_k"],pch=20,axes=TRUE)

# mapviewパッケージの場合（背景地図も追加したい場合はこちら）
mapview(mod1$SDF[,"Intercept"], legend = TRUE)
mapview(mod1$SDF[,"tdist_k"], legend = TRUE)
mapview(mod1$SDF[,"tuser_k"], legend = TRUE)
mapview(mod1$SDF[,"bdist_k"], legend = TRUE)

##################### MGWRの推定 #####################
mod2    <-gwr.multiscale(formula, data=d_s, bws0=rep(bw,4),
                         adaptive=adaptive,kernel = kernel)
mod2
summary(mod2$SDF)# 回帰係数等の要約統計量
                 # 説明変数名:回帰係数、説明変数名_SE:標準誤差、説明変数名_TV:t値

#### 回帰係数の空間分布
mapview(mod2$SDF[,"Intercept"], legend = TRUE)
mapview(mod2$SDF[,"tdist_k"], legend = TRUE)
mapview(mod2$SDF[,"tuser_k"], legend = TRUE)
mapview(mod2$SDF[,"bdist_k"], legend = TRUE)

#### |t値|>1.96を満たす回帰係数だけプロットする場合
mapview(bdat2_s[abs(bdat2[,"tdist_k_TV"])>1.96,"tdist_k"], legend = TRUE)
mapview(bdat2_s[abs(bdat2[,"tuser_k_TV"])>1.96,"tuser_k"], legend = TRUE)
mapview(bdat2_s[abs(bdat2[,"bdist_k_TV"])>1.96,"bdist_k"], legend = TRUE)

##################### GWRによる空間予測 #####################
d0         <- read.csv("https://www.dropbox.com/scl/fi/s6p2at0ju1fz672a14w3i/grid250m_6cities.csv?rlkey=pflwjf5vk64aw8xabejrll5d8&dl=1",fileEncoding="Shift-JIS")
d0[1:4,]                    # 250mグリッド毎の予測地点データの最初4行
d0$tdist_k <- d0$tdist/1000 # m単位のtdist(最寄駅距離)をkm単位へ
d0$bdist_k <- d0$bdist/1000 # m単位のbdist(バス停駅距離)をkm単位へ
d0$tuser_k <- d0$tuser/1000 # 人単位のtuser(駅利用者数)を千人単位へ

#### data.frame形式からsf形式に変換
d0_s       <- st_as_sf(d0,coords=c("X","Y")) 
st_crs(d0_s)<-6677         # 今回のデータの座標系は平面直角座標系第9系

#### 空間予測の実行
mod1p      <- gwr.predict(formula, data=d_s, predictdata=d0_s, bw=bw,
                      adaptive=adaptive,kernel = kernel)
mod1p$SDF[1:4,]            # 最初4行

#### 住宅地価の予測値の空間分布
mapview(mod1p$SDF[,"prediction"], legend = TRUE, lwd=0,cex=5)

#### 回帰係数の予測値の空間分布
mapview(mod1p$SDF[,"Intercept_coef"], legend = TRUE, lwd=0,cex=6,pch=15)
mapview(mod1p$SDF[,"tdist_k_coef"], legend = TRUE, lwd=0,cex=6,pch=15)
mapview(mod1p$SDF[,"tuser_k_coef"], legend = TRUE, lwd=0,cex=6,pch=15)
mapview(mod1p$SDF[,"bdist_k_coef"], legend = TRUE, lwd=0,cex=6,pch=15)
