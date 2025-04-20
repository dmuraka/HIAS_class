# (※まだの方のみ)RとRstudioは以下ページの手順に従ってダウンロード ####
# https://posit.co/download/rstudio-desktop/

# install.packages(c("sf","mapview","RColorBrewer","stringr","car")) # 初回のみ#を外して回してください
library(sf)
library(mapview)
library(RColorBrewer)
library(stringr)
library(car)

#######################################################
####################################################### データの読込・整備
##### 出典：国土数値情報情報ダウンロードサービス（https://nlftp.mlit.go.jp/）
#stationの定義：https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N05-2023.html
#landの定義　 ：https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-L03-a-2021.html
#priceの定義　：https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-L01-2025.html

dstation<-st_read(dsn="https://www.dropbox.com/scl/fi/uqg0hzpqs6mya1hcgggcw/station_R3.geojson?rlkey=p0oixglii01xcgwuww3mcbw6x&dl=1")
dland   <-st_read(dsn="https://www.dropbox.com/scl/fi/eb9cbb9tt2g06wydqilnl/landuse_R3.geojson?rlkey=r4bh5aoemjf3lj5ur8df6crs5&dl=1")
dprice  <-st_read(dsn="https://www.dropbox.com/scl/fi/hllfqvyqna7eoj2y9rwem/landprice_R7.geojson?rlkey=c998opyiwbift11vmuls1f7tk&dl=1")

########## 列名の変更
dland[1:3,]
names(dland)
names(dland)<-c("mesh","rice","agri","forest","wild","building","road","rail","other","river","beach","ocean","golf","out","geometry")
names(dland)

names(dprice)
names(dprice)[ names(dprice) =="L01_008" ]<-"price"
names(dprice)[ names(dprice) =="L01_028" ]<-"status"
names(dprice)

########## 地図を確認（dpriceから住宅地公示地価だけ抽出->dprice2）
###### mapview
mapview(dstation,cex=0.5)
mapview(dland[,"building"])
mapview(dprice[,"price"])

###### sfのplot関数の場合
xlim <- c(139,140)
ylim <- c(35.3,36)
plot(st_geometry(dstation),axes=TRUE,
     xlim=xlim,ylim=ylim,asp = NA)#asp=NAとするとアスペクト比が自由に
plot(dland[,"building"],axes=TRUE,xlim=xlim,ylim=ylim, asp=NA )
plot(dprice[,"price"],axes=TRUE,xlim=xlim,ylim=ylim, asp=NA )

###### 属性データ（dpriceから"住宅地"公示地価だけを抽出->dprice2）
dprice[1:10,c("price","status")]
table(dprice$status)
is_residual <- str_detect(dprice$status, "住宅")#部分一致
is_residual2<- is.element(dprice$status, "住宅")#完全一致
cbind(is_residual,is_residual2,dprice$status)[1:10,]
dprice2     <- dprice[is_residual,c("price","status")]
dprice2[1:10,]
plot(dprice2[,"price"],axes=TRUE,xlim=xlim,ylim=ylim, asp=NA )

dim(dprice)
dim(dprice2)

###### 余談：投影法の定義と変換
coords   <-st_coordinates(dprice2)
price    <-dprice2$price#or price[,"price"]
dprice2b <-data.frame(coords,price)
dprice2b[1:4,]
dprice2c <- st_as_sf(dprice2b, coords=c("X","Y"))
dprice2c[1:4,]
st_crs( dprice2c ) <- 4326
dprice2c[1:4,]
st_crs( dprice2c ) <- 6677
dprice2c[1:4,]

#######################################################
####################################################### データ処理

#################### 住宅地公示地価データに対する処理

###### 土地利用データの空間結合
#https://bakaniko.github.io/FOSS4G2019_Geoprocessing_with_R_workshop/geoprocessing-in-r.html
dprice3 <- st_join(dprice2, dland, join = st_within)

###### 最寄駅からの距離計算
dmat            <- st_distance(dprice2, dstation)
dprice3$st_dist <- apply(dmat, 1, min)

###### 東京駅からの距離計算
dstation_tokyo  <- dstation[dstation$N05_011=="東京",][1,]
dmat_t          <- st_distance(dprice2, dstation_tokyo)
dprice3$tk_dist <- dmat_t

#################### 土地利用グリッドに対する処理
###### 最寄駅からの距離計算
dmat            <- st_distance(st_centroid(dland), dstation)
dland$st_dist   <- apply(dmat, 1, min)

###### 東京駅からの距離計算
dmat_t  <- st_distance(st_centroid(dland), dstation_tokyo)
dland$tk_dist <- apply(dmat_t, 1, min)


#######################################################
####################################################### 分析

###### データの性質の確認
summary(dprice3)
dprice3 <- na.omit(dprice3)  # 欠損を含む行の除去
price   <- dprice3$price     # 住宅地公示地価
st_dist <- dprice3$st_dist   # 最寄駅までの距離
tk_dist <- dprice3$tk_dist   # 東京駅までの距離

plot(log(price),log(st_dist))# 住宅地価と最寄駅距離の2次元プロット
plot(log(price),log(tk_dist))# 住宅地価と東京駅までの距離の2次元プロット

###### 回帰モデルの推定
mod     <- lm(log(price)~log(st_dist)+log(tk_dist)+rice+agri+forest+wild+building+road+rail+river,data=dprice3)
summary(mod)# 結果

###### 予測値（対数スケール）
log_pred<- predict(mod)          # 予測値
plot(log(dprice3$price),log_pred)# 対数スケールでの実測値と予測値の比較
abline(0,1,col="red")            # 45度線プロットを引く

#######################################################
####################################################### 土地利用グリッド毎の地価予測

###### 陸地を含むグリッドのみを選択 -> dland2
land_dat1<-st_drop_geometry(dland[,c(2:14)]) # 土地利用に関する変数の列だけ取り出してgeometryを除去（sf->data.frame）
land_dat2<-rowSums(land_dat1[,-11])          # 海(ocean)以外の土地利用面積
dland2  <-dland[land_dat2>0,]                # 海(ocean)以外の土地利用が0以上のグリッド

###### 住宅地価の予測 -> dland2$pred
log_pred0      <- predict(mod,newdata=dland2)
dland2$pred    <- exp(log_pred0)

###### 地図化
mapview(dland2[,"pred"])

## 色分けの設定
quantile(dland2$pred,prob=seq(0,1,0.1))
breaks   <-c(0,100000,200000,300000,400000,500000,
             600000,800000,1200000,2000000,max(dland2$pred))
display.brewer.all()
pal0     <- brewer.pal(length(breaks)-1, "RdYlGn")
pal      <- rev( pal0 )

## plot関数を用いた地図化
plot(dland2[,"pred"],  # 住宅地価をプロット
     axes = TRUE,      # XY座標値を表示
     cex.axis = 0.8,   # XY座標値のフォントサイズ
     pal = pal,        # カラーパレット
     breaks = breaks,  # 色の区切り位置
     pch = 20,         # シンボル(例：20は円、15は四角)
     cex = 0.8,        # シンボルのサイズ(デフォルトは1)
     key.pos = 4,      # 凡例の位置（4は図の右）
     key.length = 0.8, # 判例の長さ（1=図の幅）
     xlim = c(139.2, 139.9), ylim = c(35.5, 35.85),  # 表示範囲
     border=NA,        # 枠線を消す
     main = "Land price (JPY/m2)")     

## mapview関数を用いた地図化
mapview(dland2[,"pred"],  # 住宅地価をプロット
        cex=4,             # プロットするシンボルのサイズ 
        lwd=0,             # シンボルの枠線の太さ(0は枠線なし)
        at = breaks,       # 色の区切り位置
        alpha.regions=0.8, # 透過度（1は透過なし）
        ledgend=TRUE,      # TRUEの場合、凡例を表示
        layer.name="Land price (JPY/m2)") # 


