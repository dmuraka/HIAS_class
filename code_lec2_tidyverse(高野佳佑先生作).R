# (※まだの方のみ)RとRstudioは以下ページの手順に従ってダウンロード ####
# https://posit.co/download/rstudio-desktop/

# install.packages(c("sf","mapview","modelsummary","tidyverse","magrittr")) # 初回のみ#を外して回してください
library(sf)
library(mapview)
library(modelsummary)
library(tidyverse)
library(magrittr)

#######################################################
####################################################### データの読込・整備
##### 出典：国土数値情報情報ダウンロードサービス（https://nlftp.mlit.go.jp/）
#stationの定義：https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N05-2023.html
#landの定義　 ：https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-L03-a-2021.html
#priceの定義　：https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-L01-2025.html

dstation <- sf::read_sf(dsn="https://www.dropbox.com/scl/fi/uqg0hzpqs6mya1hcgggcw/station_R3.geojson?rlkey=p0oixglii01xcgwuww3mcbw6x&dl=1")
dland    <- sf::read_sf(dsn="https://www.dropbox.com/scl/fi/eb9cbb9tt2g06wydqilnl/landuse_R3.geojson?rlkey=r4bh5aoemjf3lj5ur8df6crs5&dl=1")
dprice   <- sf::read_sf(dsn="https://www.dropbox.com/scl/fi/hllfqvyqna7eoj2y9rwem/landprice_R7.geojson?rlkey=c998opyiwbift11vmuls1f7tk&dl=1")

########## 列名の変更
dland <- dland %>%
  #列名を手動で変更
  magrittr::set_colnames(c("mesh","rice","agri","forest","wild","building","road","rail","other","river","beach","ocean","golf","out","geometry"))
names(dland)
dprice <- dprice %>%
  #メインで用いる列名を分かりやすいものに変更
  dplyr::rename(price=L01_008,status=L01_028)
names(dprice)

########## 地図を確認（dpriceから住宅地公示地価だけ抽出->dprice2）
###### mapview
mapview::mapview(dstation,cex=0.5)
#変数buildingで塗り分け
mapview::mapview(dland,zcol="building")
#変数priceで塗り分け
mapview::mapview(dprice,zcol="price")

###### sfのplot関数の場合
#x軸（経度）方向の描画範囲
xlim=c(139,140)
#y軸（緯度）方向の描画範囲
ylim=c(35.3,36)
#鉄道駅データを地図上に可視化
ggplot2::ggplot() +
  #用いるデータを指定
  ggplot2::geom_sf(data=dstation) +
  #描画範囲を指定
  ggplot2::coord_sf(xlim=xlim,ylim=ylim)
#土地利用データを地図上に可視化
ggplot2::ggplot() +
  #用いるデータを指定
  ggplot2::geom_sf(data=dland,aes(fill=building)) +
  #描画範囲を指定
  ggplot2::coord_sf(xlim=xlim,ylim=ylim)

###### 属性データ（dpriceから"住宅地"公示地価だけを抽出->dprice2）
dprice %>%
  #地価データから価格priceと利用の現況statusだけを抽出
  dplyr::select(price,status)
#利用の現況毎のレコード数を表示
dprice %>%
  #geometry属性を落とす
  sf::st_drop_geometry() %>%
  #利用の現況でグループ化
  dplyr::group_by(status) %>%
  #レコード数を集計
  dplyr::summarise(count=n())
dprice2 <- dprice %>%
  #利用の現況に「住宅」という文字列を含むレコードのみ残す
  dplyr::filter(grepl("住宅",status))
head(dprice2)
#抽出された住宅地地価ポイントの位置を可視化
ggplot2::ggplot() +
  ggplot2::geom_sf(data=dprice2) +
  ggplot2::coord_sf(xlim=xlim,ylim=ylim)

###### 余談：投影法の定義と変換
dprice2c <- dprice2 %>%
  #JGD2000に投影変換
  sf::st_transform(crs=sf::st_crs(4326))
dprice2c
dprice2c <- dprice2c %>%
  #平面直角座標第9系に投影変換
  sf::st_transform(crs=sf::st_crs(6677))
dprice2c

#######################################################
####################################################### データ処理

#################### 住宅地公示地価データに対する処理

###### 土地利用データの空間結合
#https://bakaniko.github.io/FOSS4G2019_Geoprocessing_with_R_workshop/geoprocessing-in-r.html
dprice3 <- dprice2 %>%
  sf::st_join(dland,join=st_within)

###### 最寄駅からの距離計算

dprice3 <- dprice3 %>%
  #最寄り駅までの距離を計算し変数として追加
  dplyr::mutate(st_dist=unlist(nngeo::st_nn(x=.,y=dstation,k=1,returnDist=TRUE)$dist))

# #「st_dist=」以降で関数がnestしまくっているのが嫌な人向けの書き方
# dprice3 <- dprice3 %>%
#   #最寄り駅までの距離を計算し変数として追加
#   #dprice3（パイプ演算子を用いているので「.」で表現）内の各ポイントから最寄りのdstation内のポイントを探索する
#   dplyr::mutate(st_dist=nngeo::st_nn(x=.,y=dstation,k=1,returnDist=TRUE) %>%
#                   #探索結果はlist形式で返ってくるので，最寄りポイントへの距離が入った要素distを抜いてくる
#                   purrr::chuck("dist") %>%
#                   #ベクトル形式に変換
#                   unlist())

# #mutate関数内部で行っている処理をステップ毎に知りたい場合に実行されたし
# #dprice3内の各ポイントから最寄りのdstation内のポイントを探索する
# nngeo::st_nn(x=dprice3,y=dstation,k=1,returnDist=TRUE) %>%
#   #探索結果はlist形式で返ってくるので，最寄りポイントへの距離が入った要素distを抜いてくる
#   purrr::chuck("dist") %>%
#   #ベクトル形式に変換
#   unlist()



###### 東京駅からの距離計算
dstation_tokyo <- dstation %>%
  #駅名が「東京」に一致するレコードを残す
  dplyr::filter(N05_011=="東京") %>%
  #複数レコードが該当するので，1レコード目だけ残す
  dplyr::slice(1)
dprice3 <- dprice3 %>%
  #東京駅までの距離を計算し変数として追加
  dplyr::mutate(tk_dist=unlist(nngeo::st_nn(x=.,y=dstation_tokyo,k=1,returnDist=TRUE)$dist))

#################### 土地利用グリッドに対する処理
###### 最寄駅からの距離計算
dland <- dland %>%
  #最寄り駅までの距離を計算し変数として追加
  dplyr::mutate(st_dist=unlist(nngeo::st_nn(x=sf::st_centroid(.),y=dstation,k=1,returnDist=TRUE)$dist)) %>%
  #東京駅までの距離を計算し変数として追加
  dplyr::mutate(tk_dist=unlist(nngeo::st_nn(x=sf::st_centroid(.),y=dstation_tokyo,k=1,returnDist=TRUE)$dist))

#######################################################
####################################################### 分析

###### データの性質の確認
dprice3 <- dprice3 %>%
  #いずれかの変数にNAを含むレコードを削除
  tidyr::drop_na()
#プロットするデータとx軸・y軸に取る変数を指定
ggplot2::ggplot(data=dprice3,aes(x=log(price),y=log(st_dist))) +
  #散布図をプロット
  ggplot2::geom_point()
#プロットするデータとx軸・y軸に取る変数を指定
ggplot2::ggplot(data=dprice3,aes(x=log(price),y=log(tk_dist))) +
  #散布図をプロット
  ggplot2::geom_point()

###### 回帰モデルの推定
mod <- lm(log(price)~log(st_dist)+log(tk_dist)+rice+agri+forest+wild+building+road+rail+river,data=dprice3)
modelsummary::modelsummary(mod,stars=TRUE)

###### 予測値（対数スケール）
dprice3 <- dprice3 %>%
  #モデルの予測値を変数として追加
  dplyr::mutate(log_pred=predict(mod))
#プロットするデータとx軸・y軸に取る変数を指定
ggplot2::ggplot(data=dprice3,aes(x=log(price),y=log_pred)) +
  #散布図をプロット
  ggplot2::geom_point() +
  #45度線をプロット
  ggplot2::geom_abline(intercept=0,slope=1,color="red")

#######################################################
####################################################### 土地利用グリッド毎の地価予測

###### 陸地を含むグリッドのみを選択 -> dland2
dland2 <- dland %>%
  #ocean（海洋）以外の土地利用面積がいずれか正であるレコードのみ残す
  dplyr::filter(dplyr::if_any(c(rice:beach,golf:out),~.>0))

###### 住宅地価の予測 -> dland2$pred
dland2 <- dland2 %>%
  #モデルから予測される住宅地価を変数として追加
  dplyr::mutate(log_pred0=predict(object=mod,newdata=.)) %>%
  #予測値は現状対数なので，指数を取って対数でない値に戻す
  dplyr::mutate(pred=exp(log_pred0))

###### 地図化
#とりあえず可視化
mapview::mapview(x=dland2,zcol="pred")
#色分けの閾値を指定
breaks <- c(0,100000,200000,300000,400000,500000,
            600000,800000,1200000,2000000,max(dland2$pred))
ggplot2::ggplot() +
  #プロットするデータと色分けの設定
  #メッシュをbreaksで定義した閾値に基づき色分け，枠線無し
  ggplot2::geom_sf(data=dland2,aes(fill=cut(x=pred,breaks=breaks)),color="transparent") +
  #Spectralというパレットを用いて色分け
  #デフォルトでは値が小→大になる時に赤→青と色が遷移するので，青→赤になるよう遷移の方向を反転させる
  ggplot2::scale_fill_brewer(palette="Spectral",direction=-1) +
  #凡例名を手動で設定
  ggplot2::guides(fill=ggplot2::guide_legend(title="Log Land Price")) +
  #描画範囲を手動で設定
  ggplot2::coord_sf(xlim=c(139.2,139.9),ylim=c(35.5,35.85))

## mapview関数を用いた地図化
mapview::mapview(dland2,
                 #予測された地価で色分け
                 zcol="pred",
                 #プロットするシンボルのサイズ
                 cex=4,
                 #シンボルの枠線の太さ(0は枠線なし)
                 lwd=0,
                 #色の区切り位置
                 at=breaks,
                 #透過度（1は透過なし）
                 alpha.regions=0.8,
                 #TRUEの場合，凡例を表示
                 ledgend=TRUE,
                 #凡例名を手動で設定
                 layer.name="Land price (JPY/m2)")


