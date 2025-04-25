
#### パッケージのインストール（初回のみ）
#install.packages( c("sf", "spdep","NipponMap", "RColorBrewer", "mapview") ) 

#### パッケージの読込
library( sf )
library( spdep )
library( NipponMap )
library( RColorBrewer )
library( mapview )

################################################
#### 都道府県人口データの読み込みと地図化 ######
################################################

shp      <- system.file("shapes/jpn.shp", package = "NipponMap")[1]
pref0    <- st_read(shp) 
pref     <- pref0[pref0$name != "Okinawa",]# 沖縄は今回は除外
st_crs(pref)<-4326                         # 座標参照系(crs)の指定（WGS84+地理座標系）
pref

#### plotによる地図化
breaks   <-c(0, 1000000, 2000000, 3000000, 5000000, 8000000, 
             max(pref$population))         # 色の区切り位置
pal      <-rev( brewer.pal(length(breaks)-1, "RdYlGn") )#カラーパレットを定義してrev関数で逆順に
plot(pref[, "population"],                 # population列を地図化
     pal=pal,                              # カラーパレット
     breaks=breaks,                        # 色の区切り位置
     main="Population by prefecture",      # 図のタイトル
     axes=TRUE, xlim=c(130,146),ylim=c(31,46),# 表示範囲と外枠でのXY座標の表示(axes=TRUE)
     key.pos = 1, key.length = 0.6 )       # 判例の位置(4は右)と長さ(1が図の幅いっぱい)

#### mapviewによる地図化（スライドなし）
mapview(pref[, "population"])# 座標系(crs)を指定しないと背景地図が表示されません

################################################
#### 隣接性（近さ）の定義 ######################
################################################

##### 最近隣4都道府県(の重心点)=隣接
coords  <- st_coordinates(st_centroid(pref))# 各都道府県の重心点の位置座標
knn     <- knearneigh(coords,4)             # 最近隣4都道府県の探索
nb      <- knn2nb(knn)                      # 最近隣4ゾーンのリスト（nb形式）
w       <- nb2listw(nb)                     # 隣接行列（listw形式）
plot(st_geometry(pref), border="grey")      # 都道府県の白地図
plot(nb, coords, add = TRUE, col="red",cex=0.01, lwd=1.5)# 隣接と判定された都道府県ペア

##### 境界または点を共有（Queen型）=隣接（スライドなし）
nb_q    <- poly2nb(pref, snap=0.05)         # ポリゴンの隣接ゾーンのリスト（nb形式）
w_q     <- nb2listw(nb)                     # 隣接行列（listw形式）
plot(st_geometry(pref), col="white", border="grey")     # 都道府県の白地図
plot(nb_q, coords, add=TRUE, col="red",cex=0.01,lwd=1.5)# 隣接と判定された都道府県ペア

################################################
#### モランI統計量による空間相関分析 ###########
################################################

pop     <- pref$population  # 都道府県別の人口

##### モランI統計量
moran	  <- moran.test(pop, listw=w)
moran

##### ローカルモランI統計量
lmoran	<- localmoran(pop, listw=w)
lmoran[1:3,]

## 地図化（ローカルモランI統計量）
pref$lmoran<- lmoran[, "Ii"]
breaks	  <-c( -5, -2, -1, -0.5, 0, 0.5, 1, 2, 5 )
nc   	  <- length(breaks)-1
pal 	  <- rev( brewer.pal(n = nc, name = "RdYlBu" ))
plot(pref[,"lmoran"], pal = pal, breaks=breaks, axes=TRUE)

## 地図化（統計的有意性）
pref$lmoran_p<- lmoran[,"Pr(z != E(Ii))"]
breaks	  <-c( 0, 0.01, 0.05, 0.10, 1 )
nc   	  <- length(breaks)-1
pal 	  <- rev( brewer.pal(n = nc, name = "YlOrRd" ))
plot(pref[,"lmoran_p"], pal = pal, breaks=breaks)

## モラン散布図
moran.plot(pop, listw=w, labels=pref$name, pch=20,
           xlim=c(-1000000,13000000),ylim=c(1000000,8000000))

## モラン散布図に基づく地域分類
str(lmoran)
moran_cl<- attr(lmoran,"quadr")$mean # 分類結果
pref$moran_cl<-moran_cl       # 分類結果をprefのmoran_cl列に追加

levels(moran_cl)              # カラーパレットはこの順で指定
plot(pref[,"moran_cl"], 
     axes=TRUE,
     pal = c("light blue",    # Low-Low
             "yellow",        # High-Low
             "dark green",    # Low-High
             "red"),          # High-High
     key.pos=1,
     key.length=0.5)

#################################################
#### ローカル・ギアリ統計量による空間相関分析####
#################################################

###### ローカル・ギアリ統計量
lgeary0 <- localC_perm(pop, listw=w)

###### 同統計量に基づく地域分類
geary_cl     <- attr(lgeary0,"cluster") # 地域分類結果
pref$geary_cl<- geary_cl   # 分類結果をprefのgeary_cl列に追加

levels(geary_cl)           # カラーパレットはこの順で指定
plot(pref[,"geary_cl"], 
     axes=TRUE,
     pal = c("red",        # High-High
             "light blue", # Low-Low
             "yellow",     # Other Positive
             "dark green"),# Negative
     key.pos=1,
     key.length=0.5)

######## 参考（スライドなし） ################
## 地図化（ローカル・ギアリ統計量）
lgeary  <- cbind(Ci=lgeary0,attr(lgeary0,"pseudo-p"))

pref$lgeary<- lgeary
breaks	<- c(0, 1, 2, 3, 5, 10)
nc   	  <- length(breaks)-1
pal 	  <- rev( brewer.pal(n = nc, name = "RdYlBu" ))
plot(pref[,"lgeary"], pal = pal, breaks=breaks,)

## 地図化（統計的有意性）
pref$lgeary_p<- lgeary[,"Pr(z != E(Ci))"]
breaks	<- c( 0, 0.01, 0.05, 0.10, 1 )
nc   	  <- length(breaks)-1
pal 	  <- rev( brewer.pal(n = nc, name = "YlOrRd" ))
plot(pref[,"lgeary_p"], pal = pal, breaks=breaks)
