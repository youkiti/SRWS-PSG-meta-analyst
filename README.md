# Shiny Meta-Analysis

メタ分析を実行するためのShinyアプリケーション。OpenMeta-Analystのアップデートされた実装です。

## 機能

- 二値データのメタ分析
  - オッズ比
  - リスク比
  - リスク差
- 連続データのメタ分析
  - 平均差
  - 標準化平均差
- メタ回帰分析
  - 連続変数による修飾効果
  - カテゴリ変数による修飾効果

## インストール方法

1. Rをインストール（バージョン4.0以上推奨）
2. RStudioをインストール
3. このリポジトリをクローン
4. RStudioでプロジェクトを開く
5. `setup.R`を実行して必要なパッケージをインストール

```r
source("setup.R")
```

## 使用方法

RStudioで以下のコマンドを実行：

```r
library(shiny)
runApp()
```

または、R GUIで：

```r
setwd("path/to/shiny-meta")  # アプリケーションのディレクトリに移動
source("app.R")
```

## サンプルデータ

`sample_data/`ディレクトリに以下のサンプルデータファイルが用意されています：

- `binary.csv`: 二値データのメタ分析用
- `continuous.csv`: 連続データのメタ分析用
- `meta_reg.csv`: メタ回帰分析用

## 開発環境

- R 4.3.2 ("Eye Holes")
- RStudio または VSCode
- 必要なパッケージ:
  - shiny
  - shinydashboard
  - shinyjs
  - DT
  - metafor (4.6.0)
  - meta (8.0.1)
  - dmetar
  - tidyverse
  - Matrix
  - numDeriv

## 起動方法

### RStudioの場合
```r
library(shiny)
runApp()
```

### コマンドラインの場合
```r
& 'C:/Program Files/R/R-4.3.2/bin/R.exe' --vanilla -e "source('SRWS-PSG-meta-analyst/global.R'); library(shiny); runApp('SRWS-PSG-meta-analyst')"
```

## ライセンス

このプロジェクトはGPLv3ライセンスの下で公開されています。

## 謝辞

このプロジェクトは[OpenMeta-Analyst](https://github.com/bwallace/OpenMeta-analyst-)をベースに開発されました。また、実装の一部は[Doing Meta-Analysis with R: A Hands-On Guide](https://github.com/MathiasHarrer/Doing-Meta-Analysis-in-R)を参考にしています。
