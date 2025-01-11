# Shiny Meta-Analysis

メタ分析を実行するためのShinyアプリケーション。OpenMeta-Analystの現代的な実装です。

## 機能

- 二値データのメタ分析
  - オッズ比
  - リスク比
  - リスク差
- 連続データのメタ分析
  - 平均差
  - 標準化平均差
- 診断精度研究のメタ分析
  - 感度・特異度
  - 診断オッズ比
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
- `diagnostic.csv`: 診断精度研究のメタ分析用
- `meta_reg.csv`: メタ回帰分析用

## 開発環境

- R 4.4.1
- RStudio 2023.12.0
- Shiny 1.7.5
- metafor 3.8
- meta 7.0

## ライセンス

このプロジェクトはGPLv3ライセンスの下で公開されています。

## 謝辞

このプロジェクトは[OpenMeta-Analyst](https://github.com/bwallace/OpenMeta-analyst-)をベースに開発されました。
