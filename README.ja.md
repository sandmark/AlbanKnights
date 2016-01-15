AlbanKnights
============

MMORPG『マビノギ』のベルテン特別班用に作ったユーティリティプログラムです。使用言語は Haskell 、ライセンスは BSD3 を採用しています。

* [マビノギ公式サイト](http://mabinogi.nexon.co.jp/)
* [私の騎士団](http://mabinogi.wikiwiki.jp/?%A5%AF%A5%A8%A5%B9%A5%C8%2F%C6%C3%CA%CC%C8%C9)

See also [README in English](README.md).

## 概要
ベルテン特別班に所属する NPC が好むキーワードを自動計算して表示します。[会話テーブル](http://mabinogi.wikiwiki.jp/?%BB%E4%A4%CE%B5%B3%BB%CE%C3%C4%B2%F1%CF%C3%A5%C6%A1%BC%A5%D6%A5%EB)があるとはいえ、会話のたびにテーブルとにらめっこする作業を自動化できないかと思ったのがきっかけです。

多忙を極めるシュアンさんを助けるためにも……と思いましたが、よく考えたらベルテンの下級印章をプレゼントする必要がなくなるので、やっぱり自腹でなんとかしてもらうしかなさそうです。

Haskell で実装したことに特に理由はありません。単に理解を深めたかったことと、ビルドパッケージが実行可能バイナリなので配布に都合が良かっただけです（事実、 Haskell プロジェクトをまともに書いて公開するのはこれが初なので）。ダウンロードしてダブルクリックするだけで実行できるため、他の何かをインストールする必要はありません。使わなくなったらそっとゴミ箱へどうぞ。

## 機能
* 昔ながらの CUI プログラムです
* ダイ、アイリース、カオル、エルシィの好感度番号を保存します
* `set`, `unset`, `hold` コマンドで好感度番号を設定します
* `list` コマンドで NPC の好むキーワード 3 つずつを一覧表示します
* `next` コマンドで次のキーワードテーブルへ移行します
* `suggest` コマンドで次のキーワードを予測します

## 実装予定
- [X] 現在のキーワードテーブル位置を予測
- [x] 特殊キーワード 98, 99 番のサポート
- [ ] ハードコーディングされた日本語リテラルをどうにかする

## 使い方
起動すると以下のようなプロンプトが表示されます。

`AlbanKnights(1): `

ここに各種コマンドを入力して操作していきます。

### コマンド
* `npc [index]` 好感度の上がるキーワードを 3 つ表示
* `set npc index` プログラムに NPC の好感度番号をセット
* `unset npc` セットされた好感度番号をクリア
* `list` 好感度番号がセットされた NPC のキーワードを 3 つずつすべて表示
* `next` リストの好感度番号をすべて +3 する（`hold`されたものは除く）
* `hold npc` NPC の好感度番号を固定し、 `next` で変更されないようにする
* `unhold npc` 好感度番号の固定状態を解除
* `stock npc keyword1 keyword2 ...` NPCに好感触だったキーワードを保持。一覧表示は`stock`
* `suggest npc [keywords]` キーワード候補を表示。`[keywords]`部分は`stock`を使用している場合省略可能
* `exit` 好感度番号を表示し、プログラムを終了する

`npc` となっている部分にはそれぞれ `dai`, `eirlys`, `kaour`, `elsie` と入力してください。

### エイリアス
`npc` には別名が定義されています。スペルを覚えるのが面倒な人（主に作者）向けに、ローマ字や省略形、日本語が用意されています。

NPC名   | ローマ字   | 省略形 | カナ
-------- | --------- | ------ | ------
`dai`    |           | `d`    | `ダイ`
`eirlys` | `airi-su` | `a`    | `アイリース`
`kaour`  | `kaoru`   | `k`    | `カオル`
`elsie`  | `erusii`  | `e`    | `エルシィ`

また、コマンドにも別名が定義されています。

コマンド   | 省略形 | 別名     | 別名    | 別名     | 別名
--------- | ------ | -------- | ------ | -------- | ----
`set`     | `s`
`unset`   | `u`
`list`    | `l`    | `ls`
`next`    | `x`    | `update` | `up`
`hold`    | `h`    | `unhold` | `lock` | `unlock` | `toggle`
`stock`   | `st`
`suggest` | `sug`
`exit`    | `q`    | `quit`   | `:q`

例えば `set eirlys 27` は `s a 27` と入力しても同じ意味になります。

### 作業の流れ
1. `set` コマンドで NPC のキーワード番号をセットします。
1. `ls` と入力してキーワードを確かめます。
1. **ゲーム内で** NPC に話しかけ、表示されたキーワードで会話します。
1. その後 `x`(`next`) コマンドでキーワード番号を更新します。イベント会話が発生した場合、 `h`(`hold`) コマンドで一旦固定しておくといいでしょう。
1. プログラムを終了する場合は `q`(`quit`) と入力します。
1. 2 へ戻って以下ループ。

## インストール
**[最新版をダウンロード](https://github.com/sandmark/AlbanKnights/releases/latest)**

現在 Windows しかサポートしていません。 32bit, 64bit ともに動作するはずです。

## Contribution
1. Fork it (http://github.com/sandmark/AlbanKnights/fork)
2. Create your feature branch (git checkout -b my-new-feature)
3. Commit your changes (git commit -am 'Add some feature')
4. Push to the branch (git push origin my-new-feature)
5. Create new Pull Request

## Author
[sandmark](https://github.com/sandmark)

## License
[BSD3](LICENSE)

## Special Thanks
[Mabinogi Wiki](http://mabinogi.wikiwiki.jp/)(ja)
