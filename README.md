Erlang & Haskell版 N Queens 
===========================

作成者：河野 達也 / Tatsuya Kawano、CloudianKK

最終更新：2012年9月6日

目次
----

1. はじめに
2. ディレクトリ構成
3. 問い合わせ先について


1. はじめに
-----------

- 本書および関連するプログラムは、日経Linux 2012年10月号 特集1「Linuxで楽々アプリ開発超入門 − Part 4 Erlang & Haskell」の付録です。

- ウェブブラウザで以下のURLを開くと読みやすく整形された本書が表示されます。
  * https://github.com/tatsuya6502/nikkei-linux-fp/blob/master/README.md


2. ディレクトリ構成
-------------------

　このリポジトリには関数型言語のErlangやHaskellで書かれた「N Queensパズル」プログラムが収められています。ディレクトリ構成は以下の通りです。


```shell
nikkei-linux-fp/
    README.md            # このファイル
    erlang/
        *.erl            # 演習で使うErlangコード
        answer/          # 演習の解答
            *.erl
        dist_qu/
           README.md     # 並列プログラミング版 N Queens の解説
           *.erl
    haskell/
        *.hs             # 演習で使うHaskellコード
```

- N Queensプログラムの内容については、日経Linux 2012年10月号の特集1 「Linuxで楽々アプリ開発超入門 − Part 4 Erlang & Haskell」で解説されています。そちらを読みながら演習を進めてください。

- 「並列プログラミング版 N Queens の解説」（`erlang/dist_qu/README.md`）では、誌面で紹介できなかった並列プログラミング版N Queensについて、実行方法とコードの内容を解説しています。ウェブブラウザ向けに読みやすく整形された文書が以下のURLに用意されています。
  * https://github.com/tatsuya6502/nikkei-linux-fp/blob/master/erlang/dist_qu/README.md


3. お問い合わせについて
-----------------------

- プログラムの不具合を見つけられた場合には、issueトラッカーでご報告ください。
  * https://github.com/tatsuya6502/nikkei-linux-fp/issues

- 日経Linux 2012年10月号 特集1についてのご意見・お問い合わせは、日経Linux編集部までお願いします。

##
