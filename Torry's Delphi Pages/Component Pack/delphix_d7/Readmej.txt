			==============================
				DelphiX 2000.07.17
			==============================

１．初めに

　これは DirectX を Delphi で簡単に使えるようにするコンポーネント集です。
これは DirectX SDK が無くても動作します。


２．DelphiX を利用するにあたって

　利用するには下の３つが必要になります

　・Borland Delphi 4, 5 あるいは 3 
　・DirectX 7 ランタイム


３．使用している DirectX オブジェクト

 DirectDraw
 Direct3D
 DirectSound
 DirectSoundCapture
 DirectInput(GetJoyPosEx 関数も可)
 DirectPlay

４．提供されるコンポーネント

 TDXDraw		DirectDraw, Direct3D コンポーネント。
 TDXDIB			DIB を保持するコンポーネント。
 TDXImageList		TPicture のリスト。サーフェースの管理もします。
 TDX3D			Direct3D コンポーネント(TDXDraw と連携しながら動作します)
 TDXSound		DirectSound コンポーネント。
 TDXWave		Wave を保持するコンポーネント。
 TDXWaveList		Wave のリスト。バッファの管理もします。
 TDXInput		入力コンポーネント(キーボード、ジョイステイック)。
 TDXPlay		通信コンポーネント
 TDXSpriteEngine	スプライトエンジン。
 TDXTimer		TTimer よりも高速なタイマー。
 TDXPaintBox		TPaintBox の DIB 版。

 TDXForm		フォームを DelphiX 用に最適化した物です。

５．インストール

　Bin フォルダの Install_for?.exe を実行して下さい。コンパイルと登録を行います。


６．著作権・免責について

　DelphiX の各ファイルは DXHeader、 FFEffects、 Samples フォルダを除いて、著作権は作者に属し、
フリーソフトウェアとして扱われます。

　DelphiX を利用して作られたソフトウェアの著作権は、そのソフトウェアの作者の物であり、
DelphiX の作者はそれについて何の権利も義務も持たないものとします。

　再配付に関しては、アーカイブの内容を変更せずに行ってください。別のファイルを付け加えて
再配付するのは構いません。出来れば事後でいいので下記のメールアドレスまでご一報下さい。

　また、勝手ながら、このソフトウェアを使用したことによる結果等については、作者は責任を負いかねますので、
使用者の責任において利用して下さい。


７．最後に

  質問、要望、バグ報告などは下記のメールアドレスに下さい。
皆さんのご協力期待しています。


堀　浩行	E-Mail: hori@ingjapan.ne.jp 
		Homepage: http://www.yks.ne.jp/~hori/
