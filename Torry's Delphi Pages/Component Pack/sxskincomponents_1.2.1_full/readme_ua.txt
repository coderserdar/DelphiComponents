SXSkinComponents

Порядок інсталяції:

1. Встановіть пакет з папки ..\Graphics32_1_8_1_SX2\Packages:

   Delphi5: GR32_DSGN_D5.dpk
   Delphi6: GR32_DSGN_D6.dpk
   Delphi7: GR32_DSGN_D7.dpk
   BDS2005 (Delphi): GR32_DSGN_D2005.dpk
   C++Builder5: GR32_DSGN_CB5.dpk
   C++Builder6: GR32_DSGN_CB6.dpk
   BDS2006 (Delphi&C++Builder): GR32_DSGN_BDS2006.dpk

   !!! Видаліть попереднюю версію бібліотеки Graphics32 перед установкою !!!

2. Видаліть пакет з папки ..\SXSkinComponents\Packages:

   Delphi5: дивіться ..\SXSkinComponents\Packages\readme.txt
   Delphi6: SXSkin_D6_D.dpk
   Delphi7: SXSkin_D7_D.dpk
   BDS2005 (Delphi): SXSkin_D2005_D.dpk
   C++Builder5: дивіться ..\SXSkinComponents\Packages\readme.txt
   C++Builder6: SXSkin_CB6_D.dpk
   BDS2006 (Delphi&C++Builder): SXSkin_BDS2006_D.dpk

3. Додайте наступні шляхи в Search Path (Tools -> Options -> Library / Paths and Directories):

   ..\Graphics32_1_8_1_SX2
   ..\Graphics32_1_8_1_SX2\Packages
   ..\SXSkinComponents
   ..\SXSkinComponents\Packages

   Якщо Ви додали вказані шляхи в CB6, али тим не менш отримуєте помилку "Unable to open include file ...", тоді:
   - відкрийте Project -> Options -> Directories/Conditionals і додайте наступні шляхи в Library path:
     ..\Graphics32_1_8_1_SX2
     ..\SXSkinComponents

   Якщо Ви не можете встановити пакети GR32_DSGN_D2005 й SXSkin_D2005_D в BDS2005, тоді:
   - спробуйте спочатку встановити (install) пакети GR32_D2005 й SXSkin_D2005_R.

Веб-сайт: http://www.saarixx.info/sxskincomponents/ua/
E-Mail: sxskincomponents@saarixx.info

Copyright (C) 2006-2007, Олексій Садовніков. Всі права захищені.