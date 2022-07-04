SXSkinComponents

Порядок инсталляции:

1. Установите пакет из папки ..\Graphics32_1_8_1_SX2\Packages:

   Delphi5: GR32_DSGN_D5.dpk
   Delphi6: GR32_DSGN_D6.dpk
   Delphi7: GR32_DSGN_D7.dpk
   BDS2005 (Delphi): GR32_DSGN_D2005.dpk
   C++Builder5: GR32_DSGN_CB5.dpk
   C++Builder6: GR32_DSGN_CB6.dpk
   BDS2006 (Delphi&C++Builder): GR32_DSGN_BDS2006.dpk

   !!! Удалите предыдущую версию библиотеки Graphics32 перед установкой !!!

2. Установите пакет из папки ..\SXSkinComponents\Packages:

   Delphi5: смотрите ..\SXSkinComponents\Packages\readme.txt
   Delphi6: SXSkin_D6_D.dpk
   Delphi7: SXSkin_D7_D.dpk
   BDS2005 (Delphi): SXSkin_D2005_D.dpk
   C++Builder5: смотрите ..\SXSkinComponents\Packages\readme.txt
   C++Builder6: SXSkin_CB6_D.dpk
   BDS2006 (Delphi&C++Builder): SXSkin_BDS2006_D.dpk

3. Добавьте следующие пути в Search Path (Tools -> Options -> Library / Paths and Directories):

   ..\Graphics32_1_8_1_SX2
   ..\Graphics32_1_8_1_SX2\Packages
   ..\SXSkinComponents
   ..\SXSkinComponents\Packages

   Если Вы добавили указанные пути в CB6, но тем не менее получаете ошибку "Unable to open include file ...", тогда:
   - откройте Project -> Options -> Directories/Conditionals и добавьте следующие пути в Library path:
     ..\Graphics32_1_8_1_SX2
     ..\SXSkinComponents

   Если Вы не можете установить пакеты GR32_DSGN_D2005 и SXSkin_D2005_D в BDS2005, тогда:
   - попробуйте вначале установить (install) пакеты GR32_D2005 и SXSkin_D2005_R.

Веб-сайт: http://www.saarixx.info/sxskincomponents/ru/
E-Mail: sxskincomponents@saarixx.info

Copyright (C) 2006-2007, Алексей Садовников. Все права защищены.