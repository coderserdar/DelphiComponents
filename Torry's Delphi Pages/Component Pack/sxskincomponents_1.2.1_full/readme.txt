SXSkinComponents

Installation Order:

1. Install package from ..\Graphics32_1_8_1_SX2\Packages folder:

   Delphi5: GR32_DSGN_D5.dpk
   Delphi6: GR32_DSGN_D6.dpk
   Delphi7: GR32_DSGN_D7.dpk
   BDS2005 (Delphi): GR32_DSGN_D2005.dpk
   C++Builder5: GR32_DSGN_CB5.dpk
   C++Builder6: GR32_DSGN_CB6.dpk
   BDS2006 (Delphi&C++Builder): GR32_DSGN_BDS2006.dpk

   !!! Remove previous installed Graphics32 package before install !!!

2. Install package from ..\SXSkinComponents\Packages folder:

   Delphi5: see ..\SXSkinComponents\Packages\readme.txt
   Delphi6: SXSkin_D6_D.dpk
   Delphi7: SXSkin_D7_D.dpk
   BDS2005 (Delphi): SXSkin_D2005_D.dpk
   C++Builder5: see ..\SXSkinComponents\Packages\readme.txt
   C++Builder6: SXSkin_CB6_D.dpk
   BDS2006 (Delphi&C++Builder): SXSkin_BDS2006_D.dpk

3. Add following paths to Search Path (Tools -> Options -> Library / Paths and Directories):
   ..\Graphics32_1_8_1_SX2
   ..\Graphics32_1_8_1_SX2\Packages
   ..\SXSkinComponents
   ..\SXSkinComponents\Packages

   If you added these path in CB6, but however have "Unable to open include file ..." error, then:
   - open Project -> Options -> Directories/Conditionals and add these path to Library path:
     ..\Graphics32_1_8_1_SX2
     ..\SXSkinComponents

   If you can not install GR32_DSGN_D2005 and SXSkin_D2005_D in BDS2005, then:
   - try to install GR32_D2005 and SXSkin_D2005_R first.

Web Site: http://www.saarixx.info/sxskincomponents/en/
E-Mail: sxskincomponents@saarixx.info

Copyright (C) 2006-2007, Alexey Sadovnikov. All Rights Reserved.