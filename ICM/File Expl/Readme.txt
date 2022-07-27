dows XP ThemeManifest
========================

Please note that a WinXP resource manifest, viz. WinXP.res, has been included in the Demo programs (Many thanks to Thomas Stutz for creating this XML manifest).

When Demo1 or Demo2 runs under Windows XP, the manifest tells library loader to load a specific version of ComCtrl32.dll (V6 for Windows XP), thus a number of controls in the Demo1 or Demo2 will be displayed in Windows XP's visual style.

Should it causes an Access Violation error or any other errors when running these Demos, just comment off or delete following line, then re-compiling it.

  {$R WinXP.res} in FileExpl2876_Demo1Main.pas in Demo1 or
  {$R WinXP.res} in FileExpl2876_Demo2Main.pas in Demo2.
 
Probably, you've noticed that the Polish- and Slovak-language translations are incorrect in the Demo programs.  This is not a bug, but because these Demos were compiled by English language WindowsXP.  However, these language translation will be displayed correctly once these Demos are re-compiled under Windows with appropriate language support.