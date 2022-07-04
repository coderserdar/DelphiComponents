How to create a Delphi/C++Builder packages:
 1. Create new package (File -> New -> Package).
 2. Save it as SXSkin_XX_R.bpk (e.g. SXSkin_D5_R or SXSkin_CB5_R).
 3. Go to project options and set Usage options to Designtime and Runtime.
 4. Add to this package all files except following:
     SXSkinIcons.dcr
     SXSkinNotebookReg.pas
     SXSkinReg_D.pas
     SXStrLEdit.pas
     SXSkinEditors.pas
 5. Compile/Buid this package.
 6. Create a new package.
 7. Save it as SXSkin_XX_D.bpk.
 8. Go to project options and set Usage options to Designtime only.
 9. Add following files to this package:
     SXSkinIcons.dcr
     SXSkinNotebookReg.pas
     SXSkinReg_D.pas
     SXStrLEdit.pas
     SXSkinEditors.pas
10. Click Project -> View Source and add SXSkin_XX_R to requires list.
11. Install the package.

P.S. If you can not add file SXSkinIcons.dcr, then try to add this line
     to the SXSkin_XX_R.bpk file:
     {$R '..\SXSkinIcons.dcr'}
     (Only if the packages is saved in folder SXSkinComponents\Packages).