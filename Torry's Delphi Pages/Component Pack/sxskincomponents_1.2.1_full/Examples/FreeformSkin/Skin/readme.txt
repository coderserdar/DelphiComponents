Note: These files are not used in FreeformSkin demo application. They were
converted first to SXS-file (only skin.ini) and then together compressed to
ZIP-file, which was stored in FreeformUnit.dfm with use of TSXStoredSkin
component.

If you want to use a stored skin your appilcation (like it is done in this demo),
first you should use program SkinConverted\SXSkinConverter.exe from SXSkinComponents
full distribution package. This application will help you to convert your skin.ini
file to skin.sxs (the only skin file format, which can be used in zipped skins).
Then you should compress all graphics and file skin.sxs to .zip-file using any
compression utility.

Then open your project in Delphi, add TSXStoredSkin component on your form. Click
ellipsis button near FileName property of newly created component. Select your
zipped skin in the opened dialog window. That's all. Your skin is stores and you
don't need any external graphics and skin files to work properly.