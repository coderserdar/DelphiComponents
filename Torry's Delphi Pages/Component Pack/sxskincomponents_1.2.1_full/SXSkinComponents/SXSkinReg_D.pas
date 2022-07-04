unit SXSkinReg_D;

////////////////////////////////////////////////////////////////////////////////
// SXSkinComponents: Skinnable Visual Controls for Delphi and C++Builder      //
//----------------------------------------------------------------------------//
// Version: 1.2.1                                                             //
// Author: Alexey Sadovnikov                                                  //
// Web Site: http://www.saarixx.info/sxskincomponents/                        //
// E-Mail: sxskincomponents@saarixx.info                                      //
//----------------------------------------------------------------------------//
// LICENSE:                                                                   //
// 1. You may freely distribute this file.                                    //
// 2. You may not make any changes to this file.                              //
// 3. The only person who may change this file is Alexey Sadovnikov.          //
// 4. You may use this file in your freeware projects.                        //
// 5. If you want to use this file in your shareware or commercial project,   //
//    you should purchase a project license or a personal license of          //
//    SXSkinComponents: http://saarixx.info/sxskincomponents/en/purchase.htm  //
// 6. You may freely use, distribute and modify skins for SXSkinComponents.   //
// 7. You may create skins for SXSkinComponents.                              //
//----------------------------------------------------------------------------//
// Copyright (C) 2006-2007, Alexey Sadovnikov. All Rights Reserved.           //
////////////////////////////////////////////////////////////////////////////////

interface

{$I Compilers.inc}

uses Classes, TypInfo,
     {$IFDEF COMPILER_6_UP}
     DesignIntf
     {$ELSE}
     DsgnIntf
     {$ENDIF};

procedure Register;

implementation

uses SysUtils, SXSkinLibrary, SXSkinCheckBox, Forms, Controls, SXSkinEditors,
     SXStrLEdit, SXSkinImage, SXSkinPanel, SXSkinRadioButton, SXSkinGroupBox,
     SXSkinLabel, SXSkinButton, SXSkinEdit, SXSkinSpinEdit, SXSkinNotebook,
     SXSkinPaintBox, SXSkinForm, SXSkinUpDown;

procedure Register;
begin
 RegisterComponents('SXSkinComponents',[TSXSkinLibrary,TSXSkinForm,
   TSXStoredSkin,TSXSkinImage,TSXSkinPanel,TSXSkinLabel,TSXSkinButton,
   TSXSkinCheckBox,TSXSkinRadioButton,TSXSkinEdit,TSXSkinSpinEdit,
   TSXSkinGroupBox,TSXSkinUpDown,TSXSkinNotebook,TSXSkinPaintBox]);
 RegisterClasses([TSXSkinNotebookPage]);
 //
 RegisterPropertyEditor(TypeInfo(TCaption),TSXSkinLabel,'Caption',TSXMultilineProperty);
 RegisterPropertyEditor(TypeInfo(TCaption),TSXSkinCheckBox,'Caption',TSXMultilineProperty);
 RegisterPropertyEditor(TypeInfo(TCaption),TSXSkinRadioButton,'Caption',TSXMultilineProperty);
 RegisterPropertyEditor(TypeInfo(TCaption),TSXSkinGroupBox,'Caption',TSXMultilineProperty);
 RegisterPropertyEditor(TypeInfo(TCaption),TSXSkinButton,'Caption',TSXMultilineProperty);
 RegisterPropertyEditor(TypeInfo(String),TSXSkinLibrary,'SkinFile',TSXSkinFileProperty);
 RegisterPropertyEditor(TypeInfo(String),TSXSkinLibrary,'SkinFile2',TSXSkinFileProperty);
 RegisterPropertyEditor(TypeInfo(String),TSXStoredSkin,'FileName',TSXSkinZipFileProperty); 
end;

end.
