unit SXSkinEditors;

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

uses Windows, Classes, Graphics, Forms, Controls, Buttons, Dialogs, TypInfo,
     StdCtrls, ExtCtrls, DesignIntf, DesignEditors, VCLEditors;

type

 TSXSkinFileProperty=class(TCaptionProperty)
  public
   function GetAttributes: TPropertyAttributes; override;
   function GetEditLimit: Integer; override;
   procedure Edit; override;
 end;

 TSXSkinZipFileProperty=class(TCaptionProperty)
  public
   function GetAttributes: TPropertyAttributes; override;
   function GetEditLimit: Integer; override;
   procedure Edit; override;
 end;

implementation

uses SXSkinLibrary, SXSkinUtils;

{ TSXSkinFileProperty }

function TSXSkinFileProperty.GetAttributes:TPropertyAttributes;
begin
 Result:=inherited GetAttributes+[paDialog];
end;

function TSXSkinFileProperty.GetEditLimit:Integer;
begin
 if GetPropType^.Kind=tkString then
  Result:=GetTypeData(GetPropType)^.MaxLength else Result:=1024;
end;

procedure TSXSkinFileProperty.Edit;
var        T:TOpenDialog;
        Comp:TPersistent;
 SkinLibrary:TSXSkinLibrary;
  FileP,DirP:String;
begin
 T:=TOpenDialog.Create(Application);
 try
  T.Filter:='Text skin files (*.INI)|*.INI|'+
            'Binary skin files (*.SXS)|*.SXS|'+
            'Compressed skin files (*.ZIP)|*.ZIP|'+
            'All files (*.*)|*.*';
  T.Options:=T.Options+[ofPathMustExist,ofFileMustExist]-[ofHideReadOnly];
  DirP:='';
  FileP:=GetStrValue;
  Comp:=GetComponent(0);
  if Comp is TSXSkinLibrary then
   begin
    SkinLibrary:=TSXSkinLibrary(Comp);
    if SkinLibrary.SkinDir<>'' then
     DirP:=WithLastSlash(SkinLibrary.SkinDir);
    if (DirP<>'') and PathIsRelative(FileP) then
     FileP:=GetFullPath(FileP,DirP);
   end;
  T.InitialDir:=GetFilePath(FileP);
  if T.Execute then
   begin
    FileP:=T.FileName;
    if DirP<>'' then
     FileP:=GetRelativePath(DirP,FileP) else
    if Comp is TSXSkinLibrary then
     begin
      SkinLibrary:=TSXSkinLibrary(Comp);
      SkinLibrary.SkinDir:=GetUpDir(GetFilePath(FileP));
      FileP:=GetRelativePath(WithLastSlash(SkinLibrary.SkinDir),FileP);
     end;
    SetStrValue(FileP);
   end;
 finally
  T.Free;
 end;
end;

{ TSXSkinZipFileProperty }

function TSXSkinZipFileProperty.GetAttributes:TPropertyAttributes;
begin
 Result:=inherited GetAttributes+[paDialog];
end;

function TSXSkinZipFileProperty.GetEditLimit:Integer;
begin
 if GetPropType^.Kind=tkString then
  Result:=GetTypeData(GetPropType)^.MaxLength else Result:=1024;
end;

procedure TSXSkinZipFileProperty.Edit;
var   T:TOpenDialog;
  FileP:String;
begin
 T:=TOpenDialog.Create(Application);
 try
  T.Filter:='Compressed skin files (*.ZIP)|*.ZIP|'+
            'All files (*.*)|*.*';
  T.Options:=T.Options+[ofPathMustExist,ofFileMustExist]-[ofHideReadOnly];
  FileP:=GetStrValue;
  T.InitialDir:=GetFilePath(FileP);
  if T.Execute then
   begin
    FileP:=T.FileName;
    SetStrValue(FileP);
   end;
 finally
  T.Free;
 end;
end;

end.
