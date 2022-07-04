unit SXStrLEdit;

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

 TSXStrEditDlg=class(TForm)
   Memo:TMemo;
   LineCount:TLabel;
   OpenDialog:TOpenDialog;
   SaveDialog:TSaveDialog;
   OKBtn:TButton;
   CancelBtn:TButton;
   HelpBtn:TButton;
   LoadBtn:TButton;
   SaveBtn:TButton;
   procedure FileOpen(Sender:TObject);
   procedure FileSave(Sender:TObject);
   procedure UpdateStatus(Sender:TObject);
   procedure FormCreate(Sender:TObject);
   procedure MemoKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
   procedure HelpBtnClick(Sender:TObject);
  private
   SingleLine:String[15];
   MultipleLines:String[15];
   FFilename:String;
 end;

 TSXMultilineProperty=class(TCaptionProperty)
  public
   function GetAttributes: TPropertyAttributes; override;
   function GetEditLimit: Integer; override;
   procedure Edit; override;
 end;

implementation

uses SysUtils, LibHelp;

{$R *.DFM}

resourcestring

 STextFilter='Text files (*.TXT)|*.TXT|Config files (*.SYS;*.INI)|*.SYS;*.INI|Batch files (*.BAT)|*.BAT|All files (*.*)|*.*';

{ TSXStrEditDlg }

procedure TSXStrEditDlg.FileOpen(Sender:TObject);
begin
 with OpenDialog do
  begin
   Filter:=STextFilter;
   Filename:=FFilename;
   if Execute then
    begin
     FFilename:=Filename;
     Memo.Lines.LoadFromFile(FileName);
    end;
  end;
end;

procedure TSXStrEditDlg.FileSave(Sender:TObject);
begin
 if SaveDialog.FileName='' then
  SaveDialog.FileName:=FFilename;
 with SaveDialog do
  begin
   Filter:=STextFilter;
   if Execute then
    Memo.Lines.SaveToFile(FileName);
  end;
end;

procedure TSXStrEditDlg.UpdateStatus(Sender:TObject);
var Count:Integer;
begin
 Count:=Memo.Lines.Count;
 if Count=1 then
  LineCount.Caption:=Format('%d %s',[Count,SingleLine]) else
   LineCount.Caption:=Format('%d %s',[Count,MultipleLines]);
end;

procedure TSXStrEditDlg.FormCreate(Sender:TObject);
begin
 HelpContext:=hcDStringListEditor;
 OpenDialog.HelpContext:=hcDStringListLoad;
 SaveDialog.HelpContext:=hcDStringListSave;
 SingleLine:='Line';
 MultipleLines:='Lines';
end;

procedure TSXStrEditDlg.MemoKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 if Key=VK_ESCAPE then CancelBtn.Click;
end;

procedure TSXStrEditDlg.HelpBtnClick(Sender:TObject);
begin
 Application.HelpContext(HelpContext);
end;

{ TSXMultilineProperty }

function TSXMultilineProperty.GetAttributes:TPropertyAttributes;
begin
 Result:=inherited GetAttributes+[paDialog];
end;

function TSXMultilineProperty.GetEditLimit:Integer;
begin
 if GetPropType^.Kind=tkString then
  Result:=GetTypeData(GetPropType)^.MaxLength else Result:=1024;
end;

procedure TSXMultilineProperty.Edit;
var Temp:String;
    Comp:TPersistent;
begin
 with TSXStrEditDlg.Create(Application) do
  try
   Comp:=GetComponent(0);
   if Comp is TComponent then
    Caption:=TComponent(Comp).Name+'.'+GetName else Caption:=GetName;
   Temp:=GetStrValue;
   Memo.Lines.Text:=Temp;
   Memo.MaxLength:=GetEditLimit;
   UpdateStatus(nil);
   if ShowModal=mrOk then
    begin
     Temp:=Memo.Lines.Text;
     SetStrValue(Temp);
    end;
  finally
   Free;
  end;
end;

end.

