unit SXSkinUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SXSkinLibrary, SXSkinControl, SXSkinImage, ExtCtrls, Menus, IniFiles,
  SXSkinUtils, StdCtrls, SXSkinButton, SXSkinForm, ToolWin, ComCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    SXSkinImage1: TSXSkinImage;
    SXSkinLibrary1: TSXSkinLibrary;
    MainMenu1: TMainMenu;
    Skins1: TMenuItem;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    SXSkinButton1: TSXSkinButton;
    SXSkinForm1: TSXSkinForm;
    ToolBar1: TToolBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Exit1Click(Sender: TObject);    
  private
    { Private declarations }
  public
    { Public declarations }
    SkinNames:TStringList;
    SkinPaths:TStringList;
    procedure SkinChange(Sender:TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var   SR:TSearchRec;
     A,B:Integer;
    L,SL:TStringList;
      S1:String;
       T:TMenuItem;
    Skin:TSXStoredSkin;    
begin
 SkinNames:=TStringList.Create;
 SkinPaths:=TStringList.Create;
 S1:=WithlastSlash(ExtractFilePath(Application.ExeName))+'Skins\';
 SXSkinLibrary1.SkinDir:=WithlastSlash(ExtractFilePath(Application.ExeName))+'Skins';
 if FileExists(SXSkinLibrary1.SkinDir+'\App_Skins.zip') then
  SXSkinLibrary1.SkinFile2:=SXSkinLibrary1.SkinDir+'\App_Skins.zip' else
 if FileExists(SXSkinLibrary1.SkinDir+'\App_Skins\skin.sxs') then
  SXSkinLibrary1.SkinFile2:=SXSkinLibrary1.SkinDir+'\App_Skins\skin.sxs' else
 if FileExists(SXSkinLibrary1.SkinDir+'\App_Skins\skin.ini') then
  SXSkinLibrary1.SkinFile2:=SXSkinLibrary1.SkinDir+'\App_Skins\skin.ini' else
   SXSkinLibrary1.SkinFile2:='';
 L:=TStringList.Create;
 try
  SL:=TStringList.Create;
  try
   for A:=0 to StoredSkinCount-1 do
    begin
     Skin:=GetStoredSkinByIndex(A);
     LoadStringsFromZIPSkinStream(Skin.Stream,SL);
     if SL.Values['SkinName']<>'' then
      begin
       SkinNames.Add(SL.Values['SkinName']+' (internal)');
       SkinPaths.Add(':'+Skin.FileName);
      end;
    end;
  finally
   SL.Free;
  end;
  // 
  A:=0;
  repeat
   Inc(A);
   if A=1 then B:=FindFirst(S1+'*.*',faDirectory,SR) else
    B:=FindNext(SR);
   if (B=0) and (SR.Name<>'.') and (SR.Name<>'..') then
    L.Add(S1+SR.Name);
  until B<>0;
  FindClose(SR);
  SL:=TStringList.Create;
  try
   for A:=0 to L.Count-1 do
    begin
     if FileExists(L[A]+'\skin.sxs') then
      begin
       LoadStringsFromSkinFile(L[A]+'\skin.sxs',SL);
       if SL.Values['SkinName']<>'' then
        begin
         SkinNames.Add(SL.Values['SkinName']+' (SXS)');
         SkinPaths.Add(L[A]+'\skin.sxs');
        end; 
      end else
     if FileExists(L[A]+'\skin.ini') then
      begin
       LoadStringsFromSkinFile(L[A]+'\skin.ini',SL);
       if SL.Values['SkinName']<>'' then
        begin
         SkinNames.Add(SL.Values['SkinName']+' (INI)');
         SkinPaths.Add(L[A]+'\skin.ini');
        end;
      end;
    end;
  finally
   SL.Free;
  end;
  //
  L.Clear;
  A:=0;
  repeat
   Inc(A);
   if A=1 then B:=FindFirst(S1+'*.zip',faAnyFile,SR) else
    B:=FindNext(SR);
   if (B=0) and (SR.Name<>'.') and (SR.Name<>'..') then
    L.Add(S1+SR.Name);
  until B<>0;
  FindClose(SR);
  //
  SL:=TStringList.Create;
  try
   for A:=0 to L.Count-1 do
    begin
     if FileExists(L[A]) then
      begin
       LoadStringsFromSkinFile(L[A],SL);
       if SL.Values['SkinName']<>'' then
        begin
         SkinNames.Add(SL.Values['SkinName']+' (ZIP)');
         SkinPaths.Add(L[A]);
        end;
      end
    end;
  finally
   SL.Free;
  end;  
 finally
  L.Free;
 end;
 for A:=0 to SkinNames.Count-1 do
  begin
   T:=TMenuItem.Create(Skins1);
   T.Caption:=SkinNames[A];
   T.Tag:=A;
   T.OnClick:=SkinChange;
   Skins1.Add(T);
   if A=0 then
    SkinChange(T);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 SkinNames.Free;
 SkinPaths.Free;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
 Close;
end;

procedure TForm1.SkinChange(Sender:TObject);
var A:Integer;
    S:String;
 Skin:TSXStoredSkin;
begin
 if not (Sender is TMenuItem) then exit;
 for A:=0 to TMenuItem(Sender).Parent.Count-1 do
  TMenuItem(Sender).Parent.Items[A].Checked:=False;
 TMenuItem(Sender).Checked:=True;
 SXSkinLibrary1.Active:=False;
 if TMenuItem(Sender).Tag<0 then exit;
 S:=SkinPaths[TMenuItem(Sender).Tag];
 if (S<>'') and (S[1]=':') then
  begin
   Skin:=GetStoredSkinByZIPName(Copy(S,2,MaxInt));
   SXSkinLibrary1.StoredSkin:=Skin;
  end else SXSkinLibrary1.SkinFile:=S;
 SXSkinLibrary1.Active:=True;
end;

end.
