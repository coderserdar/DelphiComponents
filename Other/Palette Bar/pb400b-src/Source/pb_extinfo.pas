//----------------------------------------------------------------------------
// Unit Name: pb_extinfo
// Author:    Helli
// Date:      29.07.2004
// Purpose:
// History:
//----------------------------------------------------------------------------
//  Copyright © 2004 by Hellinger Software.  All Rights Reserved.
//----------------------------------------------------------------------------

unit pb_extinfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  pb_Common, Buttons;

type
  TEdExinfo = class(TForm)
    Info: TEdit;
    OpenDialog: TOpenDialog;
    BtnCancel: TButton;
    BtnOk: TButton;
    BtnPath: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure BtnPathClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  EdExinfo: TEdExinfo;

implementation

{$R *.dfm}

procedure TEdExinfo.FormShow(Sender: TObject);
begin
 Caption:= GetLangStr (ltEnterExtInfo);
 BtnOk.Caption:= GetLangStr (ltOKCaption);
 BtnCancel.Caption:= GetLangStr (ltCancelCaption);
end;

procedure TEdExinfo.BtnPathClick(Sender: TObject);
begin
 if OpenDialog.Execute then begin
  Info.Text:= OpenDialog.FileName;
 end;
end;

procedure TEdExinfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 SaveFormPos (EdExinfo, cRegEdExInfo);
end;

end.
