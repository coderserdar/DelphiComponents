//----------------------------------------------------------------------------
// Unit Name: pb_about
// Author:    Helli
// Date:      12.07.2003
// Purpose:
// History:
//----------------------------------------------------------------------------
//  Copyright © 2003 by Hellinger Software.  All Rights Reserved.
//----------------------------------------------------------------------------

unit pb_about;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, jpeg, StdCtrls;

type
  TpbAboutFrm = class(TForm)
    Image1: TImage;
    Version: TLabel;
    Link: TLabel;
    procedure Image1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LinkClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

uses pb_common;

procedure TpbAboutFrm.Image1Click(Sender: TObject);
begin
 Close;
end;

procedure TpbAboutFrm.FormShow(Sender: TObject);
begin
 Caption:= GetLangStr (ltAbout);
 Version.Caption:= cVersion;
 Link.Caption:= GetLangStr (ltHomepage);
end;

procedure TpbAboutFrm.LinkClick(Sender: TObject);
begin
 RunApp (GetLangStr (ltUrl), '');
end;

end.
