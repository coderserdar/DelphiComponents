unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, API_listbox, ComCtrls;

type
  TForm1 = class(TForm)
    HeaderControl1: THeaderControl;
    API_listbox1: TAPI_listbox;
    Memo1: TMemo;
    Label1: TLabel;
    procedure HeaderControl1Resize(Sender: TObject);
    procedure HeaderControl1SectionResize(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
var
  s: string;
  sl: tstringlist;
begin
  with api_listbox1.items do
  begin
    clear;
    add('Jum Harvey||04120243||jim.harvey@imaginary1.com||');
    add('Paul Newname||23142||paul.newname@something.fi||');
    add('Jean Money||0987||jean.money@rich.me||');
  end;

  sl:= tstringlist.create;
  try
    sl.clear;
    sl.Add('Name'); sl.add('Phone'); sl.add('email');
    api_listbox1.ExportHtmlText(s, sl);
    memo1.text:= s;
  finally
    sl.free;
  end;

  label1.Caption:= inttostr( api_listbox1.CountColumns(0) );
end;

//------------------------------------------------------------------------------
procedure TForm1.HeaderControl1Resize(Sender: TObject);
begin
  api_listbox1.FormatColumns(headercontrol1);
end;

//------------------------------------------------------------------------------
procedure TForm1.HeaderControl1SectionResize(HeaderControl: THeaderControl;
  Section: THeaderSection);
begin
  api_listbox1.SetColWidth(section.Index, section.Width);
end;

end.
