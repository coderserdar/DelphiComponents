unit main;

interface

uses
  Windows, ActiveX, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OleCtrls, SHDocVw, ie5, Db, WebProtocol, ExtCtrls, Menus, DBTables;

type
  TFactFrm = class(TForm)
    WebBrowserControl1: TWebBrowserControl;
    WebProvider1: TWebProvider;
    Table1: TTable;
    Table1SpeciesNo: TFloatField;
    Table1Category: TStringField;
    Table1Common_Name: TStringField;
    Table1SpeciesName: TStringField;
    Table1Lengthcm: TFloatField;
    Table1Length_In: TFloatField;
    Table1Notes: TMemoField;
    Table1Graphic: TGraphicField;
    PopupMenu1: TPopupMenu;
    NextRecord1: TMenuItem;
    PriorRecord1: TMenuItem;
    FirstRecord1: TMenuItem;
    LastRecord1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Print1: TMenuItem;
    Preview1: TMenuItem;
    procedure WebBrowserControl1TitleChange(Sender: TObject;
      const Text: WideString);
    procedure FormCreate(Sender: TObject);
    procedure WebProvider1Items0Action(Sender: TObject;
      Request: TRequestObject; Response: TResponseObject;
      var Handled: Boolean);
    procedure WebProvider1Items1Action(Sender: TObject;
      Request: TRequestObject; Response: TResponseObject;
      var Handled: Boolean);
    procedure NextRecord1Click(Sender: TObject);
    procedure PriorRecord1Click(Sender: TObject);
    procedure FirstRecord1Click(Sender: TObject);
    procedure LastRecord1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure Preview1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FactFrm: TFactFrm;

implementation

{$R *.DFM}

procedure TFactFrm.FormCreate(Sender: TObject);
begin
  Table1.Open;
  WebProvider1.RegisterNameSpace;
  WebBrowserControl1.Navigate('local://./page.htm');
end;

procedure TFactFrm.WebBrowserControl1TitleChange(Sender: TObject;
  const Text: WideString);
begin
  Caption := Text;
end;

procedure TFactFrm.WebProvider1Items0Action(Sender: TObject;
  Request: TRequestObject; Response: TResponseObject;
  var Handled: Boolean);
begin
  if Request.Params.Values['MOVE'] = '0' then
    Table1.Prior
  else if Request.Params.Values['MOVE'] = '1' then
    Table1.Next;
  with Response do begin
    WriteTextLn('<HTML>');
    WriteTextLn('<HEAD><TITLE>FISH FACT</TITLE></HEAD>');
    WriteTextLn('<BODY>');
    WriteTextFmtLn('<DIV ALIGN=RIGHT><SMALL ALIGN=RIGHT>%d of %d</SMALL></DIV>',
      [Table1.RecNo, Table1.RecordCount]);
    WriteTextLn('<TABLE BORDER="0"><TR>');
    WriteTextFmt('<TD VALIGN=TOP><IMG SRC="local://./image.bmp"><P ALIGN=CENTER><B><I>%s</I></B></TD>',
      [Table1Common_Name.AsString]);
    WriteTextFmt('<TD><H3>About the %s</H3><P>%s</TD>',
      [Table1Common_Name.AsString, Table1Notes.AsString]);
    WriteTextLn('</TR></TABLE>');
    WriteTextLn('<P>');
    WriteTextLn('<TABLE WIDTH=100% BORDER="1">');
    WriteTextLn('<TR><TH>Category</TH></TH><TH>Species Name</TH><TH>Length (cm)</TH><TH>Length_In</TH></TR>');
    WriteTextFmtLn('<TR><TD ALIGN=CENTER>%s</TD></TD><TD ALIGN=CENTER>%s</TD><TD ALIGN=RIGHT>%f</TD><TD ALIGN=RIGHT>%f</TD></TR>',
      [Table1Category.AsString, Table1SpeciesName.AsString, Table1Lengthcm.AsFloat, Table1Length_In.AsFloat]);
    WriteTextLn('</TABLE>');
    WriteTextLn('<P ALIGN=RIGHT>');
    WriteTextLn('<INPUT CLASS="INPUT" TYPE="button" VALUE="Prior" OnClick="window.navigate(''page.htm?move=0'')">');
    WriteTextLn('<INPUT CLASS="INPUT" TYPE="button" VALUE="Next" OnClick="window.navigate(''page.htm?move=1'')">');
    WriteTextLn('</BODY>');
    WriteTextLn('</HTML>');
  end;
end;

procedure TFactFrm.WebProvider1Items1Action(Sender: TObject;
  Request: TRequestObject; Response: TResponseObject;
  var Handled: Boolean);
var
  Stream: TStream;
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  Stream := Response.GetStream;
  try
    Bitmap.Assign(Table1Graphic);
    Bitmap.SaveToStream(Stream);
  finally
    Bitmap.Free;
    Stream.Free;
  end;
end;

procedure TFactFrm.NextRecord1Click(Sender: TObject);
begin
  Table1.Next;
  WebBrowserControl1.Refresh;
end;

procedure TFactFrm.PriorRecord1Click(Sender: TObject);
begin
  Table1.Prior;
  WebBrowserControl1.Refresh;
end;

procedure TFactFrm.FirstRecord1Click(Sender: TObject);
begin
  Table1.First;
  WebBrowserControl1.Refresh;
end;

procedure TFactFrm.LastRecord1Click(Sender: TObject);
begin
  Table1.Last;
  WebBrowserControl1.Refresh;
end;

procedure TFactFrm.Print1Click(Sender: TObject);
begin
  WebBrowserControl1.ExecWB(OLECMDID_PRINT, OLECMDEXECOPT_DODEFAULT);
end;

procedure TFactFrm.Preview1Click(Sender: TObject);
begin
  WebBrowserControl1.ExecWB(OLECMDID_PRINTPREVIEW, OLECMDEXECOPT_DODEFAULT);
end;

initialization
  OleInitialize(nil);

finalization
  OleUnInitialize;

end.
