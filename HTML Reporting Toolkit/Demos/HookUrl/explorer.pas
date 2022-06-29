unit explorer;

interface

uses
  Windows, ActiveX, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, OleCtrls, SHDocVw, ie5, ExtCtrls, WebProtocol;

type
  TBrowserForm = class(TForm)
    ListBox1: TListBox;
    Splitter1: TSplitter;
    Panel1: TPanel;
    WebBrowserControl1: TWebBrowserControl;
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    WebProvider1: TWebProvider;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure WebBrowserControl1StatusTextChange(Sender: TObject;
      const Text: WideString);
    procedure WebProvider1Items0Action(Sender: TObject;
      Request: TRequestObject; Response: TResponseObject;
      var Handled: Boolean);
    procedure ListBox1DblClick(Sender: TObject);
    procedure WebBrowserControl1NavigateComplete2(Sender: TObject;
      const pDisp: IDispatch; var URL: OleVariant);
    procedure WebProvider1Items1Action(Sender: TObject;
      Request: TRequestObject; Response: TResponseObject;
      var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BrowserForm: TBrowserForm;

implementation
uses clipbrd;

{$R *.DFM}

procedure TBrowserForm.FormCreate(Sender: TObject);
begin
  WebBrowserControl1.Navigate('about:blank');
  WebProvider1.RegisterNameSpace;
end;

procedure TBrowserForm.Button1Click(Sender: TObject);
begin
  WebBrowserControl1.Navigate(Edit1.Text);
  ListBox1.Clear;  
end;

procedure TBrowserForm.WebBrowserControl1StatusTextChange(Sender: TObject;
  const Text: WideString);
begin
  if Text <> '' then
    Caption := 'Internet Explorer - ' + Text else
    Caption := 'Internet Explorer';
end;

procedure TBrowserForm.ListBox1DblClick(Sender: TObject);
begin
  with ListBox1 do
  if ItemIndex <> -1 then
    begin
      Clipboard.SetTextBuf(PChar(Items[ItemIndex]));
      WebBrowserControl1.Navigate(Items[ItemIndex]);
    end;
end;

procedure TBrowserForm.WebBrowserControl1NavigateComplete2(Sender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
begin
  if URL <> 'about:blank' then
    Edit1.Text := URL;
end;

procedure TBrowserForm.WebProvider1Items0Action(Sender: TObject;
  Request: TRequestObject; Response: TResponseObject;
  var Handled: Boolean);
begin
  ListBox1.Items.Add(Request.URL);
  Response.UseDefaultHandler;
end;

procedure TBrowserForm.WebProvider1Items1Action(Sender: TObject;
  Request: TRequestObject; Response: TResponseObject;
  var Handled: Boolean);
var
  FileStream: TFileStream;
  Stream: TStream;
begin
  Stream := Response.GetStream;
  try
    FileStream := TFileStream.Create(ExtractFilePath(ParamStr(0)) + 'mslogo1.jpg', fmOpenRead);
    try
      Stream.CopyFrom(FileStream, FileStream.Size);
    finally
      FileStream.Free;
    end;
  finally
    Stream.Free;
  end;
end;

initialization
  OleInitialize(nil);

finalization
  OleUninitialize;

end.
