{
Article: 
TRichEditURL - hyperlink aware RichEdit
http://delphi.about.com/library/weekly/aa051804a.htm

Full source code of the TRichEditURL Delphi component, 
an extension to the standard RichEdit component. The 
TRichEditURL automatically recognizes URLs. Whenever the 
text in a RichEditURL matches the format of a URL, the 
control will display it as a hyperlink - when the link is 
clicked an event is raised enabling you to, for example, 
open a browser or send an email. The TRichEditURL works 
correctly event when placed on a Panel or any other container control. 
}

{
********************************************
Zarko Gajic
About.com Guide to Delphi Programming
http://delphi.about.com
email: delphi.guide@about.com
free newsletter: http://delphi.about.com/library/blnewsletter.htm
forum: http://forums.about.com/ab-delphi/start/
********************************************
}

unit ATxREUrl;

interface

uses
  Windows, Messages, Classes, Controls, ComCtrls;
  
type
  TURLClickEvent = procedure(Sender: TObject; const URL: AnsiString) of object;

  TRichEditURL = class(TRichEdit)
  private
    FOnURLClick: TURLClickEvent;
    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;
  protected
    procedure DoURLClick(const URL: AnsiString);
    procedure CreateWnd; override;
  public
    procedure InitURL;
  published
    property OnURLClick: TURLClickEvent read FOnURLClick write FOnURLClick;
  end;

implementation

uses
  RichEdit;


{ TRichEditURL }

procedure TRichEditURL.DoURLClick(const URL: AnsiString);
begin
  if Assigned(FOnURLClick) then
    FOnURLClick(Self, URL);
end;

procedure TRichEditURL.CNNotify(var Msg: TWMNotify);
var
  p: TENLink;
  sURL: AnsiString;
begin
  if (Msg.NMHdr^.code = EN_LINK) then
  begin
    p := TENLink(Pointer(Msg.NMHdr)^);
    if (p.Msg = WM_LBUTTONDOWN) then
    begin
      try
        SendMessage(Handle, EM_EXSETSEL, 0, Longint(@(p.chrg)));
        sURL := SelText;
        DoURLClick(sURL);
      except
      end;
    end;
  end;

 inherited;
end;

procedure TRichEditURL.CreateWnd;
begin
  inherited CreateWnd;
  InitURL;
end;

procedure TRichEditURL.InitURL;
var
  mask: Longint;
begin
  SendMessage(Handle, EM_AUTOURLDETECT, 1, 0);
  mask := SendMessage(Handle, EM_GETEVENTMASK, 0, 0);
  SendMessage(Handle, EM_SETEVENTMASK, 0, mask or ENM_LINK);
end;


end.
