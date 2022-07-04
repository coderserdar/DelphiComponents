unit DnDListBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, shellapi;

// * Added (or changed) some variables, procedures and properties to support background image.
//   note) The background image is displayed when ListBox is empty.
//    - rewritten by Silhwan Hyun

type
  TFileDropped = procedure (Sender : TObject; FileName : string; var accept : boolean) of object;
  TDnDListBox = class(TListBox)
  private
    { Déclarations privées }
    FFileDropped : TFileDropped;
    FLbAddString, FLbInsertString, FLbDeleteString : TNotifyEvent;
    FAutoAdd : boolean;

    FBackgroundPicture : TBitmap;      // * Added
    FUseBackgroundPicture: boolean;    // * Added

    procedure SetBackgroundPicture(Value: TBitmap);     // * Added
    procedure SetUseBackgroundPicture(Value: Boolean);  // * Added
  protected
    { Déclarations protégées }
    procedure WMDROPFILES(var msg : TMessage); message WM_DROPFILES;
    procedure LBADDSTRING(var msg : TMessage); message LB_ADDSTRING;
    procedure LBINSERTSTRING(var msg : TMessage); message LB_INSERTSTRING;
    procedure LBDELETESTRING(var msg : TMessage); message LB_DELETESTRING;
    procedure FileDropped(FileName : string); virtual;

    procedure DrawBackground;                                     // * Added
    procedure WMPaint(var Message: TWMPaint); message WM_Paint;   // * Added
  public
    { Déclarations publiques }
    constructor Create(AOwner : TComponent); override;  // * Changed
    destructor Destroy; override;       // * Added
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
  published
    { Déclarations publiées }
    property AutoAdd : boolean read FAutoAdd write FAutoAdd;
    property BackgroundPicture: TBitmap read FBackgroundPicture write SetBackgroundPicture;     // * Added
    property UseBackgroundPicture: Boolean read FUseBackgroundPicture write SetUseBackgroundPicture default False;  // * Added
    property OnFileDropped : TFileDropped read FFileDropped write FFileDropped;
    property OnAddString : TNotifyEvent read FLbAddString write FLbAddString;
    property OnInsertString : TNotifyEvent read FLbInsertString write FLbInsertString;
    property OnDeleteString : TNotifyEvent read FLbDeleteString write FLbDeleteString;
  end;

procedure Register;

implementation


procedure TDnDListBox.DrawBackground;
{var
   Y_Used : integer; }
begin
  // Item ÀÌ Â÷ÁöÇÑ ºÎºÐ¸¸ Á¦¿ÜÇÏ°í ¹é±×¶ó¿îµå ÀÌ¹ÌÁö¸¦ Ç¥½ÃÇÒ °æ¿ì´Â ¾Æ·¡ ¹®ÀåÀ» ÀÌ¿ë
  { Y_Used := Self.ItemHeight * Self.Items.Count;
   if Y_Used = 0 then
      Y_Used := Self.ItemHeight;  // ¸Ç À§ ÇÑÁÙÀº ½Î¿ëÇÏÁö ¾Ê´Â´Ù.
   if Y_Used > Self.Height then
      exit;

   BitBlt(Self.Canvas.Handle, 0, Y_Used,
          FBackgroundPicture.Width, FBackgroundPicture.Height - (Y_Used - Self.ItemHeight),
          FBackgroundPicture.Canvas.Handle,
          0, Y_Used - Self.ItemHeight, SRCCOPY); }

   BitBlt(Self.Canvas.Handle, 0, Self.ItemHeight, FBackgroundPicture.Width, FBackgroundPicture.Height,
          FBackgroundPicture.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TDnDListBox.WMPaint(var Message: TWMPaint);
begin
    inherited;

    if FUseBackgroundPicture and (not FBackgroundPicture.Empty) and (Self.Items.Count = 0) then
       DrawBackground;

end;

procedure TDnDListBox.SetBackgroundPicture(Value: TBitmap);
begin
    FBackGroundPicture.Assign(Value);

    if (csDesigning in ComponentState) then
        begin
            Refresh;
        end;
end;

procedure TDnDListBox.SetUseBackgroundPicture(Value: Boolean);
begin
    if FUseBackgroundPicture <> Value then
        begin
            FUseBackgroundPicture := Value;

            Invalidate;
        end;

    if (csDesigning in ComponentState) then
        begin
            Refresh;
        end;
end;

constructor TDnDListBox.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FAutoAdd:=true;
  FBackGroundPicture := TBitmap.Create;
end;

destructor TDnDListBox.Destroy;
begin
   if Assigned(FBackGroundPicture) then
   begin
      FBackGroundPicture.Free;
      FBackGroundPicture := nil;
   end;

   inherited Destroy;
end;

procedure TDnDListBox.LBADDSTRING(var msg : TMessage);
begin
  if assigned(FLbAddString) then FLbAddString(self);
  inherited;
end;

procedure TDnDListBox.LBINSERTSTRING(var msg : TMessage);
begin
  if assigned(FLbInsertString) then FLbInsertString(self);
  inherited;
end;

procedure TDnDListBox.LBDELETESTRING(var msg : TMessage);
begin
  if assigned(FLbDeleteString) then FLbDeleteString(self);
  inherited;
end;

procedure TDnDListBox.FileDropped(FileName : string);
var acp : boolean;
begin
  acp:=true;
  if assigned(FFileDropped) then FFileDropped(self,FileName,acp);
  if (FAutoAdd and acp) then self.items.add(filename);
end;

procedure TDnDListBox.WMDROPFILES(var msg : TMessage);
var
  dr : HDrop;
  nb, j : integer;
  fn : array[0..254] of char;
begin
  dr:=msg.wparam;
  nb:=DragQueryFile(dr,$FFFFFFFF,fn,sizeof(fn));
  for j:=0 to nb-1 do begin
    DragQueryFile(dr,j,fn,sizeof(fn));
    FileDropped(fn);
  end;
  DragFinish(dr);
//  inherited;
end;

procedure TDnDListBox.CreateWnd;
begin
  inherited;
  DragAcceptFiles(self.handle,true);
end;

procedure TDnDListBox.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);

   Params.Style := Params.Style xor WS_VSCROLL;
end;

procedure Register;
begin
  RegisterComponents('Steff', [TDnDListBox]);
end;

end.
