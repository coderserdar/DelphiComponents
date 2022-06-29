{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  User interface for TnEmulVT component options
Creation:     May, 1996
Author:       François PIETTE
EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1996-2000 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>

              This software is provided 'as-is', without any express or
  	      implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment 
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source 
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Mar 18, 1999  Removed FormPos dependency

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit TnOptFrm;

interface

uses
  WinTypes, WinProcs, SysUtils, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, IniFiles, Buttons;

type
  TOptForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    AutoCRCheckBox: TCheckBox;
    AutoLFCheckBox: TCheckBox;
    LocalEchoCheckBox: TCheckBox;
    MonoChromeCheckBox: TCheckBox;
    RowsEdit: TEdit;
    ColsEdit: TEdit;
    OkButton: TButton;
    CancelButton: TButton;
    LaboButton: TButton;
    RDVButton: TButton;
    USUSButton: TButton;
    XlatCheckBox: TCheckBox;
    FontDialog1: TFontDialog;
    FontButton: TButton;
    LineHeightEdit: TEdit;
    Label3: TLabel;
    NamesButton: TButton;
    UpperLockCheckBox: TCheckBox;
    A11Button: TButton;
    GroupBox1: TGroupBox;
    FKeys1RadioButton: TRadioButton;
    FKeys2RadioButton: TRadioButton;
    FKeys3RadioButton: TRadioButton;
    Label4: TLabel;
    Label5: TLabel;
    LineZoomEdit: TEdit;
    CharZoomEdit: TEdit;
    GraphicDrawCheckBox: TCheckBox;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LaboButtonClick(Sender: TObject);
    procedure RDVButtonClick(Sender: TObject);
    procedure USUSButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FontButtonClick(Sender: TObject);
    procedure NamesButtonClick(Sender: TObject);
    procedure A11ButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    FIniFilename  : String;
    FInitialized  : Boolean;
    FSectionName  : String;
    FKeyName      : String;
    FHostName     : String;
    FFont         : TFont;
    FOnNamesClick : TNotifyEvent;
    function  GetLocalEcho   : Boolean;
    function  GetAutoCr      : Boolean;
    function  GetAutoLF      : Boolean;
    function  GetAltKeys     : Boolean;
    function  GetMonoChrome  : Boolean;
    function  GetUpperLock   : Boolean;
    function  GetXlat        : Boolean;
    function  GetGraphicDraw : Boolean;
    function  GetRows        : Integer;
    function  GetCols        : integer;
    function  GetLineHeight  : Integer;
    function  GetLineZoom    : Single;
    function  GetCharZoom    : Single;
    function  GetFKeys       : Integer;
    procedure SetLocalEcho(Value : Boolean);
    procedure SetAutoCr(Value : Boolean);
    procedure SetAutoLF(Value : Boolean);
    procedure SetAltKeys(Value : Boolean);
    procedure SetMonoChrome(Value : Boolean);
    procedure SetUpperLock(Value : Boolean);
    procedure SetXlat(Value : Boolean);
    procedure SetGraphicDraw(Value : Boolean);
    procedure SetRows(Value : Integer);
    procedure SetCols(Value : Integer);
    procedure SetHostName(Value : String);
    procedure SetLineHeight(Value : Integer);
    procedure SetLineZoom(Value : Single);
    procedure SetCharZoom(Value : Single);
    procedure SetFKeys(Value : Integer);

    property IniFilename : String  read FIniFileName   write FIniFileName;
    property SectionName : String  read FSectionName   write FSectionName;
    property KeyName     : String  read FKeyName       write FKeyName;
    property HostName    : String  read FHostName      write SetHostName;
    property LocalEcho   : Boolean read GetLocalEcho   write SetLocalEcho;
    property AutoCR      : Boolean read GetAutoCr      write SetAutoCR;
    property AutoLF      : Boolean read GetAutoLF      write SetAutoLF;
    property AltKeys     : Boolean read GetAltKeys     write SetAltKeys;
    property FKeys       : Integer read GetFKeys       write SetFkeys;
    property MonoChrome  : Boolean read GetMonoChrome  write SetMonoChrome;
    property UpperLock   : Boolean read GetUpperLock   write SetUpperLock;
    property Xlat        : Boolean read GetXlat        write SetXlat;
    property GraphicDraw : Boolean read GetGraphicDraw write SetGraphicDraw;
    property Rows        : Integer read GetRows        write SetRows;
    property Cols        : Integer read GetCols        write SetCols;
    property AFont       : TFont   read FFont          write FFont;
    property LineHeight  : Integer read GetLineHeight  write SetLineHeight;
    property LineZoom    : Single  read GetLineZoom    write SetLineZoom;
    property CharZoom    : Single  read GetCharZoom    write SetCharZoom;

    property OnNamesClick : TNotifyEvent read FOnNamesClick write FOnNamesClick;
  end;

var
  OptForm: TOptForm;

implementation

{$R *.DFM}

const
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function atoi(value : string) : Integer;
var
    i : Integer;
begin
    Result := 0;
    i := 1;
    while (i <= Length(Value)) and (Value[i] = ' ') do
        i := i + 1;
    while (i <= Length(Value)) and (Value[i] in ['0'..'9']) do begin
        Result := Result * 10 + ord(Value[i]) - ord('0');
        i := i + 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetFKeys(Value : Integer);
begin
    case Value of
    0 : FKeys1RadioButton.Checked := TRUE;
    1 : FKeys2RadioButton.Checked := TRUE;
    2 : FKeys3RadioButton.Checked := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOptForm.GetFKeys : Integer;
begin
   if FKeys1RadioButton.Checked then
       Result := 0
   else if FKeys2RadioButton.Checked then
       Result := 1
   else
       Result := 2;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetHostName(Value : String);
begin
    FHostName := Value;
    Caption := 'Options for ' + HostName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetLocalEcho  : Boolean;
begin
    Result := LocalEchoCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetAutoCr     : Boolean;
begin
    Result := AutoCrCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetAutoLF     : Boolean;
begin
    Result := AutoLFCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetXlat     : Boolean;
begin
    Result := XlatCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetGraphicDraw : Boolean;
begin
    Result := GraphicDrawCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetAltKeys    : Boolean;
begin
    Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetMonoChrome : Boolean;
begin
    Result := MonoChromeCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetUpperLock : Boolean;
begin
    Result := UpperLockCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetLocalEcho(Value : Boolean);
begin
    LocalEchoCheckBox.Checked := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetAutoCr(Value : Boolean);
begin
    AutoCRCheckBox.Checked := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetAutoLF(Value : Boolean);
begin
    AutoLFCheckBox.Checked := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetXlat(Value : Boolean);
begin
    XlatCheckBox.Checked := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetGraphicDraw(Value : Boolean);
begin
    GraphicDrawCheckBox.Checked := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetAltKeys(Value : Boolean);
begin
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetMonoChrome(Value : Boolean);
begin
    MonoChromeCheckBox.Checked := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetUpperLock(Value : Boolean);
begin
    UpperLockCheckBox.Checked := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetRows : Integer;
begin
    Result := atoi(RowsEdit.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetCols : integer;
begin
    Result := atoi(ColsEdit.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetLineZoom : Single;
begin
    try
        Result := StrToFloat(LineZoomEdit.Text);
    except
        Result := 1.0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetCharZoom : Single;
begin
    try
        Result := StrToFloat(CharZoomEdit.Text);
    except
        Result := 1.0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetLineHeight : integer;
var
    DC      : HDC;
    Metrics : TTextMetric;
    hObject : THandle;
begin
    Result := atoi(LineHeightEdit.Text);
    if Result = 0 then begin
        DC      := GetDC(0);
        hObject := SelectObject(DC, FFont.Handle);
        GetTextMetrics(DC, Metrics);
        SelectObject(DC, hOBject);
        ReleaseDC(0, DC);

        Result := Metrics.tmHeight;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetRows(Value : Integer);
begin
    RowsEdit.Text := IntToStr(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetCols(Value : Integer);
begin
    ColsEdit.Text := IntToStr(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetLineHeight(Value : Integer);
begin
    LineHeightEdit.Text := IntToStr(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetLineZoom(Value : Single);
begin
    LineZoomEdit.Text := Format('%5.3f', [Value]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetCharZoom(Value : Single);
begin
    CharZoomEdit.Text := Format('%5.3f', [Value]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.OkButtonClick(Sender: TObject);
begin
    ModalResult := IDOK;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.CancelButtonClick(Sender: TObject);
begin
    ModalResult := IDCANCEL;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionName, KeyName + KeyWidth,
                                            Width);
        Height       := IniFile.ReadInteger(SectionName, KeyName + KeyHeight,
                                            Height);
        Top          := IniFile.ReadInteger(SectionName, KeyName + KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionName, KeyName + KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        IniFile.Destroy;
    end;
    NamesButton.Visible := Assigned(FOnNamesClick);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionName, KeyName + KeyTop,         Top);
    IniFile.WriteInteger(SectionName, KeyName + KeyLeft,        Left);
    IniFile.WriteInteger(SectionName, KeyName + KeyWidth,       Width);
    IniFile.WriteInteger(SectionName, KeyName + KeyHeight,      Height);
    IniFile.Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.A11ButtonClick(Sender: TObject);
begin
    Rows       := 25;
    Cols       := 80;
    AutoCr     := FALSE;
    AutoLF     := FALSE;
    AltKeys    := TRUE;
    LocalEcho  := FALSE;
    MonoChrome := TRUE;
    UpperLock  := TRUE;
    Xlat       := FALSE;
    FKeys3RadioButton.Checked := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.LaboButtonClick(Sender: TObject);
begin
    Rows       := 24;
    Cols       := 80;
    AutoCr     := TRUE;
    AutoLF     := FALSE;
    AltKeys    := TRUE;
    LocalEcho  := FALSE;
    MonoChrome := TRUE;
    UpperLock  := FALSE;
    Xlat       := FALSE;
    FKeys2RadioButton.Checked := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.RDVButtonClick(Sender: TObject);
begin
    Rows       := 25;
    Cols       := 80;
    AutoCr     := FALSE;
    AutoLF     := FALSE;
    AltKeys    := FALSE;
    LocalEcho  := FALSE;
    MonoChrome := FALSE;
    UpperLock  := FALSE;
    Xlat       := TRUE;
    FKeys1RadioButton.Checked := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.USUSButtonClick(Sender: TObject);
begin
    Rows       := 25;
    Cols       := 80;
    AutoCr     := FALSE;
    AutoLF     := FALSE;
    AltKeys    := FALSE;
    LocalEcho  := FALSE;
    MonoChrome := FALSE;
    UpperLock  := FALSE;
    Xlat       := TRUE;
    FKeys2RadioButton.Checked := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.FormCreate(Sender: TObject);
begin
    FFont       := TFont.Create;
    SectionName := 'Windows';
    KeyName     := 'Options';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.FontButtonClick(Sender: TObject);
begin
    FontDialog1.Font := FFont;
    if FontDialog1.Execute then
        FFont := FontDialog1.Font;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.NamesButtonClick(Sender: TObject);
begin
    if Assigned(FOnNamesClick) then
        FOnNamesClick(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

