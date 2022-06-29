{**********************************************************}
{                                                          }
{  Devrace Extension Library example of                    }
{  TELEvent, TELEventSender                                }
{                                                          }
{  Copyright (c) 2001, Balabuyev Yevgeny                   }
{  Contact: yebalabuyev@devrace.com                        }
{                                                          }
{ -------------------------------------------------------- }
{  ExtLib home page      : http://www.devrace.com/extlib   }
{  ExtLib support e-mail : extlib@devrace.com              }
{ -------------------------------------------------------- }
{                                                          }
{  Please see the file License.txt for full license        }
{  information                                             }
{                                                          }
{**********************************************************}

unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ColorGrd, Registry, ELControls;

type
  TForm3 = class(TForm)
    FontDialog1: TFontDialog;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ColorGrid1: TColorGrid;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ELEventSender1: TELEventSender;
    Label2: TLabel;
    ColorGrid2: TColorGrid;
    procedure Button1Click(Sender: TObject);
    procedure ColorGrid1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FCancelFont: TFont;
    FCancelBackgroundColor: TColor;
    FCancelAutoOpenNewDoc: Boolean;
    FCancelShowAnnotation: Boolean;
    FCancelMainFormBackColor: TColor;
    function GetMemoBackgroundColor: TColor;
    function GetMemoFont: TFont;
    function GetAutoOpenNewDoc: Boolean;
    function GetShowAnnotation: Boolean;
    procedure SaveToReg;
    procedure LoadFromReg;
    function GetMainFormBackColor: TColor;
  public
    property MemoBackgroundColor: TColor read GetMemoBackgroundColor;
    property MemoFont: TFont read GetMemoFont;
    property AutoOpenNewDoc: Boolean read GetAutoOpenNewDoc;
    property ShowAnnotation: Boolean read GetShowAnnotation;
    property MainFormBackColor: TColor read GetMainFormBackColor;
  end;

  TFontSaver = class(TComponent)
  private
    FFont: TFont;
    procedure SetFont(const Value: TFont);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Font: TFont read FFont write SetFont;
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(Memo1.Font);
  if FontDialog1.Execute then
  begin
    Memo1.Font.Assign(FontDialog1.Font);
    ColorGrid1.ForegroundIndex := ColorGrid1.ColorToIndex(Memo1.Font.Color);
  end;
end;

procedure TForm3.ColorGrid1Change(Sender: TObject);
begin
  Memo1.Color := ColorGrid1.BackgroundColor;
  Memo1.Font.Color := ColorGrid1.ForegroundColor;
end;

function TForm3.GetAutoOpenNewDoc: Boolean;
begin
  Result := CheckBox1.Checked;
end;

function TForm3.GetMemoBackgroundColor: TColor;
begin
  Result := Memo1.Color;
end;

function TForm3.GetMemoFont: TFont;
begin
  Result := Memo1.Font;
end;

function TForm3.GetShowAnnotation: Boolean;
begin
  Result := CheckBox1.Checked and CheckBox2.Checked;
end;

procedure TForm3.CheckBox1Click(Sender: TObject);
begin
  CheckBox2.Enabled := CheckBox1.Checked;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  FCancelFont := TFont.Create;
  LoadFromReg;
end;

procedure TForm3.LoadFromReg;
var
  LR: TRegIniFile;
  LS: TMemoryStream;
  LFS: TFontSaver;
  LB: Boolean;
  LC: TColor;
  LDI: TRegDataInfo;
begin
  {$IFDEF VER150}
  LR := TRegIniFile.Create('Software\ExtensionLibrary\TELEventDemo_D7');
  {$ENDIF}
  {$IFDEF VER140}
  LR := TRegIniFile.Create('Software\ExtensionLibrary\TELEventDemo_D6');
  {$ENDIF}
  {$IFDEF VER130}
  LR := TRegIniFile.Create('Software\ExtensionLibrary\TELEventDemo_D5');
  {$ENDIF}
  LS := TMemoryStream.Create;
  LFS := TFontSaver.Create(nil);
  try
    if LR.ValueExists('Options') then
    begin
      LS.Size := 10000;
      LR.GetDataInfo('Options', LDI);
      LS.Size := LDI.DataSize;
      LR.ReadBinaryData('Options', LS.Memory^, LS.Size);
      LS.Position := 0;
      LS.Read(LB, SizeOf(Boolean));
      CheckBox1.Checked := LB;
      LS.Read(LB, SizeOf(Boolean));
      CheckBox2.Checked := LB;
      LS.ReadComponent(LFS);
      Memo1.Font.Assign(LFS.Font);
      ColorGrid1.ForegroundIndex := ColorGrid1.ColorToIndex(LFS.Font.Color);
      LS.Read(LC, SizeOf(LC));
      Memo1.Color := LC;
      ColorGrid1.BackgroundIndex := ColorGrid1.ColorToIndex(LC);
      LS.Read(LC, SizeOf(LC));
      ColorGrid2.BackgroundIndex := ColorGrid2.ColorToIndex(LC);
    end;
  finally
    LR.Free;
    LS.Free;
    LFS.Free;
  end;
end;

procedure TForm3.SaveToReg;
var
  LR: TRegIniFile;
  LS: TMemoryStream;
  LFS: TFontSaver;
  LB: Boolean;
  LC: TColor;
begin
  {$IFDEF VER150}
  LR := TRegIniFile.Create('Software\ExtensionLibrary\TELEventDemo_D7');
  {$ENDIF}
  {$IFDEF VER140}
  LR := TRegIniFile.Create('Software\ExtensionLibrary\TELEventDemo_D6');
  {$ENDIF}
  {$IFDEF VER130}
  LR := TRegIniFile.Create('Software\ExtensionLibrary\TELEventDemo_D5');
  {$ENDIF}
  LS := TMemoryStream.Create;
  LFS := TFontSaver.Create(nil);
  try
    LB := CheckBox1.Checked;
    LS.Write(LB, SizeOf(Boolean));
    LB := CheckBox2.Checked;
    LS.Write(LB, SizeOf(Boolean));
    LFS.Font := Memo1.Font;
    LS.WriteComponent(LFS);
    LC := MemoBackgroundColor;
    LS.Write(LC, SizeOf(LC));
    LC := MainFormBackColor;
    LS.Write(LC, SizeOf(LC));

    LS.Position := 0;
    LR.WriteBinaryData('Options', LS.Memory^, LS.Size);
  finally
    LR.Free;
    LS.Free;
    LFS.Free;
  end;
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  SaveToReg;

  { Send event to all registered TELEvent components }
  ELEventSender1.SendEvent(nil);
end;

procedure TForm3.FormShow(Sender: TObject);
begin
  FCancelFont.Assign(Memo1.Font);
  FCancelBackgroundColor := Memo1.Color;
  FCancelAutoOpenNewDoc := AutoOpenNewDoc;
  FCancelShowAnnotation := ShowAnnotation;
  FCancelMainFormBackColor := MainFormBackColor;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  FCancelFont.Free;
end;

procedure TForm3.Button3Click(Sender: TObject);
begin
  Memo1.Font.Assign(FCancelFont);
  ColorGrid1.ForegroundIndex := ColorGrid1.ColorToIndex(FCancelFont.Color);
  Memo1.Color := FCancelBackgroundColor;
  ColorGrid1.BackgroundIndex := ColorGrid1.ColorToIndex(FCancelBackgroundColor);
  CheckBox1.Checked := FCancelAutoOpenNewDoc;
  CheckBox2.Checked := FCancelShowAnnotation;
  ColorGrid2.BackgroundIndex := ColorGrid2.ColorToIndex(FCancelMainFormBackColor);
end;

{ TFontSaver }

constructor TFontSaver.Create(AOwner: TComponent);
begin
  inherited;
  FFont := TFont.Create;
end;

destructor TFontSaver.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TFontSaver.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

function TForm3.GetMainFormBackColor: TColor;
begin
  Result := ColorGrid2.BackgroundColor;
end;

end.
