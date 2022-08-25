unit OverbyteIcsPemtool2;

// Aug 2021 - never close window, only hide it
//            restore left and width

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, OverbyteIcsIniFiles;

type
  TfrmPemTool2 = class(TForm)
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Memo1DblClick(Sender: TObject);
  private
    FInitialized : Boolean;
  public
    { Public-Deklarationen }
  end;

var
  frmPemTool2: TfrmPemTool2;

implementation

{$R *.DFM}

uses
   OverbyteIcsPemTool1;

const
    SectionDisplayWindow = 'DisplayWindow';
    KeyTop               = 'Top';
    KeyLeft              = 'Left';
    KeyWidth             = 'Width';
    KeyHeight            = 'Height';

procedure TfrmPemTool2.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile := TIcsIniFile.Create(frmPemTool1.FIniFileName);
        try
            Width   := IniFile.ReadInteger(SectionDisplayWindow, KeyWidth,  Width);
            Height  := IniFile.ReadInteger(SectionDisplayWindow, KeyHeight, Height);
            Top     := IniFile.ReadInteger(SectionDisplayWindow, KeyTop, (Screen.Height - Height) div 2);     { V8.67 }
            Left    := IniFile.ReadInteger(SectionDisplayWindow, KeyLeft, (Screen.Width  - Width)  div 2);    { V8.67 }
        finally
            IniFile.Free;
        end;
    end;
end;

procedure TfrmPemTool2.Memo1DblClick(Sender: TObject);
begin
    Memo1.Lines.Clear;   { V8.67 }
    Hide;                { V8.67 }
end;

procedure TfrmPemTool2.FormClose(Sender: TObject; var Action: TCloseAction);
    var
    IniFile : TIcsIniFile;
begin
    Action := caHide;
    IniFile := TIcsIniFile.Create(frmPemTool1.FIniFileName);
    IniFile.WriteInteger(SectionDisplayWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionDisplayWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionDisplayWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionDisplayWindow, KeyHeight,      Height);
    IniFile.UpdateFile;
    IniFile.Free;
end;

end.
