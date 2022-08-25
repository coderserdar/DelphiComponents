 unit OverbyteIcsMailQuTstdiag;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Menus, RichEdit, OverbyteIcsIniFiles ;

type
  TDiagForm = class(TForm)
    TraceBox: TRichEdit;
    PopupMenu: TPopupMenu;
    mCopy: TMenuItem;
    mPaste: TMenuItem;
    mSelAll: TMenuItem;
    mCopyAll: TMenuItem;
    procedure mCopyClick(Sender: TObject);
    procedure mPasteClick(Sender: TObject);
    procedure mSelAllClick(Sender: TObject);
    procedure mCopyAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DiagForm: TDiagForm;

implementation

{$R *.DFM}

Uses OverbyteIcsMailQuTst1 ;

procedure TDiagForm.mCopyClick(Sender: TObject);
begin
    TraceBox.CopyToClipboard;
end;

procedure TDiagForm.mPasteClick(Sender: TObject);
begin
    TraceBox.PasteFromClipboard ;
end;

procedure TDiagForm.mSelAllClick(Sender: TObject);
begin
    TraceBox.SelectAll ;
end;

procedure TDiagForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
    section: string ;
begin
    DiagWinFlag := false ;
    IniFile := TIcsIniFile.Create(FIniFileName);
    with IniFile do
    begin
        section := 'DiagWindow' ;
        WriteInteger (section, 'Top', Top);
        WriteInteger (section, 'Left', Left);
        WriteInteger (section, 'Width', Width);
        WriteInteger (section, 'Height', Height);
    end ;
    IniFile.UpdateFile;
    IniFile.Free;
end;

procedure TDiagForm.FormCreate(Sender: TObject);
var
    IniFile : TIcsIniFile;
    section: string ;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    with IniFile do
    begin
        section := 'DiagWindow' ;
        Top := ReadInteger (section, 'Top', (Screen.Height - Height) div 2);
        Left := ReadInteger (section, 'Left', (Screen.Width - Width) div 2);
        Width := ReadInteger (section, 'Width', Width);
        Height := ReadInteger (section, 'Height', Height);
    end;
    IniFile.Free;
    DiagWinFlag := true ;
end;

procedure TDiagForm.FormDestroy(Sender: TObject);
begin
    DiagWinFlag := false ;
end;

procedure TDiagForm.mCopyAllClick(Sender: TObject);
begin
    TraceBox.SelectAll ;
    TraceBox.CopyToClipboard;
end;

end.
