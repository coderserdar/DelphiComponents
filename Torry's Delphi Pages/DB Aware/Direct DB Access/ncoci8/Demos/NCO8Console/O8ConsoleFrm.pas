{*******************************************************}
{File:      O8ConsoleFrm.PAS                            }
{Revision:  0.03 / 29.10.1999                           }
{Comment:   NC OCI8 Demo: NC O8 Console, child form     }
{Copyright: (c) 1999, Dmitry Arefiev                    }
{Author:    Dmitry Arefiev, diman@ncom.ru               }
{*******************************************************}

unit O8ConsoleFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, NCOciDB, NCOci, NCOciWrapper, ComCtrls, ToolWin, Grids, DBGrids,
  ExtCtrls, StdCtrls, ImgList, ActnList, Menus, StdActns, DBCtrls;

type
  TNCO8ConChildFrm = class;

  TNCO8ConSource = (csFile, csDB);
  TNCO8ConFile = class(TAction)
  private
    FFileName: String;
    FNameUserGiven: Boolean;
    FComeFrom: TNCO8ConSource;
    FModified: Boolean;
    FFileMI: TMenuItem;
    FFileTS: TTabSheet;
    function GetFileWnd: TNCO8ConChildFrm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class procedure CreateFile;
    procedure Rename(ANewName: String);
    procedure ReadFile(AName: String);
    procedure SaveFile;
    procedure CanCloseFile;
    procedure CloseFile;
    function Execute: Boolean; override;
    procedure Activate;
    procedure DeActivate;
    procedure Changed;
    property FileMI: TMenuItem read FFileMI;
    property FileTS: TTabSheet read FFileTS;
    property FileWnd: TNCO8ConChildFrm read GetFileWnd;
    property FileName: String read FFileName write Rename; 
  end;

  TNCO8ConChildFrm = class(TForm)
    qrUser: TOCIQuery;
    DataSource1: TDataSource;
    tlbrSQL: TToolBar;
    CoolBar1: TCoolBar;
    pgcntrl: TPageControl;
    Splitter1: TSplitter;
    tbshtData: TTabSheet;
    tbshtPlan: TTabSheet;
    tlbttnRun: TToolButton;
    tbshtOutput: TTabSheet;
    ImageList2: TImageList;
    ToolButton1: TToolButton;
    tlbttnExplainPlan: TToolButton;
    tlbttnScan: TToolButton;
    TabSheet1: TTabSheet;
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    actnlstScript: TActionList;
    actnExecute: TAction;
    actnFindError: TAction;
    actnExplainPlan: TAction;
    actnScanSubst: TAction;
    MainMenu1: TMainMenu;
    SQL1: TMenuItem;
    Execute1: TMenuItem;
    Findnexterror1: TMenuItem;
    Explainplan1: TMenuItem;
    N2: TMenuItem;
    Scanformacros1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    mmMacro: TMemo;
    Panel3: TPanel;
    grdData: TDBGrid;
    Panel4: TPanel;
    lstvwOutput: TListView;
    Panel5: TPanel;
    lstvwStat: TListView;
    Panel6: TPanel;
    trvwPlan: TTreeView;
    ToolBar1: TToolBar;
    DBNavigator1: TDBNavigator;
    mmSQL: TMemo;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure actnDBUpdate(Sender: TObject);
    procedure actnExplainPlanExecute(Sender: TObject);
    procedure actnExecuteExecute(Sender: TObject);
    procedure actnScanSubstExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure mmSQLChange(Sender: TObject);
  private
    { Private declarations }
    FConFile: TNCO8ConFile;
  public
    { Public declarations }
    property ConFile: TNCO8ConFile read FConFile;
  end;

var
  NCO8ConChildFrm: TNCO8ConChildFrm;

implementation

{$R *.DFM}

Uses O8MainFrm;

// -----------------------------------------------------------------------
// -----------------------------------------------------------------------

var
  FUnNamedCount: Integer = 0;

constructor TNCO8ConFile.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FFileMI := TMenuItem.Create(nil);
    with FFileMI do begin
        Action := Self;
        with NCO8ConMainFrm.Window1 do begin
            Add(FFileMI);
            Visible := True;
        end;
    end;
    FFileTS := TTabSheet.Create(nil);
    with FFileTS do begin
        Action := Self;
        PageControl := NCO8ConMainFrm.pgcntrlFiles;
    end;
    Inc(FUnNamedCount);
    Rename('UnNamed' + IntToStr(FUnNamedCount));
    DisableIfNoHandler := False;
    Enabled := True;
    FNameUserGiven := False;
end;

destructor TNCO8ConFile.Destroy;
begin
    NCO8ConMainFrm.pgcntrlFiles.ActivePage := nil;
    FFileTS.Free;
    FFileMI.Free;
    with NCO8ConMainFrm do
        if Window1.Count = PersistentWinItems then
            Window1.Visible := False;
    inherited Destroy;
end;

class procedure TNCO8ConFile.CreateFile;
var
    frm: TForm;
begin
    if NCO8ConMainFrm.WindowState = wsMaximized then
        LockWindowUpdate(NCO8ConMainFrm.Handle);
    try
        frm := TNCO8ConChildFrm.Create(Application);
        if NCO8ConMainFrm.MDIChildCount = 1 then
            frm.WindowState := wsMaximized;
    finally
        if NCO8ConMainFrm.WindowState = wsMaximized then
            LockWindowUpdate(0);
    end;
end;

procedure TNCO8ConFile.Rename(ANewName: String);
begin
    FFileName := ANewName;
    FileWnd.Caption := ANewName;
    FileTS.Caption := ExtractFileName(ANewName);
    Caption := FileTS.Caption;
    FNameUserGiven := True;
end;

function TNCO8ConFile.Execute: Boolean;
begin
    if NCO8ConMainFrm.WindowState = wsMaximized then
        LockWindowUpdate(NCO8ConMainFrm.Handle);
    try
        FileWnd.Show;
    finally
        if NCO8ConMainFrm.WindowState = wsMaximized then
            LockWindowUpdate(0);
    end;
    Result := True;
end;

procedure TNCO8ConFile.Activate;
begin
    Checked := True;
    (FileTS.Parent as TPageControl).ActivePage := FileTS;
    with NCO8ConMainFrm.StatusBar1.Panels[1] do
        if FModified then
            Text := 'Modified'
        else
            Text := '';
end;

procedure TNCO8ConFile.DeActivate;
begin
    Checked := False;
    NCO8ConMainFrm.StatusBar1.Panels[1].Text := '';
end;

function TNCO8ConFile.GetFileWnd: TNCO8ConChildFrm;
begin
    Result := Owner as TNCO8ConChildFrm;
end;

procedure TNCO8ConFile.ReadFile(AName: String);
begin
    Application.ProcessMessages;
    FileWnd.mmSQL.Lines.LoadFromFile(AName);
    Rename(AName);
    FModified := False;
    NCO8ConMainFrm.StatusBar1.Panels[1].Text := '';
end;

procedure TNCO8ConFile.SaveFile;
begin
    Application.ProcessMessages;
    if not FNameUserGiven then
        NCO8ConMainFrm.actnSaveAsDocExecute(nil)
    else begin
        FileWnd.mmSQL.Lines.SaveToFile(FFileName);
        FModified := False;
        NCO8ConMainFrm.StatusBar1.Panels[1].Text := '';
    end;
end;

procedure TNCO8ConFile.Changed;
begin
    FModified := True;
    NCO8ConMainFrm.StatusBar1.Panels[1].Text := 'Modified';
end;

procedure TNCO8ConFile.CloseFile;
begin
    FileWnd.Close;
end;

procedure TNCO8ConFile.CanCloseFile;
begin
    Application.ProcessMessages;
    if FModified then
        case Application.MessageBox(
            PChar('Save changes to ' + FFileName + ' ?'),
            PChar('NC O8 Console'),
            MB_YESNOCANCEL + MB_ICONEXCLAMATION + MB_DEFBUTTON1) of
        IDYES: SaveFile;
        IDNO: ;
        IDCANCEL: Abort;
        end;
end;

// -----------------------------------------------------------------------
// -----------------------------------------------------------------------

procedure TNCO8ConChildFrm.FormCreate(Sender: TObject);
begin
    FConFile := TNCO8ConFile.Create(Self);
end;

procedure TNCO8ConChildFrm.FormDestroy(Sender: TObject);
begin
    if FConFile <> nil then begin
        FConFile.Free;
        FConFile := nil;
    end;
end;

procedure TNCO8ConChildFrm.FormActivate(Sender: TObject);
begin
    if FConFile <> nil then
        FConFile.Activate;
end;

procedure TNCO8ConChildFrm.FormDeactivate(Sender: TObject);
begin
    if FConFile <> nil then
        FConFile.DeActivate;
end;

procedure TNCO8ConChildFrm.mmSQLChange(Sender: TObject);
begin
    if FConFile <> nil then
        FConFile.Changed;
end;

procedure TNCO8ConChildFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
    if FConFile <> nil then
        FConFile.CanCloseFile;
    Action := caFree;
end;

procedure TNCO8ConChildFrm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if (Key = VK_PRIOR) and (Shift = [ssCtrl, ssShift]) then
        pgcntrl.Height := pgcntrl.Height + 20
    else if (Key = VK_NEXT) and (Shift = [ssCtrl, ssShift]) then
        pgcntrl.Height := pgcntrl.Height - 20
end;

// ------------------------------------------------------------
// Query execution

procedure TNCO8ConChildFrm.actnDBUpdate(Sender: TObject);
begin
    if actnFindError <> Sender then
        (Sender as TAction).Enabled := NCO8ConMainFrm.dtbs.Connected;
end;

procedure TNCO8ConChildFrm.actnExplainPlanExecute(Sender: TObject);
begin
    NCO8ConMainFrm.ExplainPlan(trvwPlan, mmSQL.Lines, mmMacro.Lines);
    pgcntrl.ActivePage := tbshtPlan;
end;

procedure TNCO8ConChildFrm.actnExecuteExecute(Sender: TObject);
begin
    try
        NCO8ConMainFrm.ExecuteSQL(lstvwOutput, lstvwStat, mmSQL.Lines,
            mmMacro.Lines, qrUser);
        if qrUser.StatementType = stSelect then
            pgcntrl.ActivePage := tbshtData
        else
            pgcntrl.ActivePage := tbshtOutput;
    except
        pgcntrl.ActivePage := tbshtOutput;
        raise;
    end;
end;

procedure TNCO8ConChildFrm.actnScanSubstExecute(Sender: TObject);
begin
    if Sender <> nil then
        actnScanSubst.Checked := not actnScanSubst.Checked;
    qrUser.MacroCheck := actnScanSubst.Checked;
end;

end.
