{*******************************************************}
{File:      O8MainFrm.PAS                               }
{Revision:  0.03 / 29.10.1999                           }
{Comment:   NC OCI8 Demo: NC O8 Console, main form      }
{Copyright: (c) 1999, Dmitry Arefiev                    }
{Author:    Dmitry Arefiev, diman@ncom.ru               }
{*******************************************************}

unit O8MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, ActnList, NCOci, NCOciWrapper, NCOciDB, ToolWin, ImgList,
  Db, ExtCtrls, StdActns, O8ConsoleFrm;

type
  TNCO8ConMainFrm = class(TForm)
    StatusBar1: TStatusBar;
    dtbs: TOCIDatabase;
    strdprcOutPutGetLines: TOCIStoredProc;
    strdprcOutPutEnable: TOCIStoredProc;
    strdprcOutPutDisable: TOCIStoredProc;
    qrStat1: TOCIQuery;
    qrStat2: TOCIQuery;
    qrDelPlan: TOCIQuery;
    qrExplainPlan: TOCIQuery;
    qrGetPlan: TOCIQuery;
    CoolBar1: TCoolBar;
    tlbrDatabase: TToolBar;
    tlbttnLogin: TToolButton;
    tlbttnLogoff: TToolButton;
    ToolButton3: TToolButton;
    tlbttnStartTrans: TToolButton;
    tlbttnCommit: TToolButton;
    tlbttnRollback: TToolButton;
    tlbttnOutPut: TToolButton;
    tlbttnStat: TToolButton;
    mnmnMain: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    actnlstDatabase: TActionList;
    actnConnect: TAction;
    actnDisconnect: TAction;
    actnStartTran: TAction;
    actnCommit: TAction;
    actnRollback: TAction;
    actnOutPut: TAction;
    actnStat: TAction;
    actnAbout: TAction;
    actnExit: TAction;
    ToolBar1: TToolBar;
    tlbttnNew: TToolButton;
    tlbttnOpen: TToolButton;
    tlbttnSave: TToolButton;
    tlbttnSaveAll: TToolButton;
    N1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Save2: TMenuItem;
    SaveAll1: TMenuItem;
    Close1: TMenuItem;
    Window1: TMenuItem;
    actnNewDoc: TAction;
    actnOpenDoc: TAction;
    actnCloseDoc: TAction;
    actnSaveDoc: TAction;
    actnSaveAsDoc: TAction;
    actnSaveAllDoc: TAction;
    Cascade1: TMenuItem;
    TileHorizontally1: TMenuItem;
    TileVertically1: TMenuItem;
    Arrange1: TMenuItem;
    N2: TMenuItem;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    WindowCascade1: TWindowCascade;
    WindowTileHorizontal1: TWindowTileHorizontal;
    WindowTileVertical1: TWindowTileVertical;
    WindowArrange1: TWindowArrange;
    Panel1: TPanel;
    pgcntrlFiles: TPageControl;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    actnServerVersion: TAction;
    N3: TMenuItem;
    ServerVersion1: TMenuItem;
    imglst: TImageList;
    procedure actnConnectExecute(Sender: TObject);
    procedure actnDisconnectExecute(Sender: TObject);
    procedure actnExitExecute(Sender: TObject);
    procedure actnAboutExecute(Sender: TObject);
    procedure actnStartTranExecute(Sender: TObject);
    procedure actnCommitExecute(Sender: TObject);
    procedure actnRollbackExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actnStartTranUpdate(Sender: TObject);
    procedure actnOutPutExecute(Sender: TObject);
    procedure actnStatExecute(Sender: TObject);
    procedure actnFileNewExecute(Sender: TObject);
    procedure pgcntrlFilesChange(Sender: TObject);
    procedure actnOpenDocExecute(Sender: TObject);
    procedure actnCloseDocExecute(Sender: TObject);
    procedure actnSaveDocExecute(Sender: TObject);
    procedure actnSaveAsDocExecute(Sender: TObject);
    procedure actnSaveAllDocExecute(Sender: TObject);
    procedure actnDocUpdate(Sender: TObject);
    procedure actnServerVersionExecute(Sender: TObject);
  private
    { Private declarations }
    FPrevStat, FCurStat: TOCIQuery;
    FWinInitCount: Integer;
    function GetActConFile: TNCO8ConFile;
  public
    { Public declarations }
    procedure UpdateConnectedState;
    procedure GetOutput(AOutView: TListView);
    procedure GetError(AOutView: TListView; E: EOCINativeError);
    procedure GetStatistics(AStatView: TListView);
    procedure ExplainPlan(APlanView: TTreeView; ASQL, AMacros: TStrings);
    procedure ExecuteSQL(AOutView, AStatView: TListView;
        ASQL, AMacros: TStrings; AQuery: TOCIQuery);
    property ActiveConFile: TNCO8ConFile read GetActConFile;
    property PersistentWinItems: Integer read FWinInitCount;
  end;

var
  NCO8ConMainFrm: TNCO8ConMainFrm;

implementation

{$R *.DFM}

Uses O8AboutFrm;

procedure TNCO8ConMainFrm.FormCreate(Sender: TObject);
begin
    UpdateConnectedState;
    FWinInitCount := Window1.Count;
end;

procedure TNCO8ConMainFrm.actnExitExecute(Sender: TObject);
begin
    Close;
end;

procedure TNCO8ConMainFrm.actnAboutExecute(Sender: TObject);
begin
    TNCO8AboutFrm.Execute;
end;

procedure TNCO8ConMainFrm.actnStartTranUpdate(Sender: TObject);
begin
    (Sender as TAction).Enabled := dtbs.Connected;
end;

// ------------------------------------------------------------
// Connect / Disconnect

procedure TNCO8ConMainFrm.UpdateConnectedState;
begin
    actnConnect.Checked := dtbs.Connected;
    actnDisConnect.Checked := not dtbs.Connected;
    if dtbs.Connected then
        StatusBar1.Panels[0].Text := dtbs.UserName + '@' + dtbs.ServerName
    else
        StatusBar1.Panels[0].Text := 'Disconnected';
end;

procedure TNCO8ConMainFrm.actnConnectExecute(Sender: TObject);
begin
    try
        if dtbs.InteractiveOpen then begin
            strdprcOutPutGetLines.Prepare;
            strdprcOutPutEnable.Prepare;
            strdprcOutPutDisable.Prepare;
            actnOutPutExecute(nil);
            actnStatExecute(nil);
        end;
    finally
        UpdateConnectedState;
    end;
end;

procedure TNCO8ConMainFrm.actnDisconnectExecute(Sender: TObject);
begin
    try
        dtbs.Close;
    finally
        UpdateConnectedState;
    end;
end;

procedure TNCO8ConMainFrm.actnServerVersionExecute(Sender: TObject);
begin
    ShowMessage(dtbs.ServerVersion);
end;

// ------------------------------------------------------------
// Transaction

procedure TNCO8ConMainFrm.actnStartTranExecute(Sender: TObject);
begin
    if dtbs.InTransaction then
        dtbs.Commit
    else
        dtbs.StartTransaction;
end;

procedure TNCO8ConMainFrm.actnCommitExecute(Sender: TObject);
begin
    dtbs.Commit;
end;

procedure TNCO8ConMainFrm.actnRollbackExecute(Sender: TObject);
begin
    dtbs.Rollback;
end;

// ------------------------------------------------------------
// Output, Stat, Scan en/dis

procedure TNCO8ConMainFrm.actnOutPutExecute(Sender: TObject);
begin
    if Sender <> nil then
        actnOutPut.Checked := not actnOutPut.Checked;
    if actnOutPut.Checked then begin
        strdprcOutPutEnable.Params[0].AsInteger := 100000;
        strdprcOutPutEnable.ExecProc;
    end
    else
        strdprcOutPutDisable.ExecProc;
end;

procedure TNCO8ConMainFrm.actnStatExecute(Sender: TObject);
begin
    if Sender <> nil then
        actnStat.Checked := not actnStat.Checked;
    if actnStat.Checked then begin
        qrStat1.Open;
        FPrevStat := qrStat1;
        FCurStat := qrStat2;
    end
    else begin
        qrStat1.Close;
        qrStat2.Close;
    end;
end;

// ------------------------------------------------------------
// SQL

procedure AddMessage(AListView: TListView; AImgIndex: Integer; AMessage: String);
begin
    with AListView.Items.Add do begin
        ImageIndex := AImgIndex;
        Caption := '';
        SubItems.Add('');
        SubItems.Add('');
        SubItems.Add(AMessage);
    end;
end;

procedure TNCO8ConMainFrm.GetOutput(AOutView: TListView);
var
    i: Integer;
begin
    if actnOutPut.Checked then begin
        AOutView.Items.BeginUpdate;
        try
            with strdprcOutPutGetLines do
                repeat
                    Params[0].ArrayLen := 10;
                    Params[1].AsInteger := 10;
                    ExecProc;
                    for i := 0 to Params[1].AsInteger - 1 do
                        AddMessage(AOutView, 4, Params[0].AsStrings[i]);
                until Params[1].AsInteger <> 10;
        finally
            AOutView.Items.EndUpdate;
        end;
    end;
end;

procedure TNCO8ConMainFrm.GetError(AOutView: TListView; E: EOCINativeError);
var
    s, s1: String;
    fromI, toI, i: Integer;
begin
    s := E.Errors[0].Message;
    fromI := 1;
    i := 1;
    while i <= Length(s) do begin
        if s[i] in [#13, #10] then begin
            toI := i - 1;
            if (i < Length(s)) and (s[i + 1] in [#13, #10]) then
                Inc(i);
            s1 := Copy(s, fromI, toI - fromI + 1);
            if fromI = 1 then
                AddMessage(AOutView, 3, s1)
            else
                AddMessage(AOutView, -1, s1);
            fromI := i + 1;
        end;
        Inc(i);
    end;
end;

procedure TNCO8ConMainFrm.GetStatistics(AStatView: TListView);
var
    tmp: TOCIQuery;
begin
    AStatView.Items.BeginUpdate;
    try
        AStatView.Items.Clear;
        if (FCurStat = nil) or (FPrevStat = nil) or not actnStat.Checked then
            Exit;
        FCurStat.Open;
        FCurStat.First;
        FPrevStat.First;
        while not FCurStat.EOF do begin
            with AStatView.Items.Add do begin
                ImageIndex := 2;
                Caption := '';
                SubItems.Add(FCurStat.Fields[0].AsString);
                SubItems.Add(IntToStr(FCurStat.Fields[1].AsInteger - FPrevStat.Fields[1].AsInteger));
                SubItems.Add(IntToStr(FCurStat.Fields[1].AsInteger));
            end;
            FCurStat.Next;
            FPrevStat.Next;
        end;
        tmp := FCurStat;
        FCurStat := FPrevStat;
        FPrevStat := tmp;
        FCurStat.Close;
    finally
        AStatView.Items.EndUpdate;
    end;
end;

procedure TNCO8ConMainFrm.ExecuteSQL(AOutView, AStatView: TListView;
    ASQL, AMacros: TStrings; AQuery: TOCIQuery);
const
    stNm: array [TOCIStatementType] of String =
        ('Unknown', 'Select', 'Update', 'Delete', 'Insert',
         'Create', 'Drop', 'Alter', 'Begin', 'Declare');
var
    w1, w2: dword;
begin
    AOutView.Items.BeginUpdate;
    AQuery.DisableControls;
    try
        AOutView.Items.Clear;
        try
            AQuery.Macros.Assign(AMacros);
            AQuery.SQL.Assign(ASQL);
            w1 := GetTickCount;
            AQuery.Prepare;
            w1 := GetTickCount - w1;
            w2 := GetTickCount;
            if AQuery.StatementType = stSelect then
                AQuery.Open
            else
                AQuery.ExecSQL;
            w2 := GetTickCount - w2;
            AddMessage(AOutView, 1, stNm[AQuery.StatementType] + ' command parsed in ' +
                FloatToStr(w1 / 1000.0) + ' sec., executed in ' +
                FloatToStr(w2 / 1000.0) + ' sec.');
            if AQuery.StatementType in [stUpdate, stDelete, stInsert] then
                AddMessage(AOutView, 1, IntToStr(AQuery.RowsAffected) + ' rows ' +
                    LowerCase(stNm[AQuery.StatementType]) + 'd');
            GetStatistics(AStatView);
            GetOutput(AOutView);
        except
            on E: EOCINativeError do begin
                GetError(AOutView, E);
                raise;
            end;
        end;
    finally
        AOutView.Items.EndUpdate;
        AQuery.EnableControls;
    end;
end;

procedure TNCO8ConMainFrm.ExplainPlan(APlanView: TTreeView;
    ASQL, AMacros: TStrings);
const
    sid: String = 'NCOCI8';
var
    lst: TStringList;
    strm: TStringStream;
begin
    qrDelPlan.Params[0].AsString := sid;
    qrDelPlan.ExecSQL;
    qrExplainPlan.MacroByName('SID').AsString := '''' + sid + '''';
    with TOCIQuery.Create(nil) do
    try
        SQL.Assign(ASQL);
        Macros.Assign(AMacros);
        qrExplainPlan.MacroByName('STMT').AsString := Text;
    finally
        Free;
    end;
    qrExplainPlan.ExecSQL;
    qrGetPlan.Close;
    qrGetPlan.Params[0].AsString := sid;
    qrGetPlan.Open;
    lst := TStringList.Create;
    try
        while not qrGetPlan.EOF do begin
            lst.Add(qrGetPlan.FieldByName('plan_str').AsString);
            qrGetPlan.Next;
        end;
        strm := TStringStream.Create(lst.Text);
        APlanView.Items.BeginUpdate;
        try
            APlanView.LoadFromStream(strm);
            APlanView.FullExpand;
        finally
            strm.Free;
            APlanView.Items.EndUpdate;
        end;
    finally
        lst.Free;
    end;
end;

// ------------------------------------------------------------
// File

function TNCO8ConMainFrm.GetActConFile: TNCO8ConFile;
begin
    Result := (ActiveMDIChild as TNCO8ConChildFrm).ConFile;
end;

procedure TNCO8ConMainFrm.pgcntrlFilesChange(Sender: TObject);
begin
    if (pgcntrlFiles.ActivePage <> nil) then
        pgcntrlFiles.ActivePage.Action.Execute;
end;

procedure TNCO8ConMainFrm.actnFileNewExecute(Sender: TObject);
begin
    TNCO8ConFile.CreateFile;
end;

procedure TNCO8ConMainFrm.actnOpenDocExecute(Sender: TObject);
var
    i: Integer;
begin
    Application.ProcessMessages;
    if not OpenDialog1.Execute then
        Abort;
    with OpenDialog1.Files do
        for I := 0 to Count - 1 do
            with TNCO8ConChildFrm.Create(Application) do
                ConFile.ReadFile(Strings[I]);
end;

procedure TNCO8ConMainFrm.actnCloseDocExecute(Sender: TObject);
begin
    ActiveConFile.CloseFile;
end;

procedure TNCO8ConMainFrm.actnSaveDocExecute(Sender: TObject);
begin
    ActiveConFile.SaveFile;
end;

procedure TNCO8ConMainFrm.actnSaveAsDocExecute(Sender: TObject);
begin
    Application.ProcessMessages;
    SaveDialog1.FileName := ActiveConFile.FileName;
    if not SaveDialog1.Execute then
        Abort;
    with ActiveConFile do begin
        Rename(SaveDialog1.FileName);
        SaveFile;
    end;
end;

procedure TNCO8ConMainFrm.actnSaveAllDocExecute(Sender: TObject);
var
    i: Integer;
begin
    for i := 0 to MDIChildCount - 1 do
        (MDIChildren[i] as TNCO8ConChildFrm).ConFile.SaveFile;
end;

procedure TNCO8ConMainFrm.actnDocUpdate(Sender: TObject);
begin
    (Sender as TAction).Enabled := MDIChildCount > 0;
end;

end.
