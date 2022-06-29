{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Mar 20, 1999
Description:  This program is used to scan an MBX file (Outlook Express) and
              search for subscription messages to one of F. Piette mailing
              lists. It extract EMail address from the subsject and add it
              to a DBF file if not already there. DBF file is automatically
              created if not found in same directory as exe file. You can
              select MBX file using an entry in INI file.
              I use this program to find new subscribers and send a message
              to them asking for subscription postcard.
              This program will not work with Delphi 1 because it uses
              32 bits features such as splitter bar and long strings.
Version:      1.00
EMail:        francois.piette@pophost.eunet.be
              francois.piette@rtfm.be             http://www.rtfm.be/fpiette
Support:      Unsupported code.
Legal issues: Copyright (C) 1999 by François PIETTE
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


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit MbxSub1;

{$IFDEF VER80}
    Bomb('Sorry, this program uses 32 bits features.');
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IniFiles, StdCtrls, ExtCtrls, MbxFile, Db, DBTables, Bde, Grids, DBGrids,
  DBCtrls, ComCtrls;

const
  WM_APPSTARTUP      = WM_USER + 1;

type
  TAppBaseForm = class(TForm)
    ToolsPanel: TPanel;
    MbxHandler1: TMbxHandler;
    ScanButton: TButton;
    EMailTable: TTable;
    PageControl1: TPageControl;
    ScanTabSheet: TTabSheet;
    DisplayMemo: TMemo;
    EMailMemo: TMemo;
    Splitter1: TSplitter;
    ViewTabSheet: TTabSheet;
    Panel1: TPanel;
    EMailDBNavigator: TDBNavigator;
    EMailDBGrid: TDBGrid;
    EMailDataSource: TDataSource;
    FindEdit: TEdit;
    SortByDateRadioButton: TRadioButton;
    SortByEmailRadioButton: TRadioButton;
    OpenDialog1: TOpenDialog;
    BrowseButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ScanButtonClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FindEditChange(Sender: TObject);
    procedure SortByDateRadioButtonClick(Sender: TObject);
    procedure SortByEmailRadioButtonClick(Sender: TObject);
    procedure BrowseButtonClick(Sender: TObject);
  private
    FIniFileName  : String;
    FInitialized  : Boolean;
    FDatabaseName : String;
    FTableName    : String;
    function Extract(Item : String) : String;
    procedure CreateDataTable;
    procedure PackTable(aTable : TTable);
    procedure WMAppStartup(var msg: TMessage); message WM_APPSTARTUP;
    procedure SelectIndex;
  public
    procedure Display(Msg : String);
    property  IniFileName : String read FIniFileName write FIniFileName;
  end;

function RenameToNumberedFile(From : String) : String;
function GetToken(pDelim : PCHar; Src : PChar; var Dst : String): PChar;

var
  AppBaseForm: TAppBaseForm;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';
    KeyMbxFile         = 'MbxFile';
    KeySplitter        = 'Splitter';
    TempFileName       = 'MbxSub.tmp';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.FormCreate(Sender: TObject);
begin
    FIniFileName  := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName  := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
    FDatabaseName := LowerCase(ExtractFilePath(Application.ExeName));
    FTableName    := 'subscribe.dbf';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        DisplayMemo.Width := IniFile.ReadInteger(SectionData, KeySplitter, Width div 2);
        MbxHandler1.FileName := IniFile.ReadString(SectionData, KeyMbxFile,
        'c:\Windows\Application Data\Microsoft\Outlook Express\Mail\Dossier24.mbx');
        IniFile.WriteString(SectionData, KeyMbxFile, MbxHandler1.FileName);
        IniFile.Destroy;
        DisplayMemo.Clear;
        EMailMemo.Clear;
        FindEdit.Clear;
        SortByEmailRadioButton.Checked := TRUE;
        PageControl1.ActivePage := ScanTabSheet;
        Caption := 'MbxSub - ' + ExtractFileName(MbxHandler1.FileName);
        PostMessage(Handle, WM_APPSTARTUP, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,      Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,     Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,    Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,   Height);
    IniFile.WriteInteger(SectionData,   KeySplitter, DisplayMemo.Width);
    IniFile.Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.WMAppStartup(var msg: TMessage);
var
    I       : Integer;
begin
    Update;
    EMailTable.DatabaseName := FDatabaseName;
    EMailTable.TableName    := FTableName;
    try
        EMailTable.Open;
    except
        on E:EDBEngineError do begin
            if E.Errors[0].ErrorCode = DBIERR_NOSUCHTABLE then begin
                Display('Missing datafile. Creating a new file.');
                CreateDataTable;
                EMailTable.Open;
            end
            else if E.Errors[0].ErrorCode = DBIERR_NOSUCHINDEX then begin
                Display('Missing index file. Creating new index file.');
                DeleteFile(FDatabaseName + TempFileName);
                RenameFile(FDatabaseName + FTableName, FDatabaseName + TempFileName);
                CreateDataTable;
                DeleteFile(FDatabaseName + FTableName);
                RenameFile(FDatabaseName + TempFileName, FDatabaseName + FTableName);
                PackTable(EMailTable);
                EMailTable.Open;
            end
            else if (E.Errors[0].Category = ERRCAT_DATACORRUPT) and
                    (E.ErrorCount > 1) and
                    (UpperCase(ExtractFileExt(E.Errors[1].Message)) = '.MDX') then begin
                Display('Corrupt index file. Rebuilding index file.');
                DeleteFile(FDatabaseName + TempFileName);
                RenameFile(FDatabaseName + FTableName, FDatabaseName + TempFileName);
                CreateDataTable;
                DeleteFile(FDatabaseName + FTableName);
                RenameFile(FDatabaseName + TempFileName, FDatabaseName + FTableName);
                PackTable(EMailTable);
                EMailTable.Open;
            end
            else if E.Errors[0].ErrorCode = DBIERR_HEADERCORRUPT then begin
                Display('Corrupt data file.');
                Display('Save corrupted file to: ''' +
                             RenameToNumberedFile(FDatabaseName +
                                                  FTableName) +
                             '''');
                Display('Creating new data file.');
                CreateDataTable;
                EMailTable.Open;
            end
            else begin
                Display(E.ClassName + ': ' + E.Message);
                for I := 0 to E.ErrorCount - 1 do
                    Display(IntToStr(E.Errors[I].ErrorCode) + '/' +
                            IntToStr(E.Errors[I].Category) +
                            ': ' + E.Errors[I].Message);
            end;
        end;
    end;
    EMailTable.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.Display(Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
//Fri, 19 Mar 1999 18:50:07 +0100
function ExtractDate(S : String) : TDateTime;
var
    P : PChar;
    Token : String;
    Year, Month, Day : Word;
begin
    P := PChar(S);
    if P = nil then begin
        Result := 0;
        Exit;
    end;
    // Get day of week
    P := GetToken(' ', P, Token);
    Token := LowerCase(Copy(Trim(Token), 1, 3));
    if not ((Token = 'mon') or (Token = 'tue') or
            (Token = 'wed') or (Token = 'thu') or
            (Token = 'fri') or (Token = 'sat') or (Token = 'sun')) then
        raise Exception.Create('Invalid day name: ' + S);

    // get day
    P := GetToken(' ', P, Token);
    Day := StrToInt(Trim(Token));

    // get month
    P := GetToken(' ', P, Token);
    Token := LowerCase(Trim(Token));
    if Token = 'jan' then
        Month := 1
    else if Token = 'feb' then
        Month := 2
    else if Token = 'mar' then
        Month := 3
    else if Token = 'apr' then
        Month := 4
    else if Token = 'may' then
        Month := 5
    else if Token = 'jun' then
        Month := 6
    else if Token = 'jul' then
        Month := 7
    else if Token = 'aug' then
        Month := 8
    else if Token = 'sep' then
        Month := 9
    else if Token = 'oct' then
        Month := 10
    else if Token = 'nov' then
        Month := 11
    else if Token = 'dec' then
        Month := 12
    else
        raise Exception.Create('Invalid month name: ' + S);

    // get year
    GetToken(' ', P, Token);
    Year := StrToInt(Trim(Token));

    Result := EncodeDate(Year, Month, Day);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.ScanButtonClick(Sender: TObject);
var
    Subject         : String;
    Token           : String;
    List            : String;
    EMail           : String;
    P               : PChar;
    MsgDate         : String;
    MsgCount        : Integer;
    NewCount        : Integer;
    FieldEMail      : TField;
    FieldSubDate    : TField;
//  FieldUnsDate    : TField;
//  FieldName       : TField;
begin
    Display('Scanning ' + MbxHandler1.FileName);
    PageControl1.ActivePage := ScanTabSheet;
    MsgCount                := 0;
    NewCount                := 0;
    EMailTable.Active       := TRUE;
    EMailTable.IndexName    := 'EMAIL';
    FieldEMail              := EMailTable.FieldByName('EMail');
    FieldSubDate            := EMailTable.FieldByName('SubDate');
//  FieldName               := EMailTable.FieldByName('Name');
//  FieldUnsDate            := EMailTable.FieldByName('UnsDate');

    MbxHandler1.Active := TRUE;
    MbxHandler1.First;
    while not MbxHandler1.Eof do begin
        Inc(MsgCount);
        Subject := Extract('Subject');
//        Display(Subject);
        P := PChar(Subject);
        P := GetToken(' ', P, Token);
        Token := LowerCase(Trim(Token));
        if Token = 'subscribe' then begin
            P := GetToken(' ', P, List);
            List := LowerCase(Trim(List));
            if (List = 'twsocket') or
               (List = 'twsocket-announce') or
               (List = 'midware') then begin
                GetToken(' ', P, EMail);
                EMail := LowerCase(Trim(EMail));
if Copy(Email, 1, 5) = 'napol' then
    MessageBeep(MB_OK);
                EMailTable.SetKey;
                FieldEMail.AsString := EMail;
                EMailTable.GotoNearest;
                if FieldEMail.AsString <> EMail then begin
                    // Do not exists yet, will create
                    Inc(NewCount);
                    MsgDate := Extract('Date');
                    Display(List + ' ' + EMail);
                    EMailMemo.Lines.Add(EMail + ';');
                    EMailTable.Append;
                    FieldEMail.AsString := EMail;
                    FieldSubDate.AsString := FormatDateTime('YYYYMMDD', ExtractDate(MsgDate));
                    EMailTable.Post;
                end;
            end;
        end;
        MbxHandler1.Next;
    end;
    MbxHandler1.Active := FALSE;
    EMailTable.Active  := FALSE;
    Display('Done ' + IntToStr(MsgCount) + '/' + IntToStr(NewCount));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TAppBaseForm.Extract(Item : String) : String;
var
    P, Q, R : PChar;
begin
    Result := '';
    P := MbxHandler1.MsgStream.Memory;
    Q := StrPos(P, PChar(#13#10 + Item + ': '));
    if Q <> nil then begin
        R := StrPos(Q + 2, #13#10);
        if R > Q then begin
            SetLength(Result, R - Q - 2 - Length(Item) - 2);
            if Length(Result) > 0 then
                Move(Q[2 + Length(Item) + 2], Result[1], Length(Result));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.PackTable(aTable : TTable);
var
    Status : Integer;
begin
    aTable.Active    := FALSE;
    aTable.Exclusive := TRUE;
    aTable.Active    := TRUE;

    Status := DbiPackTable(aTable.DataBase.Handle,
                           aTable.Handle,
                           nil, nil, TRUE);

    if Status <> DBIERR_NONE then
        DbiError(Status);
    aTable.Active := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.CreateDataTable;
var
    Table : TTable;
begin
    Table := TTable.Create(nil);
    try
        Table.TableType    := ttDBase;
        Table.DatabaseName := FDatabaseName;
        Table.TableName    := FTableName;
        with Table.FieldDefs do begin
            Clear;

            Add('SubDate',    ftString, 8,                              FALSE);
            Add('UnsDate',    ftString, 8,                              FALSE);
            Add('EMail',      ftString, 64,                             FALSE);
            Add('Name',       ftString, 64,                             FALSE);
        end;

        // A bug in D3 prevent us from defining the indexes before calling
        // CreateTable. We will just add the indexes after creation.
        Table.CreateTable;
        Table.AddIndex('EMail',   'EMAIL+SUBDATE', [ixExpression]);
        Table.AddIndex('SubDate', 'SUBDATE', []);

    finally
        Table.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Replace any existing file extension, or add an extension is none exists.
// The extension is a 3 digits number, with leading zeros, build to make
// it unique. Numbering start at 001 and increment until an unused number
// is found. If 1000 is reached, then an exception will be triggered.
function RenameToNumberedFile(From : String) : String;
var
    FPath                : String;
    FDir                 : String;
    FName                : String;
    FExt                 : String;
    FBaseName            : String;
    FileHandle           : DWORD;
    Count                : Integer;
begin
    FExt  := ExtractFileExt(From);
    FName := Copy(From, 1, Length(From) - Length(FExt));
    FName := ExtractFileName(FName);
    FDir  := ExtractFilePath(From);
    if FDir[Length(FDir)] <> '\' then
        FDir := FDir + '\';

    Count := 1;
    while TRUE do begin
        FBaseName := FName + '.' + Format('%3.3d', [Count]);
        FPath     := FDir + FBaseName;
        FileHandle := CreateFile(PChar(FPath),
                                 GENERIC_READ or GENERIC_WRITE,
                                 0,                                // ShareMode
                                 nil,                              // SecurityAttributes
                                 OPEN_EXISTING,
                                 FILE_ATTRIBUTE_NORMAL,
                                 0);                               // TemplateFile
        if FileHandle = INVALID_HANDLE_VALUE then begin
            RenameFile(From, FPath);
            Result := FPath;
            Exit;
        end;
        // File exists, close it and continue
        Windows.CloseHandle(FileHandle);

        // Be sure to not loop forever here !
        Inc(Count);
        if Count >= 1000 then
            raise Exception.Create('RenameToNumberedFile failed');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetToken(pDelim : PChar; Src : PChar; var Dst : String): PChar;
var
    FldSep  : Char;
    RecSep  : Char;
begin
    Dst    := '';
    if Src = nil then begin
        Result := nil;
        Exit;
    end;

    FldSep := pDelim[0];
    RecSep := pDelim[1];
    Result := Src;

    while (Result^ <> FldSep) and (Result^ <> RecSep) do begin
        Dst := Dst + Result^;
        Inc(Result);
    end;
    Inc(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.PageControl1Change(Sender: TObject);
begin
    if PageControl1.ActivePage = ViewTabSheet then begin
        EMailTable.Active := TRUE;
        SelectIndex;
    end
    else
        EMailTable.Active := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.SelectIndex;
begin
    if SortByEmailRadioButton.Checked then
        EMailTable.IndexName := 'EMAIL'
    else
        EMailTable.IndexName := 'SUBDATE';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.FindEditChange(Sender: TObject);
var
    FldName : String;
begin
    if not EMailTable.Active then
        Exit;
    SelectIndex;
    if SortByEmailRadioButton.Checked then
        FldName := 'EMAIL'
    else
        FldName := 'SUBDATE';
    EMailTable.SetKey;
    EMailTable.FieldByName(FldName).AsString := LowerCase(Trim(FindEdit.Text));
    EMailTable.GotoNearest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.SortByDateRadioButtonClick(Sender: TObject);
begin
    SelectIndex;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.SortByEmailRadioButtonClick(Sender: TObject);
begin
    SelectIndex;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.BrowseButtonClick(Sender: TObject);
var
    IniFile         : TIniFile;
begin
    OpenDialog1.DefaultExt := 'mbx';
    OpenDialog1.InitialDir := ExtractFilePath(MbxHandler1.FileName);
    OpenDialog1.FileName   := ExtractFileName(MbxHandler1.FileName);
    OpenDialog1.Filter     := 'Mailbox files (*.mbx)|*.mbx|All files (*.*)|*.*';
    if not OpenDialog1.Execute then
        Exit;
    MbxHandler1.Close;
    MbxHandler1.FileName := OpenDialog1.FileName;
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteString(SectionData, KeyMbxFile, MbxHandler1.FileName);
    IniFile.Free;
    Caption := 'MbxSub - ' + ExtractFileName(MbxHandler1.FileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

