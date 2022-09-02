{$A+,B-,D+,F+,G+,I+,K+,L+,N+,P+,Q-,R-,S+,T-,V+,W-,X+,Y+}
{$IFNDEF Win32}
 {$M 16384,8192}
{$ENDIF}
{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

unit Deldemo1;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, FvcBrows, StdCtrls, LowBrows, BTBase, BTIsBase, Filer,
  Restruct, ReIndex,
  ExtCtrls, Menus;

type

  GenName = IsamFileBlockName;

  ReXCallType = (ReXFirst, ReXPerform, ReXFinal);

  PersonRec = packed record
    LastName  : string [25];
    FirstName : string [20];
    Country   : string [3];
    Zip       : string [5];
    City      : string [30];
    Street    : string [30];
    Phone     : string [15];
  end;

  PPersonDef = ^PersonDef;
  PersonDef = packed record
    Dele : LongInt;
    Pers : PersonRec;
  end;

  TDemoForm = class(TForm)
    Timer1: TTimer;
    FvcBrowser1: TFvcBrowser;
    PopupMenu1: TPopupMenu;
    D1: TMenuItem;
    A1: TMenuItem;
    E1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    function FvcBrowser1BuildRow(Sender: TObject; var RR: RowRec): integer;
    procedure FormShow(Sender: TObject);
    function FvcBrowser1PerformFilter(Sender: TObject; var RR: RowRec;
      var UseIt: Boolean): Integer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FvcBrowser1RowChanged(Sender: TObject);
    procedure FvcBrowser1KeyPress(Sender: TObject; var Key: Char);
    procedure EditClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure AddClick(Sender: TObject);
    procedure HelpClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure SearchClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FilterClick(Sender: TObject);
  private
    { Private declarations }
    BH, BW:   word;                        { Beginning Height & Width of form }
    NetSupp:  Boolean;
    PersFB:   IsamFileBlockPtr;
    Person:   PersonDef;
    function  AddRecAndKeys(var Ref : LongInt): Boolean;
    procedure AddRecord;
    function  ChangeRecAndKeys(var OldPerson : PersonDef; Ref: LongInt): Boolean;
    function  CreateTheFile: Boolean;
    function  DeleteRecAndKeys(Ref : LongInt): Boolean;
    procedure DeleteRecord;
    procedure EditRecord;
    function  InitBrowser: Boolean;
    function  InitDataBase: Boolean;
    function  OpenTheFile: Boolean;
    function  RebuildTheFile: Boolean;
    procedure Search;
    function  SearchForKey(var Key    : IsamKeyStr;
                           var DatRef : LongInt ) : Boolean;
  public
    { Public declarations }
  end;

procedure IsamRebuildInfo(KeyNr: Integer; DatSNrR, DatSNrW: LongInt;
                          var DatS; Len: Word);

var
  DemoForm: TDemoForm;

implementation

{$R *.DFM}

uses DelDemo2, DelDemo3, DelDemo4, DelDemo5;

const
  TheFileBlockName = 'AdrBase';

var
  FBlock  : string;
  LockEnd : Boolean;

function PadCH (S : string; Ch : Char; Len : Byte): string;
var
  OldLength : integer;
begin
  if Len > Length(S) then begin
    OldLength := Length(S);
  {$IFDEF Win32}
    SetLength(S, Len);
  {$ELSE}
    S[0] := Chr(Len);
  {$ENDIF}
    FillChar(S[Succ(OldLength)], Len - OldLength, Ch);
  end;
  Result := S;
end;

function Trim(S: string): string;
var
  SLen : Integer;
  I    : Word;
begin
  SLen := Length(S);
  while (SLen > 0) and (S[SLen] <= ' ') do begin
    Dec(SLen);
  end;
{$IFDEF Win32}
  SetLength(S, SLen);
{$ELSE}
  S[0] := Char(SLen);
{$ENDIF}
  I := 1;
  while (I <= SLen) and (S[I] <= ' ') do begin
    Inc(I);
  end;
  if I > 1 then Delete(S, 1, Pred (I));
  Result := S;
end;

function IsLockError: Boolean;
begin
  Result := BTIsamErrorClass = 2;
end;

function LockAbort : Boolean;
begin
  Result := False;
  LockEnd := IsLockError;
  if not LockEnd then Exit;
  Result := (MessageDlg('Cannot get a lock.  Abort?',
    mtInformation, [mbYes, mbNo], 0) = mrYes);
end;

procedure DisplayRebuildInfo(DrName: GenName; CallType: ReXCallType;
                             KeyNr: Integer; DatSNrR, DatSNrW: LongInt;
                             var DatS; Len: Word); far;
begin
  case CallType of
    ReXFirst : begin
      StatForm.Show;
    end;
    ReXPerform : begin
      Application.ProcessMessages;
      if StatForm.ModalResult = mrAbort then
        IsamOK := False;
        if DatSNrR mod 50 = 0 then begin
          with StatForm do begin
            Label1.Caption := Format('Key number: %d', [KeyNr]);
            Label2.Caption := Format('Records read: %d', [DatSNrR]);
            Label3.Caption := Format('Records written: %d', [DatSNrW]);
          end;
        end;
    end;
    RexFinal : begin
      StatForm.Close;
    end;
  end;
end;

procedure StartRebuild(AName: IsamFileBlockName);
var
  Dummy : LongInt;
begin
  DisplayRebuildInfo(AName, ReXFirst, 0, 0, 0, Dummy, 0);
end;

procedure StopRebuild;
var
  Dummy : LongInt;
begin
  DisplayRebuildInfo('', ReXFinal, 0, 0, 0, Dummy, 0);
end;

procedure IsamRebuildInfo(KeyNr: Integer; DatSNrR, DatSNrW: LongInt;
                          var DatS; Len: Word);
begin
  DisplayRebuildInfo('', ReXPerform, KeyNr, DatSNrR,
                       DatSNrW, DatS, Len);
end;

procedure RebuildAndInform ( FBlName      : IsamFileBlockName;
                             DatSLen      : LongInt;
                             VarRec       : Boolean;
                             CharConvProc : ProcBTCharConvert;
                             NumberOfKeys : Word;
                             IID          : IsamIndDescr;
                             FuncBuildKey : Pointer);
var
  MyBuildKey : Reindex.FuncBuildKey absolute FuncBuildKey;
  Dummy : Boolean;
begin
  IsamReXUserProcPtr := IsamRebuildInfo;
  StartRebuild(FBlName);
  RestructFileBlock (FBlName, DatSLen, DatSLen, VarRec, 0,
      ChangeDatSNoChange, CharConvProc, nil);
  if IsamOK then begin
    ReIndexFileBlock ( FBlName, NumberOfKeys, IID, VarRec,
        MyBuildKey, True, Dummy,
        CharConvProc, nil );
  end;
  StopRebuild;
end;

function BuildPersKey (var P; KeyNr: Word): IsamKeyStr;
begin
  Result := '';
  with PersonDef(P).Pers do begin
    case KeyNr of
      1: Result := Uppercase(Format('%-20s%-10s', [Trim(LastName),
           Trim(FirstName)]));
      2: Result := Uppercase(Format('%-3s%5s', [Trim(Country),Trim(Zip)]));
    end;
  end;
end;

function AddData(P: PersonDef;var Ref: LongInt;
                 FB: IsamFileBlockPtr): Boolean;
begin
  Result := False;
  repeat
    BTAddRec(FB, Ref, P);
    if LockAbort then Exit;
  until not LockEnd;
  Result := IsamOK;
end;

function ChangeData(P: PersonDef; Ref: LongInt;
                    FB: IsamFileBlockPtr): Boolean;
begin
  Result := False;
  repeat
    BTPutRec(FB, Ref, P, False);
    if LockAbort then Exit;
  until not LockEnd;
  Result := IsamOK;
end;

function DeleteData(Ref: LongInt; FB: IsamFileBlockPtr): Boolean;
begin
  Result := False;
  repeat
    BTDeleteRec(FB, Ref);
    if LockAbort then Exit;
  until not LockEnd;
  Result := IsamOK;
end;

function AddaKey(K: IsamKeyStr; Ref: LongInt; KeyNr: Integer;
                 FB: IsamFileBlockPtr): Boolean;
begin
  Result := False;
  repeat
    BTAddKey(FB, KeyNr, Ref, K);
    if LockAbort then exit;
  until not LockEnd;
  Result := IsamOK;
end;

function DeleteaKey(K: IsamKeyStr; Ref: LongInt; KeyNr: Integer;
                    FB: IsamFileBlockPtr): Boolean;
begin
  Result := False;
  repeat
    BTDeleteKey (FB, KeyNr, Ref, K);
    if LockAbort then Exit;
  until not LockEnd;
  Result := IsamOK;
end;

function ChangeaKey(OldK, NewK: IsamKeyStr; Ref: LongInt; KeyNr: Integer;
                    FB: IsamFileBlockPtr): Boolean;
begin
  Result := True;
  if OldK = NewK then Exit;
  if AddaKey(NewK, Ref, KeyNr, FB) then begin
    if DeleteaKey(OldK, Ref, KeyNr, FB) then;
  end else begin
    Result := False;
  end;
end;

procedure TDemoForm.AddRecord;
var
  Ref : LongInt;
begin
  Timer1.Enabled := False;
  try
    FillChar(Person, SizeOf(Person), 0);
    with EditDlg do
    begin
      Caption := 'Add Record';
      with Person.Pers do
      begin
        FName.Text := FirstName;
        LName.Text := LastName;
        Cntry.Text := Country;
        ZipCode.Text := Zip;
        CityEdit.Text := City;
        StreetEdit.Text := Street;
        PhoneEdit.Text := Phone;
      end;
      ShowModal;
      if ModalResult=mrOK then
      begin
        with Person.Pers do
        begin
          FirstName := FName.Text;
          LastName := LName.Text;
          Country := Cntry.Text;
          Zip := ZipCode.Text;
          City := CityEdit.Text;
          Street := StreetEdit.Text;
          Phone := PhoneEdit.Text;
        end;
        if not AddRecAndKeys(Ref) then begin
          raise Exception.Create( 'Add operation failed');
        end else begin
          FvcBrowser1.SetAndUpDateBrowserScreen(BuildPersKey
            (Person, FvcBrowser1.KeyNumber), Ref);
        end;
      end;
    end;
  finally
    Timer1.Enabled := NetSupp;
  end;
end;

function TDemoForm.AddRecAndKeys(var Ref : LongInt): Boolean;
var
  Key : IsamKeyStr;
  OK  : Boolean;
begin
  Result := False;
  repeat
    BTLockFileBlock(PersFB);
    if not IsamOK then begin
      if MessageDlg('The files are in use.  Abort?',
        mtInformation, [mbYes, mbNo], 0) = mrYes then Exit;
    end;
  until IsamOK;
  OK := AddData(Person, Ref, PersFB);
  if OK then begin
    Key := BuildPersKey(Person, 1);
    OK := AddaKey(Key, Ref, 1, PersFB);
    if not OK then if DeleteData(Ref, PersFB) then;
  end;
  if OK then begin
    Key := BuildPersKey(Person, 2);
    OK := AddaKey(Key, Ref, 2, PersFB);
    if not OK then begin
      Key := BuildPersKey(Person, 1);                         {!!.56}
      if DeleteaKey (Key, Ref, 1, PersFB) then;
      if DeleteData (Ref, PersFB) then;
    end;
  end;
  BTUnLockFileBlock(PersFB);
  Result := IsamOK and OK;
end;

function TDemoForm.ChangeRecAndKeys(var OldPerson : PersonDef; Ref: LongInt): Boolean;
var
  CurrentPerson : PersonDef;
  Ok            : Boolean;

  function CompStruc(var S1, S2; Len: Word): integer;
  var
    A1 : array [1..$FFFF] of Byte absolute S1;
    A2 : array [1..$FFFF] of Byte absolute S2;
    I  : Word;
  begin
    for I := 1 to Len do begin
      if A1 [I] < A2 [I] then begin
        CompStruc := -1;
        Exit;
      end;
      if A1 [I] > A2 [I] then begin
        CompStruc := 1;
        Exit;
      end;
    end;
    CompStruc := 0;
  end;

begin
  Result := True;
  if CompStruc(Person, OldPerson, SizeOf(PersonDef)) = 0 then Exit;
  Result := False;
  repeat
    BTLockFileBlock(PersFB);
    if not IsamOK then begin
      if MessageDlg('The files are in use.  Abort?',
        mtInformation, [mbYes, mbNo], 0) = mrYes then Exit;
    end;
  until IsamOK;
  BTGetRec(PersFB, Ref, CurrentPerson, False);
  if not IsamOK or (CurrentPerson.Dele <> 0) then begin
    BTUnLockFileBlock(PersFB);
    Exit;
  end;
  if CompStruc (CurrentPerson, OldPerson, SizeOf (PersonDef)) <> 0
    then begin
    BTUnLockFileBlock(PersFB);
    ShowMessage('The record has changed in the meantime.');
    Exit;
  end;
  OK := ChangeaKey(BuildPersKey(CurrentPerson, 1),
      BuildPersKey(Person, 1), Ref, 1, PersFB);
  if OK then begin
    OK := ChangeaKey(BuildPersKey(CurrentPerson, 2),
      BuildPersKey(Person, 2), Ref, 2, PersFB);
    if not OK then begin
      if ChangeaKey(BuildPersKey(Person, 1),
          BuildPersKey(CurrentPerson, 1), Ref, 1, PersFB) then;
    end;
  end;
  if OK then begin
    OK := ChangeData(Person, Ref, PersFB);
    if not OK then begin
      if ChangeaKey(BuildPersKey(Person, 1),
          BuildPersKey(CurrentPerson, 1), Ref, 1, PersFB) then;
      if ChangeaKey(BuildPersKey(Person, 2),
          BuildPersKey(CurrentPerson, 2), Ref, 2, PersFB) then;
    end;
  end;
  BTUnLockFileBlock(PersFB);
  Result := IsamOK and OK;
end;

function TDemoForm.CreateTheFile: Boolean;
var
  IID : IsamIndDescr;
begin
  IID [1].Keyl := 30;
  IID [1].AllowDupK := False;
  IID [2].Keyl := 8;
  IID [2].AllowDupK := True;
  BTCreateFileBlock (FBlock, SizeOf(PersonDef), 2, IID);               {!!.52}
  Result := IsamOK;
end;

function TDemoForm.DeleteRecAndKeys(Ref : LongInt): Boolean;
var
  OK : Boolean;
begin
  Result := False;
  repeat
    BTLockFileBlock(PersFB);
    if not IsamOK then begin
      if MessageDlg('The files are in use.  Abort?',
        mtInformation, [mbYes, mbNo], 0) = mrYes then Exit;
      end;
  until IsamOK;
  OK := DeleteaKey(BuildPersKey(Person, 1), Ref, 1, PersFB);
  OK := DeleteaKey(BuildPersKey(Person, 2), Ref, 2, PersFB) and OK;
  OK := DeleteData(Ref, PersFB) and OK;
  BTUnLockFileBlock(PersFB);
  Result := IsamOK and OK;
end;

procedure TDemoForm.DeleteRecord;
var
  Match      : Boolean;
  SavePerson : PersonDef;
begin
  if FvcBrowser1.GetCurrentRec(Match) > 0 then Exit;
  if not Match then Exit;
  Timer1.Enabled := False;
  try
    SavePerson := Person;
    with EditDlg do
    begin
      Caption := 'Delete Record';
      with Person.Pers do
      begin
        FName.Text := FirstName;
        LName.Text := LastName;
        Cntry.Text := Country;
        ZipCode.Text := Zip;
        CityEdit.Text := City;
        StreetEdit.Text := Street;
        PhoneEdit.Text := Phone;
      end;
      ShowModal;
      if ModalResult=mrOK then
      begin
        Person := SavePerson;
        if not DeleteRecAndKeys(FvcBrowser1.GetCurrentDatRef) then begin
          raise Exception.Create( 'Delete operation failed');
        end else begin
          FvcBrowser1.UpDateBrowserScreen;
        end;
      end;
    end;
  finally
    Timer1.Enabled := NetSupp;
  end;
end;

procedure TDemoForm.EditRecord;
var
  Match : Boolean;
  SavePerson : PersonDef;
begin
  Timer1.Enabled := False;
  try
    if FvcBrowser1.GetCurrentRec(Match) = 0 then
    begin
      SavePerson := Person;
      with EditDlg do
      begin
        Caption := 'Edit Record';
        with Person.Pers do
        begin
          FName.Text := FirstName;
          LName.Text := LastName;
          Cntry.Text := Country;
          ZipCode.Text := Zip;
          CityEdit.Text := City;
          StreetEdit.Text := Street;
          PhoneEdit.Text := Phone;
        end;
        ShowModal;
        if ModalResult=mrOK then
        begin
          with Person.Pers do
          begin
            FirstName := FName.Text;
            LastName := LName.Text;
            Country := Cntry.Text;
            Zip := ZipCode.Text;
            City := CityEdit.Text;
            Street := StreetEdit.Text;
            Phone := PhoneEdit.Text;
          end;
          if not ChangeRecAndKeys(SavePerson, FvcBrowser1.GetCurrentDatRef) then
          begin
            raise Exception.Create( 'Modify operation failed');
          end else begin
            FvcBrowser1.SetAndUpDateBrowserScreen(BuildPersKey
              (Person, FvcBrowser1.KeyNumber), FvcBrowser1.GetCurrentDatRef );
          end;
        end;
      end;
    end;
  finally
    Timer1.Enabled := NetSupp;
  end;
end;

function TDemoForm.OpenTheFile: Boolean;
var
  OK,
  OK1 : Boolean;
begin
  OpenTheFile := False;
  OK := False;
  repeat
    { Assumes the fileblock is in the same directory as the exe }
    FBlock := ExtractFilePath(Application.ExeName) + TheFileBlockName;
    BTOpenFileBlock (PersFB, FBlock, False, False, False,
        True);
    OK := IsamOK;
    if not OK then begin
      OK1 := False;
      if IsamError = 10010 then begin
        if MessageDlg('Index file is defective.  Rebuild?',
          mtInformation, [mbYes, mbNo], 0) = mrYes then begin
            OK1 := RebuildTheFile;
        end else begin
          Exit;
        end;
      end else begin
        if IsamError = 9903 then begin
          if MessageDlg ('Data file does not exist. Create?',
            mtInformation, [mbYes, mbNo], 0) = mrYes then begin
              OK1 := CreateTheFile;
            if not OK1 then Exit;
          end else begin
            Exit;
          end;
        end else begin
          Exit;
        end;
      end;
    end;
  until OK;
  Result := True;
end;

function TDemoForm.RebuildTheFile: Boolean;
var
  IID : IsamIndDescr;
  I   : Integer;
begin
  IID [1].Keyl := 30;
  IID [1].AllowDupK := False;
  IID [2].Keyl := 8;
  IID [2].AllowDupK := True;
  RebuildAndInform(FBlock, SizeOf(PersonDef), False,
    BTNoCharConvert, 2, IID, @BuildPersKey);
  Result := IsamOK;
end;

procedure TDemoForm.Search;
var
  Key : IsamKeyStr;
  Ref : LongInt;
begin
  Timer1.Enabled := False;
  try
    FillChar(Person, SizeOf (Person), 0);
    with EditDlg do
    begin
      Caption := 'Search';
      with Person.Pers do
      begin
        FName.Text := FirstName;
        LName.Text := LastName;
        Cntry.Text := Country;
        ZipCode.Text := Zip;
        CityEdit.Text := City;
        StreetEdit.Text := Street;
        PhoneEdit.Text := Phone;
      end;
      ShowModal;
      if ModalResult=mrOK then
      begin
        with Person.Pers do
        begin
          FirstName := FName.Text;
          LastName := LName.Text;
          Country := Cntry.Text;
          Zip := ZipCode.Text;
          City := CityEdit.Text;
          Street := StreetEdit.Text;
          Phone := PhoneEdit.Text;
        end;
        Key := BuildPersKey(Person, 1);
        if not SearchForKey(Key, Ref) then begin
          raise Exception.Create( 'Search operation failed');
        end else begin
          FvcBrowser1.SetAndUpDateBrowserScreen(Key, Ref);
        end;
      end;
    end;
  finally
    Timer1.Enabled := NetSupp;
  end;
end;

function TDemoForm.SearchForKey(var Key    : IsamKeyStr;
                             var DatRef : LongInt ) : Boolean;
begin
  Result := False;
  repeat
    BTSearchKey(PersFB, 1, DatRef, Key);
    if LockAbort then Exit;
  until not LockEnd;
  Result := IsamOK;
End;

function TDemoForm.InitBrowser : Boolean;
begin
  FvcBrowser1.ConnectLowBrowser(New(PLowWinBrowser, Init(True, PersFB,
            1, 50, 1, '', '', Person, False )));
  FvcBrowser1.SetAndUpdateBrowserScreen('', 0);
  NetSupp := BTNetSupported <> NoNet;
  Timer1.Enabled := NetSupp;
end;

function TDemoForm.InitDataBase: Boolean;
var
  NetType  : NetSupportType;
  NoNetCom : Boolean;
begin
  Result := False;
  NoNetCom := BTNoNetCompiled;
  {-Ignore the error code}
  if NoNetCom then NetType := NoNet
  else begin
    NetTypeDlg.ShowModal;
    if NetTypeDlg.ModalResult = mrOk then
      with NetTypeDlg do begin
        if NoNetBtn.Checked then NetType := NoNet;
        if MsNetBtn.Checked then NetType := MsNet;
        if NovellBtn.Checked then NetType := Novell;
      end else Abort;
  end;
  BTInitIsam(NetType, 30);                                             {!!.52}
  if not IsamOK then Exit;
  if not OpenTheFile then Exit;
  Result := True;
end;

function TDemoForm.FvcBrowser1BuildRow(Sender: TObject; var RR: RowRec): integer;
begin
  Result := NoError;
  with Person.Pers do begin
    if RR.Status <> NoError then begin
      RR.Row := PadCH('****  ' + RR.IKS, #32, MaxCols);
    end else begin
      RR.Row := Copy(PadCH(LastName + ', ' + FirstName, #32, 30), 1, 30);
      RR.Row := PadCH(RR.Row
        + ' '+
        PadCH(Trim(Country) + '-' + Trim(Zip), #32, 9)
        + ' ' +
        PadCH(City, #32, 30)
        + ' '+
        PadCH(Street, #32, 30)
        + ' ' +
        PadCH(Phone, #32, 15), #32, MaxCols)
    end;
  end;
end;

procedure TDemoForm.FormShow(Sender: TObject);
begin
  FvcBrowser1.Header := PadCH (
    PadCH ( 'LastName' + ', ' + 'FirstName', #32, 30 )
    + ' ' + PadCH ( 'Ctry-Zip', #32, 9 )
    + ' ' + PadCH ( 'City', #32, 30 )
    + ' ' + PadCH ( 'Street', #32, 30 )
    + ' ' + PadCH ( 'Telephone', #32, 15 ), #32, MaxCols );
  if not InitDataBase then begin
    ShowMessage('Unable to open database');
    Application.Terminate;
  end;
  if not InitBrowser then Exit;
end;

function TDemoForm.FvcBrowser1PerformFilter(Sender: TObject;
  var RR: RowRec; var UseIt: Boolean): Integer;
begin
  UseIt := RR.IKS[1] = 'B';
  Result := NoError;
end;

procedure TDemoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  BTCloseFileBlock(PersFB);
  FvcBrowser1.FileblockHasClosed;                              {!!.54}
  BTExitIsam;
end;

procedure TDemoForm.FvcBrowser1RowChanged(Sender: TObject);
begin
  Panel3.Caption := 'Records: ' +
    IntToStr(BTUsedKeys(PersFB, FvcBrowser1.KeyNumber));
  Panel4.Caption := 'Task: ' + IntToStr(BTGetInternalDialogID(PersFB));
end;

procedure TDemoForm.FvcBrowser1KeyPress(Sender: TObject; var Key: Char);
begin
  { Overrides the default handling of a key press to force uppercase }
  Key := Upcase(Key);
end;

procedure TDemoForm.EditClick(Sender: TObject);
begin
  EditRecord;
end;

procedure TDemoForm.FormCreate(Sender: TObject);
begin
  BH := Height;
  BW := Width;
{$IFDEF Win32}
  Caption := 'Delphi B-Tree Filer Demo - 32-bit mode';
{$ELSE}
  Caption := 'Delphi B-Tree Filer Demo - 16-bit mode';
{$ENDIF}
end;

procedure TDemoForm.FormResize(Sender: TObject);
var
  AH, AW: Word;
  R: TRect;
  I: Integer;
begin
  AH := Height;
  AW := Width;

  { Resize browser when form resizes }
  R := FvcBrowser1.BoundsRect;
  R.Right := R.Right + AW - BW;
  R.Bottom := R.Bottom + AH - BH;
  FvcBrowser1.BoundsRect := R;

  { Update Height/Width for next time }
  BH := Height;
  BW := Width;
end;

procedure TDemoForm.DeleteClick(Sender: TObject);
begin
  DeleteRecord;
end;

procedure TDemoForm.AddClick(Sender: TObject);
begin
  AddRecord;
end;

procedure TDemoForm.HelpClick(Sender: TObject);
begin
  AboutBox.Showmodal;
end;

procedure TDemoForm.ExitClick(Sender: TObject);
begin
  Close;
end;

procedure TDemoForm.SearchClick(Sender: TObject);
begin
  Search;
end;

procedure TDemoForm.Timer1Timer(Sender: TObject);
begin
  FvcBrowser1.UpdateBrowserScreen;
end;

procedure TDemoForm.FilterClick(Sender: TObject);
begin
  FvcBrowser1.FilterEnabled := not(FvcBrowser1.FilterEnabled);
  if FvcBrowser1.FilterEnabled then
    Panel5.Font.Color := clBlack
  else Panel5.Font.Color := clGray;
end;

end.
