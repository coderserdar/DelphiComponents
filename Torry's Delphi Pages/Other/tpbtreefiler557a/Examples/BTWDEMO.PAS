{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

{--Conditional defines and compiler options that affect this program}
  {$I BRDefOpt.Inc}

{$M 16384,8192}
{$F+,X+}

Program BTWDemo;

 {$R BTWDemo}


Uses
{$IFDEF VER80}                                                  {!!.51}
  Messages,                                                     {!!.51}
{$ENDIF}                                                        {!!.51}
  WinTypes,
  WinProcs,
{$IFDEF Ver10}
  WObjects,
{$ELSE}
 {$IFDEF Ver15}
  WObjects,
 {$ELSE}
  {$IFNDEF Ver80}
  Objects,
  {$ENDIF}
  OWindows,
  ODialogs,
 {$ENDIF}
{$ENDIF}
  Strings,
  BTBase, BTIsBase,
  Filer,
  Rebuild,
  IsamTool,
  LowBrows,
  WBrowser;

Const
  id_NetSingle = 101;
  id_NetMSNet = 102;
  id_NetNovell = 103;
  idm_About = 201;
  idm_New = 202;
  idm_Delete = 203;
  idm_Search = 204;
  idm_Edit = 205;
  idm_FilterOnOff = 206;
  idm_Status = 207;

  TheFileBlockName = 'WAddress';

Type
  PDemoApp = ^TDemoApp;
  TDemoApp = Object (TApplication)
    Constructor Init ( AName : PChar );
    Destructor Done; Virtual;
    Function InitDataBase : Boolean;
    Function InitBrowser : Boolean;
    Procedure InitMainWindow; Virtual;
  End;

  PDemoWin = ^TDemoWin;
  TDemoWin = Object ( TBrowserWindow )
    FilterOn : Boolean;

    Constructor InitDemo ( AParent             : PWindowsObject;
                       ATitle              : PChar;
                       ADrvOrFileBlockPtr  : Pointer;
                       ANumberOfEltsPerRow : Word;
                       ANumberOfRows       : Word;
                       AKeyNr              : Word;
                       ALKey,
                       AHKey               : GenKeyStr );
    Destructor Done; Virtual;
    Function BuildRow ( Var RR : RowRec ) : Integer; Virtual;
    Function PerformFilter ( Var RR    : RowRec;
                             Var UseIt : Boolean ) : Integer; Virtual;
    Procedure ShowErrorOccured ( EClass : Integer ); Virtual;      {!!.51}
    Procedure GetWindowClass ( Var WndClass : TWndClass ); Virtual;
    Function EditDataRec ( ATitle : PChar ) : Boolean;
    Procedure ProcessAbout ( Var Msg : TMessage ); Virtual
        cm_First + idm_About;
    Procedure NewRec ( Var Msg : TMessage ); Virtual
        cm_First + idm_New;
    Procedure DelRec ( Var Msg : TMessage ); Virtual
        cm_First + idm_Delete;
    Procedure EditRec ( Var Msg : TMessage ); Virtual
        cm_First + idm_Edit;
    Procedure Search ( Var Msg : TMessage ); Virtual
        cm_First + idm_Search;
    Procedure FilterOnOff ( Var Msg : TMessage ); Virtual
        cm_First + idm_FilterOnOff;
    Procedure ShowStatus ( Var Msg : TMessage ); Virtual
        cm_First + idm_Status;
    procedure WMQueryEndSession(var Msg: TMessage); Virtual
        wm_First + wm_QueryEndSession;
    procedure LButtonDblClk(var Msg: TMessage); Virtual
        wm_First + wm_LButtonDblClk;
    procedure wmSize(var Msg : TMessage); virtual     {!!.42}
        wm_First+wm_Size;
  End;

  PEditDialog = ^TEditDialog;
  TEditDialog = Object ( TDialog )
    WName : array[0..20] of Char;     {!!.24}
    Constructor Init ( AParent : PWindowsObject; AName : PChar );
    procedure SetupWindow; virtual;   {!!.24}
  End;

  PNetDialog = ^TNetDialog;
  TNetDialog = Object ( TDialog )
    Constructor Init ( AParent : PWindowsObject; AName : PChar );
    Procedure ButtonSingle ( Var Msg : TMessage ); Virtual
        id_First + id_NetSingle;
    Procedure ButtonMsNet ( Var Msg : TMessage ); Virtual
        id_First + id_NetMsNet;
    Procedure ButtonNovell ( Var Msg : TMessage ); Virtual
        id_First + id_NetNovell;
  End;

  PersonRec = Record
    LastName    : Array [0..15] Of Char;
    FirstName   : Array [0..15] Of Char;
    Address     : Array [0..25] Of Char;
    City        : Array [0..20] Of Char;
    State       : Array [0..2] of Char;
    Zip         : Array [0..10] Of Char;
    Phone       : Array [0..14] Of Char;
  End;

  PersonDef = Record
    Dele : LongInt;
    Pers : Personrec;
  End;

Var
  Person  : PersonDef;
  BRHeader   : BRLRowEltString;
  PersFB  : IsamFileBlockPtr;
  DemoApp : TDemoApp;


  Procedure DbError ( S : PChar );
  var
    S1 : array [0..255] of char;
    S2 : array [0..10] of char;
  Begin
    Str(IsamError, S2);
    StrCopy(S1, S);
    StrCat(S1, ', code: ');
    StrCat(S1, S2);
    MessageBox ( GetActiveWindow, S1, 'Data Base Error',
        MB_OK Or mb_TaskModal Or mb_SystemModal );
  End;

  Procedure AutoErrorDisplay;

  Var
    CharAr : Array [0..255] Of Char;

  Begin
    If IsamError <> 0 Then Begin
      StrPCopy ( CharAr, IsamErrorMessage ( IsamError ) );
      DbError ( CharAr );
    End;
  End;

  Function YesNo ( S : String ) : Boolean;

  Var
    SZ : Array [0..255] Of Char;

  Begin
    StrPCopy ( SZ, S + '?' );
    YesNo := MessageBox ( GetActiveWindow, SZ, 'User Question',
        mb_YesNo Or mb_IconHand Or mb_SystemModal ) = id_Yes;
  End;

  Function OKBox ( S : String ) : Boolean;

  Var
    SZ : Array [0..255] Of Char;

  Begin
    StrPCopy ( SZ, S + '!' );
    okbOX := MessageBox ( GetActiveWindow, SZ, 'User Warning',
        mb_OK Or mb_IconExclamation Or mb_SystemModal ) = id_Ok;
  End;


  Function Extend ( S : String; Nr : Byte) : String;

  Var
    S1 : String;

  Begin
    S1 := S;
    While Length (S1) < Nr Do Begin
      S1 := S1 + ' ';
    End;
    Extend := S1;
  End;


  Function BuildPersKey ( Var P; KeyNr : Word ) : IsamKeyStr;


    Function StrToKey ( S : String ) : String;

    Var
      S1   : String;
      I, L : Word;


    Begin
      S1 [0] := S [0];
      L := Length (S);
      For I := 1 To L Do Begin
        Case S [I] Of
          'a'..'z' : S1 [I] := UpCase (S [I]);
          'ä' : S1 [I] := 'Ä';
          'ö' : S1 [I] := 'Ö';
          'ü' : S1 [I] := 'Ü';
          'ß' : S1 [I] := 'S';
        Else S1 [I] := S [I];
        End; {Case}
      End;
      StrToKey := S1;
    End;


  Begin
    BuildPersKey := '';
    With PersonDef (P).Pers Do Begin
      Case KeyNr Of
        1: BuildPersKey := Copy ( StrToKey (StrPas (LastName) ) , 1, 15 );
      End;
    End;
  End;


  Constructor TEditDialog.Init ( AParent : PWindowsObject;
                                 AName   : PChar);


    Procedure InitChildren;

    Var
      S : PEdit;
      P : PButton;

    Begin
      TransferBuffer := @Person.Pers;
      With Person.Pers Do Begin
        New ( S, InitResource ( @Self, 300, SizeOf (LastName) ) );
        New ( S, InitResource ( @Self, 301, SizeOf (FirstName) ) );
        New ( S, InitResource ( @Self, 302, SizeOf (Address) ) );
        New ( S, InitResource ( @Self, 303, SizeOf (City) ) );
        New ( S, InitResource ( @Self, 304, SizeOf (State) ) );
        New ( S, InitResource ( @Self, 305, SizeOf (Zip) ) );
        New ( S, InitResource ( @Self, 306, SizeOf (Phone) ) );
        New ( P, InitResource ( @Self, id_OK ) );
        New ( P, InitResource ( @Self, id_Cancel ) );
      End;
    End;


  Begin
    TDialog.Init ( AParent, AName );
    InitChildren;
  End;


  procedure TEditDialog.SetupWindow;  {!!.24}
  begin
    TDialog.SetupWindow;
    SetWindowText ( HWindow, WName);
  end;


  Constructor TNetDialog.Init ( AParent : PWindowsObject; AName : PChar );


    Procedure InitChildren;

    Var
      P : PButton;

    Begin
      New ( P, InitResource ( @Self, id_NetSingle ) );
      New ( P, InitResource ( @Self, id_NetMSNet ) );
      New ( P, InitResource ( @Self, id_NetNovell ) );
    End;


  Begin
    TDialog.Init ( AParent, AName );
    InitChildren;
  End;


  Procedure TNetDialog.ButtonSingle ( Var Msg : TMessage );

  Begin
    EndDlg ( id_NetSingle );
  End;


  Procedure TNetDialog.ButtonMsNet ( Var Msg : TMessage );

  Begin
    EndDlg ( id_NetMSNet );
  End;


  Procedure TNetDialog.ButtonNovell ( Var Msg : TMessage );

  Begin
    EndDlg ( id_NetNovell );
  End;

  Constructor TDemoWin.InitDemo ( AParent             : PWindowsObject;
                              ATitle              : PChar;
                              ADrvOrFileBlockPtr  : Pointer;
                              ANumberOfEltsPerRow : Word;
                              ANumberOfRows       : Word;
                              AKeyNr              : Word;
                              ALKey,
                              AHKey               : GenKeyStr );
  Begin
    If Not TBrowserWindow.Init( AParent, ATitle ) Then Fail;
    FilterOn := False;
    If Not ConnectLowBrowser (
        New ( PLowWinBrowser, Init ( True, ADrvOrFileBlockPtr,
                                     ANumberOfEltsPerRow,
                                     ANumberOfRows,
                                     AKeyNr,
                                     ALKey,
                                     AHKey,
                                     Person, False )),
                              BRHeader, '') Then Begin
      Fail;
    End;
  End;

  Destructor TDemoWin.Done;

  Begin
   If BTNetSupported <> NoNet Then
     KillTimer ( HWindow, 1 );
    TBrowserWindow.Done;
  End;


  Function TDemoWin.BuildRow ( Var RR : RowRec ) : Integer;

  Begin
    BuildRow := NoError;
    With Person.Pers Do Begin
      If RR.Status <> NoError Then Begin
        RR.Row := Extend ( '****  ' + RR.IKS, MaxCols );
      End Else Begin
        RR.Row := Extend (
            Extend ( StrPas ( LastName ) + ', ' + StrPas ( FirstName ), 30 )
            + ' '+
            Extend ( StrPas ( Address ), 25 )
            + ' '+
            Extend ( StrPas ( City ), 15 )
            + ' ' +
            Extend ( StrPas ( State), 2 )
            + ' ' +
            Extend ( StrPas ( Zip ), 10 )
            + ' ' +
            Extend ( StrPas ( Phone ), 12 ), MaxCols )
      End;
    End;
  End;

  Function TDemoWin.PerformFilter ( Var RR    : RowRec;
                                    Var UseIt : Boolean ) : Integer;
  Begin
    PerformFilter := NoError;
    UseIt := True;
    If FilterOn Then UseIt := (Length ( RR.IKS ) > 8);
  End;

  Procedure TDemoWin.ShowErrorOccured ( EClass : Integer );     {!!.51}

  Begin
    MessageBeep ( 0 );
    If EClass > 1 Then Begin                                    {!!.51}
      If YesNo ( 'Abort program' ) Then Begin
        Halt;
      End Else Begin
        SetSuppressTimer ( True );
          {-Suspend timer events until a next action is done}
      End;
    End;
  End;

  Function TDemoWin.EditDataRec ( ATitle : PChar ) : Boolean;

  Var
    Dialog : PEditDialog;

  Begin
    Dialog := New ( PEditDialog, Init ( @Self, 'BTWINDEMO_DIA' ) );
    StrCopy(Dialog^.WName, ATitle);                {!!.24}
    {SetWindowText ( Dialog^.HWindow, ATitle );}   {!!.24}
    EditDataRec := Application^.ExecDialog ( Dialog ) = id_OK;
  End;


  Procedure TDemoWin.ProcessAbout ( Var Msg: TMessage );

  Begin
    MessageBox ( HWindow, 'B-Tree Filer Demo for Windows' + #13#10 +
        'Copyright (c) Enz EDV-Beratung GmbH 1986-91' + #13#10 +
        'Modified and used under license by TurboPower Software.',
        'About', mb_OK );
  End;

  procedure TDemoWin.LButtonDblClk(var Msg: TMessage);
  begin
    EditRec(Msg);
  end;

  {$I BTWStuff.Inc}


  Procedure TDemoWin.FilterOnOff( Var Msg: TMessage);

  Begin
    FilterOn := Not FilterOn;
    UpDateBrowserScreen;
  End;


  Procedure TDemoWin.ShowStatus ( Var Msg: TMessage);

  Var
    LResult : Integer;                                          {!!.51}
    UK     : LongInt;
    S, T   : String;
    Buffer : Array [0..255] Of Char;

  Begin
    SetSuppressTimer ( True );
    UK := BTUsedKeys ( PersFB, GetCurrentKeyNr );
    LResult := BTIsamErrorClass;                                {!!.51}
    If LResult = NoError Then Begin                             {!!.51}
      Str ( UK : 0, S );
      Str ( BTGetInternalDialogID(PersFB): 0, T ); {!!}
      S := 'Records : ' + S + '    Task : ' + T;
      StrPCopy ( Buffer, S );
      MessageBox ( GetActiveWindow, @Buffer,'Status',
          mb_OK Or mb_TaskModal );
    End;
    SetSuppressTimer( False );
  End;

  procedure TDemoWin.WMQueryEndSession(var Msg: TMessage);

  Begin
    DbError ( 'First close the database application' );
    DefWndProc ( Msg );
    msg.result := 0;
  End;

  Procedure TDemoWin.GetWindowClass ( Var WndClass : TWndClass );

  Begin
    TWindow.GetWindowClass ( WndClass );
    WndClass.Style := cs_DblClks;
    WndClass.hIcon := LoadIcon ( hInstance, 'BTWINDEMO_ICON' );
  End;

  procedure TDemoWin.wmSize(var Msg : TMessage);  {!!.42}
    var
      LineNr : word;
      DC     : HDC;
      S : string;
    begin
      TBrowserWindow.wmSize(Msg);
      {Paint the final partial line - better this way than the flicker with
       erasing the background}
      S := GetFooter;
      if (S <> '') then LineNr := FullPage+2
      else              LineNr := FullPage+1;
      S := '';
      DC := GetDC(HWindow);
      WriteStringOut(S, LineNr, DC, 0);
      ReleaseDC(HWindow, DC);
    end;

  Constructor TDemoApp.Init ( AName : PChar );

  Begin
    If Not TApplication.Init ( AName ) Then Fail;
    If Not InitBrowser Then Fail;
  End;


  Destructor TDemoApp.Done;

  Begin
    TApplication.Done;
      {-Order must be this way because Isam or Shell are called indirectly}
    BTCloseFileBlock ( PersFB );
    BTExitIsam;
  End;


  Procedure TDemoApp.InitMainWindow;
  Begin
    {Initially, the main window is a hidden dummy window}
    CmdShow := sw_Hide;
    MainWindow := New(PWindow, Init(Nil, 'BTree Start'));

    {Initialize Filer and open the fileblock}
    If Not InitDataBase Then Begin
      DbError ( 'Fileblock could not be initialized.');
      Halt;
    End;
  End;

  Function TDemoApp.InitDataBase : Boolean;

  Var
    LResult : Integer;                                          {!!.51}

    Function InitForIsam : Boolean;

    Var
      NetType : NetSupportType;

    Begin
      InitForIsam := False;
      Case LResult Of                                           {!!.51}
        id_NetSingle : NetType := NoNet;
        id_NetMSNet : NetType := MSNet;
        id_NetNovell : NetType := Novell;
      End; {Case}
      BTInitIsam ( NetType, 30 );                               {!!.52}
      If Not IsamOK Then Exit;
      If Not OpenTheFile Then Exit;
      InitForIsam := True;
    End;

  Begin
    InitDataBase := False;
    if BTNoNetCompiled then                      {!!.23}
      LResult := id_NetSingle                     {!!.23}         {!!.51}
    else begin                                   {!!.23}
      LResult := ExecDialog ( New ( PNetDialog,                   {!!.51}
        Init ( Nil, 'BTWINDEMO_NET' ) ) );
      If LResult = id_cancel Then Halt {Exit};    {!!.24}         {!!.51}
    end;                                         {!!.23}

    If Not InitForIsam Then Exit;

    InitDataBase := True;
  End;

  Function TDemoApp.InitBrowser : Boolean;
  Var
    PD    : PDemoWin;
    SW    : PWindowsObject;
  Begin
    InitBrowser := False;

    {Restore CmdShow}
    CmdShow := sw_Show;

    {Now create the browser window}
    PD := New ( PDemoWin, InitDemo ( Nil, 'B-Tree Filer Test',
                                     PersFB, 1, 50, 1, '', ''  ));
    If PD = Nil Then Exit;
    {Attach its menu system}
    PD^.Attr.Menu := LoadMenu ( HInstance, 'BTWINDEMO_MENU' );

    {Get its window handle, etc.}
    PD^.Create;

    {Get rid of previous dummy MainWindow}
    SW := Application^.MainWindow;
    Application^.MainWindow := PD;
    Dispose ( SW, Done );

    {Show the browser window}
    PD^.Show ( sw_Show );
    PD^.SetAndUpdateBrowserScreen ( '', 0 );

    {Set a timer for refreshing screen in multi-user situations}
    If BTNetSupported <> NoNet Then
       SetTimer ( MainWindow^.HWindow, 1, 10000, Nil );

    InitBrowser := True;
  End;

Var
  SaveExitProc : Pointer;

  {$F+}
  Procedure DemoExitProc;

  Begin
    AutoErrorDisplay;
    ExitProc := SaveExitProc;
  End;

Begin
  {SaveExitProc := ExitProc;}  {!!.24}
  {ExitProc := @DemoExitProc;} {!!.24}
  BRHeader :=
        Extend (
            Extend ( 'Last name' + ', ' + 'First name', 30 )
            + ' ' +
            Extend ( 'Address', 25 )
            + ' '+
            Extend ( 'City', 15 )
            + ' '+
            Extend ( 'St', 2 )
            + ' ' +
            Extend ( 'Zip', 10 )
            + ' ' +
            Extend ( 'Phone', 12 ), MaxCols);
  If DemoApp.Init ( 'Demo Application') Then Begin
    SaveExitProc := ExitProc;   {!!.24}
    ExitProc := @DemoExitProc;  {!!.24}
    DemoApp.Run;
    DemoApp.Done;
  End;
End.
