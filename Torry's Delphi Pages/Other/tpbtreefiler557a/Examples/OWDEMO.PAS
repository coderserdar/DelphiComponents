{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

{--Conditional defines and compiler options that affect this program}
  {$I BRDefOpt.Inc}

{$F+,X+,V-}
{$M 16384,4096}

Program OWDemo;

{$R OWDemo.Res}

Uses
{$IFDEF Ver80}                                                  {!!.51}
  Messages,                                                     {!!.51}
  SysUtils,                                                     {!!.51}
{$ENDIF}                                                        {!!.51}
  WinTypes,
  WinProcs,
{$IFDEF Ver10}
  WObjects,
{$ELSE}
 {$IFDEF Ver15}
  WObjects,
 {$ELSE}
  OWindows,
  ODialogs,
 {$ENDIF}
{$ENDIF}
{$IFNDEF Ver80}
  Strings,
{$ENDIF}
{$IFDEF BRUseShell}
  OPSBase,
  OOPShell,
  OPSCodes,
{$ENDIF}
{$IFDEF BRUseIsam}
  BTBase, BTIsBase,
  Filer,     {!!.TP}
  Rebuild,
{$ENDIF}
  CCSKeys,           {!!.TP: this is a bonus unit}                   {!!.05}
  WRebld,
  IsamTool,
  LowBrows,
  WBrowser;

Const
  id_NetSingle = 101;
  id_NetMSNet = 102;
  id_NetNovell = 103;

  id_DlgInput = 110;
  id_DlgPrompt = 111;

  idm_About = 201;
  idm_New = 202;
  idm_Delete = 203;
  idm_Search = 204;
  idm_Edit = 205;
  idm_FilterOnOff = 206;
  idm_Status = 207;

  em_DBPossibleAbortError = -199;

  TheFileBlockName = 'Adressen';

Type
  PDemoApp = ^TDemoApp;
  TDemoApp = Object (TApplication)
    SourceDestCP : Word;                                             {!!.05}
    OtherCP      : LongInt;                                          {!!.05}

    Constructor Init ( AName : PChar );
    Destructor Done; Virtual;
    Function InitDataBase : Boolean;
    Function InitBrowser : Boolean;
    Procedure InitMainWindow; Virtual;
  End;

  PFilterWindow = ^TFilterWindow;
  TFilterWindow = Object (TWindow)
    Constructor Init ( AParent: PWindow );
    Destructor Done; Virtual;
    Procedure GetWindowClass ( Var WndClass: TWndClass ); Virtual;
    Procedure SetFilterCaption ( CaptionOn : Boolean ); Virtual; {!!.51}
    Procedure WMSysCommand ( Var Msg : TMessage ); Virtual
        wm_First + wm_SysCommand;
  End;

  PStartWin = ^TStartWin;
  TStartWin = Object ( TWindow )
    SP : HBitMap;

    Constructor Init;
    Procedure GetWindowClass ( Var WndClass: TWndClass ); Virtual;
    Function GetClassName : PChar; Virtual;
    Procedure Paint (     PaintDC     : HDC;
                      Var PaintStruct : TPaintStruct ); Virtual;
  End;

  PDemoWin = ^TDemoWin;
  TDemoWin = Object (TBrowserWindow)
{    FW         : PFilterWindow;                   removed !!.03 }
{    Filtering  : Boolean;                         removed !!.03 }
{    FilterOn   : Boolean;                         removed !!.03 }
    Constructor Init ( AParent             : PWindowsObject;
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
    Procedure ShowErrorOccured ( EClass : Integer ); Virtual;
    Procedure ShowFilterWorking ( CallState : Integer;     {!!.03}
                                  Rejected  : Boolean ); Virtual;
{    Procedure EndFiltering;                          removed !!.03 }
    Procedure SetFilterMenuItem ( ItemOn : Boolean ); Virtual;  {!!.51}
{  Function PostCompletePage : Integer; Virtual;      removed !!.03 }
{  Function CanCallLowBroser : Boolean; Virtual;      removed !!.03 }
    Procedure GetWindowClass ( Var WndClass : TWndClass ); Virtual;
    Function EditDataRec ( ATitle : PChar ) : Boolean;
    Procedure EnableFilter ( FilterOn : Boolean ); Virtual;     {!!.51}
    Function HandleChar ( Var Msg : TMessage ) : Boolean; Virtual;  {!!.03a}
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
    Procedure WMQueryEndSession ( Var Msg: TMessage ); Virtual
        wm_First + wm_QueryEndSession;
    Procedure LButtonDblClk ( Var Msg: TMessage ); Virtual
        wm_First + wm_LButtonDblClk;
  End;

  PEditDialog = ^TEditDialog;
  TEditDialog = Object (TDialog)
    ThisTitle : PChar;

    Constructor Init ( AParent : PWindowsObject;
                       AName   : PChar;
                       ATitle  : PChar );
    Procedure SetUpWindow; Virtual;
    Procedure Ok ( Var Msg : TMessage); Virtual                      {!!.05}
        id_First + id_Ok;
  End;

  PLengthByteEdit = ^TLengthByteEdit;
  TLengthByteEdit = Object (TEdit)
    Function Transfer ( DataPtr      : Pointer;
                        TransferFlag : Word ) : Word; Virtual;
  End;

  PNetDialog = ^TNetDialog;
  TNetDialog = Object (TDialog)
    Constructor Init ( AParent : PWindowsObject; AName : PChar );
    Procedure ButtonSingle ( Var Msg : TMessage ); Virtual
        id_First + id_NetSingle;
    Procedure ButtonMsNet ( Var Msg : TMessage ); Virtual
        id_First + id_NetMsNet;
    Procedure ButtonNovell ( Var Msg : TMessage ); Virtual
        id_First + id_NetNovell;
    Procedure SetUpWindow; Virtual;
  End;

  PMLInputDialog = ^TMLInPutDialog;
  TMLInputDialog = Object (TDialog)
    Caption    : PChar;
    Prompt     : PChar;
    Buffer     : PChar;
    BufferSize : Word;

    Constructor Init ( AParent     : PWindowsObject;
                       ACaption,
                       APrompt,
                       ABuffer     : PChar;
                       ABufferSize : Word );
    Function CanClose : Boolean; Virtual;
    Procedure SetupWindow; Virtual;
  End;

  PersonRec = Record
    Name    : String [25];
    Vorname : String [20];
    Land    : String [3];
    Plz     : String [5];
    Wohnort : String [30];
    Strasse : String [30];
    Telefon : String [15];
  End;

  PPersonDef = ^PersonDef;
  PersonDef = Record
    Dele : LongInt;
    Pers : PersonRec;
  End;

Var
  Person     : PersonDef;
{$IFDEF BRUseShell}
  PersD      : IFDriver;
{$ENDIF}
{$IFDEF BRUseIsam}
  PersFB     : IsamFileBlockPtr;
{$ENDIF}
  DemoApp    : TDemoApp;


  Procedure DemoCharConvert ( DataPtr  : Pointer;                    {!!.05}
                              DataLen  : LongInt;
                              PostRead : Boolean;
                              HookPtr  : Pointer ); Far;

  Var
    SDCP : Word;
    OCP  : LongInt;

  Begin
    If PostRead Then Begin
      SDCP := Swap ( PDemoApp (Application)^.SourceDestCP );
    End Else Begin
      SDCP := PDemoApp (Application)^.SourceDestCP;
    End;
    OCP := PDemoApp (Application)^.OtherCP;
    With PPersonDef (DataPtr)^, Pers Do Begin
      ConvertChars ( @Name [1], Length ( Name ), SDCP, OCP );
      ConvertChars ( @Vorname [1], Length ( Vorname ), SDCP, OCP );
      ConvertChars ( @Land [1], Length ( Land ), SDCP, OCP );
      ConvertChars ( @Plz [1], Length ( Plz ), SDCP, OCP );
      ConvertChars ( @Wohnort [1], Length ( Wohnort ), SDCP, OCP );
      ConvertChars ( @Strasse [1], Length ( Strasse ), SDCP, OCP );
      ConvertChars ( @Telefon [1], Length ( Telefon ), SDCP, OCP );
    End;
  End;


  Procedure DbError ( S : PChar );

  Begin
    MessageBox ( GetActiveWindow, S, 'Data Base Error',
        MB_OK Or mb_TaskModal Or mb_SystemModal );
  End;

{$IFDEF BRUseShell}
  Procedure AutoErrorDisplay;

  Var
    CharAr : Array [Byte] Of Char;

  Begin
    If IFDIsamError <> 0 Then Begin
      StrPCopy ( CharAr, IFDErrorMessage ( 0, IFDIsamError ) );
      DbError ( CharAr );
    End;
  End;
{$ENDIF}

{$IFDEF BRUseIsam}
  Procedure AutoErrorDisplay;

  Var
    CharAr : Array [Byte] Of Char;

  Begin
    If IsamError <> 0 Then Begin
      StrPCopy ( CharAr, IsamErrorMessage ( IsamError ) );
      DbError ( CharAr );
    End;
  End;
{$ENDIF}

  Function YesNo ( S : String ) : Boolean;

  Var
    SZ : Array [Byte] Of Char;

  Begin
    StrPCopy ( SZ, S + '?' );
    YesNo := MessageBox ( GetActiveWindow, SZ, 'User Question',
        mb_YesNo Or mb_IconHand Or mb_SystemModal ) = id_Yes;
  End;


  Function LeftPadCH ( S : String; Ch : Char; Len : Byte ) : String;

  Var
    TempS : String;

  Begin
    If Len > Length (S) Then Begin
      FillChar ( TempS [1], Len - Length (S), Ch );
      TempS [0] := Chr (Len - Length (S));
      LeftPadCH := TempS + S;
    End Else Begin
      LeftPadCH := S;
    End;
  End;


  Function PadCH ( S : String; Ch : Char; Len : Byte) : String;

  Var
    TempS : String;

  Begin
    If Len > Length (S) Then Begin
      FillChar ( S [Succ (Length (S))], Len - Length (S), Ch );
      S [0] := Chr (Len);
    End;
    PadCH := S;
  End;


  Function Trim ( S : String ) : String;

  Var
    I    : Word;
    SLen : Byte Absolute S;

  Begin
    While (SLen > 0) And (S [SLen] <= ' ') Do Begin
      Dec (SLen);
    End;

    I := 1;
    While (I <= SLen) And (S[I] <= ' ') Do Begin
      Inc (I);
    End;
    If I > 1 Then Delete (S, 1, Pred (I));

    Trim := S;
  End;


{$IFDEF BRUseShell}
  Function BuildPersKey ( Var P; KeyNr : Word ) : IFDKeyStr;
{$ENDIF}
{$IFDEF BRUseIsam}
  Function BuildPersKey ( Var P; KeyNr : Word ) : IsamKeyStr;
{$ENDIF}

  Var
    TempS : String;                                                  {!!.05}

  Begin
    BuildPersKey := '';
    With PersonDef (P).Pers Do Begin
      Case KeyNr Of
        1: Begin
          TempS := PadCH ( Copy (Trim ( Name ), 1, 20), #32, 20 ) {!!.05mod}
              + PadCH ( Copy (Trim ( Vorname ), 1, 10), #32, 10 );{!!.05mod}
          BuildPersKey := NLStrToKey ( TempS,                     {!!.05mod}
              PDemoApp (Application)^.SourceDestCP,               {!!.05mod}
              PDemoApp (Application)^.OtherCP );                  {!!.05mod}
        End;
        2: Begin
          TempS := PadCH ( Trim ( Land ), #32, 3 )                {!!.05mod}
              + LeftPadCH ( Trim ( Plz ), #32, 5 );               {!!.05mod}
          BuildPersKey := NLStrToKey ( TempS,                     {!!.05mod}
              PDemoApp (Application)^.SourceDestCP,               {!!.05mod}
              PDemoApp (Application)^.OtherCP );                  {!!.05mod}
        End;
      End;
    End;
  End;



{$IFDEF BRUseShell}
  Function UserAnswer ( DrName : IFDriverName; Msg : IFDUserMsgType )
                                                                : Boolean;
  Var
    M : TMsg;

  Begin
    UserAnswer := False;
    Case Msg Of
      IULockViolation : YesNo ( DrName +
            ' record or file is locked. Try again '); {!!.TP}
      IULockRec :
        UserAnswer := YesNo ( DrName +
            ' <LockRec> failed. Try again '); {!!.TP}
      IULockFile :
        UserAnswer := YesNo ( DrName +
            ' <(Read)LockFileBlock> failed. Try again '); {!!.TP}
      IUIndexDefect :
        UserAnswer := YesNo ( DrName +
            ' Index is defective or invalid. Rebuild '); {!!.TP}
      IUFileNotExist :
        UserAnswer := YesNo ( DrName + ' existiert nicht. Neuanlegen' );
      IURecDeleted :
        UserAnswer := YesNo ( DrName +
            ' record was deleted. Create new '); {!!.TP}
      IUDriveNotReady :
        UserAnswer:= YesNo ( DrName +
            ': drive not ready. Try again '); {!!.TP}
      IURebuildOnlyIndex :                                           {!!.05}
        UserAnswer := YesNo ( DrName +
            ': rebuild the index file only ' );
      Else
        UserAnswer:= YesNo ( DrName +
            ': unknown error. Try again '); {!!.TP}
    End;
  End;
{$ENDIF}


  Constructor TFilterWindow.Init ( AParent : PWindow );

  Var
    TR : TRect;
    H  : Integer;

  Begin
    TWindow.Init ( AParent,
                   'Filter : abort by pressing Esc or left mouse button'); {!!.TP}
    Attr.Style := ws_PopUp Or ws_Caption;
    H := GetSystemMetrics ( sm_CYcaption );
    GetWindowRect ( AParent^.HWindow, TR );
    Attr.X :=  TR.Left + GetSystemMetrics ( sm_CxFrame )
        - GetSystemMetrics ( sm_CXBorder );
    Attr.Y :=  TR.Top +  GetSystemMetrics ( sm_CYFrame )
        - GetSystemMetrics ( sm_CYBorder );
    Attr.W :=  Tr.Right - Tr.Left - 2 * GetSystemMetrics ( sm_CXFrame )
        + 2 * GetSystemMetrics ( sm_CXBorder );
    Attr.H :=  H;
  End;


  Destructor TFilterWindow.Done;

  Begin
    TWindow.Done;
  End;


  Procedure TFilterWindow.SetFilterCaption ( CaptionOn : Boolean );  {!!.51}

  Const
    RejectChar = '?';

  Var
    CaptionBuffer : Array [Byte] Of Char;
    CapLen        : Word;

  Begin
    GetWindowText ( HWindow, CaptionBuffer, SizeOf (CaptionBuffer) );
    CapLen := StrLen (CaptionBuffer);
    If CaptionOn Then Begin                                         {!!.51}
      If CaptionBuffer [Pred (CapLen)] <> RejectChar Then Begin
        Move ( CaptionBuffer, CaptionBuffer [2], Succ (CapLen) );
        CaptionBuffer [0] := RejectChar;
        CaptionBuffer [1] := ' ';
        StrCat ( CaptionBuffer, ' ' );
        StrCat ( CaptionBuffer, RejectChar );
      End Else Begin
        Exit;
      End;
    End Else Begin
      If CaptionBuffer [Pred (CapLen)] = RejectChar Then Begin
        Move ( CaptionBuffer [2], CaptionBuffer, Pred (CapLen) );
        CaptionBuffer [CapLen - 4] := #0
      End Else Begin
        Exit;
      End;
    End;
    SetWindowText ( HWindow, CaptionBuffer );
  End;


  Procedure TFilterWindow.WMSysCommand ( Var Msg : TMessage );


  Begin
   If Msg.wParam And $FFF0 <> sc_Move Then
     DefWndProc ( Msg );
  End;


  Procedure TFilterWindow.GetWindowClass ( Var WndClass: TWndClass );

  Begin
    TWindow.GetWindowClass ( WndClass );
    WndClass.Style := WndClass.Style Or cs_NoClose;
  End;


  Constructor TEditDialog.Init ( AParent : PWindowsObject;
                                 AName   : PChar;
                                 ATitle  : PChar );


    Procedure InitChildren;

    Var
      S : PLengthByteEdit;
      P : PButton;

    Begin
      TransferBuffer := @Person.Pers;
      With Person.Pers Do Begin
        New (S, InitResource ( @Self, 300, SizeOf (Name) ));
        New (S, InitResource ( @Self, 301, SizeOf (VorName) ));
        New (S, InitResource ( @Self, 306, SizeOf (Land) ));
        New (S, InitResource ( @Self, 302, SizeOf (Plz) ));
        New (S, InitResource ( @Self, 303, SizeOf (Wohnort) ));
        New (S, InitResource ( @Self, 304, SizeOf (Strasse) ));
        New (S, InitResource ( @Self, 305, SizeOf (Telefon) ));
        New (P, InitResource ( @Self, id_OK ));
        New (P, InitResource ( @Self, id_Cancel ));
      End;
    End;


  Begin
    TDialog.Init ( AParent, AName );
    ThisTitle := ATitle;
    InitChildren;
  End;


  Procedure TEditDialog.SetUpWindow;

  Begin
    TDialog.SetUpWindow;
    SetWindowText ( HWindow, ThisTitle );
  End;


  Procedure TEditDialog.Ok ( Var Msg : TMessage);                    {!!.05}


    Procedure ShowAndPosOnError ( ID, Pos : Word );

    Begin
      MessageBox ( HWindow, 'Invalid Entry',
          'Edit Error', mb_Ok Or mb_TaskModal Or mb_IconInformation ); {!!.TP}
      SendMessage ( HWindow, wm_NextDlgCtl, GetDlgItem ( HWindow, ID ), 1 );
      SendDlgItemMsg ( ID, em_SetSel, 0, MakeLong ( Pos, Succ (Pos) ) );
    End;


  Var
    Res     : LongInt;
    FaultID : Word;
    SDCP    : Word;
    OCP     : LongInt;

  Begin
    TransferData ( tf_GetData );
    SDCP := PDemoApp (Application)^.SourceDestCP;
    OCP := PDemoApp (Application)^.OtherCP;

    FaultID := 300;
    With Person.Pers Do Begin
      Res := CheckChars ( @Name [1], Length ( Name ), SDCP, OCP );
      If Res = 0 Then Begin
        FaultID := 301;
        Res := CheckChars ( @Vorname [1], Length ( Vorname ), SDCP, OCP );
      End;
      If Res = 0 Then Begin
        FaultID := 306;
        Res := CheckChars ( @Land [1], Length ( Land ), SDCP, OCP );
      End;
      If Res = 0 Then Begin
        FaultID := 302;
        Res := CheckChars ( @Plz [1], Length ( Plz ), SDCP, OCP );
      End;
      If Res = 0 Then Begin
        FaultID := 303;
        Res := CheckChars ( @Wohnort [1], Length ( Wohnort ), SDCP, OCP );
      End;
      If Res = 0 Then Begin
        FaultID := 304;
        Res := CheckChars ( @Strasse [1], Length ( Strasse ), SDCP, OCP );
      End;
      If Res = 0 Then Begin
        FaultID := 305;
        Res := CheckChars ( @Telefon [1], Length ( Telefon ), SDCP, OCP );
      End;
    End;

    If Res <> 0 Then Begin
      ShowAndPosOnError ( FaultID, Res And $FFFF );
      Exit;
    End;

    TDialog.OK ( Msg );
  End;


  Function TLengthByteEdit.Transfer ( DataPtr      : Pointer;
                                      TransferFlag : Word ) : Word;

  Var
    TempStr : String;

  Begin
    Transfer := TextLen;
    If TransferFlag = tf_GetData Then Begin
      GetWindowText ( HWindow, @TempStr, TextLen );
      TempStr := StrPas ( PChar (@TempStr) );
      Move ( TempStr, DataPtr^, TextLen );
    End Else Begin
      Move ( DataPtr^, TempStr, TextLen );
      StrPCopy ( PChar (@TempStr), TempStr );
      SetWindowText ( HWindow, @TempStr );
    End;
  End;


  Constructor TNetDialog.Init ( AParent : PWindowsObject; AName : PChar );


    Procedure InitChildren;

    Var
      P : PButton;

    Begin
      New (P, InitResource ( @Self, id_NetSingle ));
      New (P, InitResource ( @Self, id_NetMSNet ));
      New (P, InitResource ( @Self, id_NetNovell ));
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


  Procedure TNetDialog.SetUpWindow;

  Var
    TR : TRect;

  Begin
    GetWindowRect ( HWindow, TR );
    With Tr Do Begin
      MoveWindow ( HWindow,
          ( GetSystemMetrics ( sm_CXScreen ) - Right + Left ) Div 2,
          GetSystemMetrics ( sm_CYScreen ) - Bottom + Top - 40,
          TR.Right- Tr.Left, TR.Bottom - Tr.Top, False );
    End;
  End;


  Constructor TMLInputDialog.Init ( AParent     : PWindowsObject;
                                    ACaption,
                                    APrompt,
                                    ABuffer     : PChar;
                                    ABufferSize : Word );

  Begin
    TDialog.Init ( AParent, 'MULTILINE_INPUT' );
    Caption := ACaption;
    Prompt := APrompt;
    Buffer := ABuffer;
    BufferSize := ABufferSize;
  End;


  Function TMLInputDialog.CanClose : Boolean;

  Begin
    GetDlgItemText ( HWindow, id_DlgInput, Buffer, BufferSize );
    CanClose := True;
  End;


  Procedure TMLInputDialog.SetupWindow;

  Begin
    TDialog.SetupWindow;
    SetWindowText ( HWindow, Caption );
    SetDlgItemText ( HWindow, id_DlgPrompt, Prompt );
    SetDlgItemText ( HWindow, id_DlgInput, Buffer );
    SendDlgItemMessage ( HWindow, id_DlgInput, em_LimitText,
        BufferSize - 1, 0);
  End;


  Constructor TStartWin.Init;

  Begin
    TWindow.Init ( Nil, 'BTree Start' );
    Attr.Style := ws_Visible Or ws_PopUp Or ws_Border Or ws_Disabled;
    Sp := 0;
  {$IFDEF BrUseShell}
    Sp := LoadBitMap ( HInstance, 'SStart_Pict' );
  {$ENDIF}
  {$IFDEF BrUseIsam}
    Sp := LoadBitMap ( HInstance, 'IStart_Pict' );
  {$ENDIF}
  {$IFNDEF BrUseShell}                                          {!!.51}
   {$IFNDEF BrUseIsam}                                          {!!.51}
    Attr.X := (GetSystemMetrics ( sm_CXScreen ) - 250) Div 2;
    Attr.Y := 40;
    Attr.W := 250;
    Attr.H := 150;
   {$ELSE}                                                      {!!.51}
  {$ELSE}                                                       {!!.51}
    Attr.X := 0;                                                {!!.51}
    Attr.Y := 0;                                                {!!.51}
    Attr.W := 0;                                                {!!.51}
    Attr.H := 0;                                                {!!.51}
   {$ENDIF}                                                     {!!.51}
  {$ENDIF}                                                      {!!.51}
  End;


  Procedure TStartWin.GetWindowClass ( Var WndClass: TWndClass );

  Begin
    TWindow.GetWindowClass ( WndClass );
    WndClass.Style := WndClass.Style Or cs_NoClose;
  End;


  Function TStartWin.GetClassName : PChar;

  Begin
    GetClassName := 'StartWindow';
  End;


  Procedure TStartWin.Paint (     PaintDC     : HDC;
                              Var PaintStruct : TPaintStruct );

  Var
    MemDC     : HDC;
    OldBitMap : HBitMap;
    TR        : TRect;

  Begin
    If Sp >0 Then Begin
      GetClientRect ( Hwindow, TR );
      MemDC := CreateCompatibleDC ( PaintDC );
      OldBitMap := SelectObject ( MemDC, SP );
      BitBlt ( PaintDC, 0, 0, TR.Right, TR.Bottom, MemDC, 0, 0, SRCCopy );
      SelectObject ( MemDC, OldBitMap );
      ReleaseDC ( HWindow, MemDC );
    End;
  End;


  Constructor TDemoWin.Init ( AParent             : PWindowsObject;
                              ATitle              : PChar;
                              ADrvOrFileBlockPtr  : Pointer;
                              ANumberOfEltsPerRow : Word;
                              ANumberOfRows       : Word;
                              AKeyNr              : Word;
                              ALKey,
                              AHKey               : GenKeyStr );

  Var
    Dummy    : Boolean;
    BRHeader : BRLRowEltString;

  Begin
    If Not TBrowserWindow.Init ( AParent, ATitle ) Then Fail;

    BRHeader := PadCH (                                           {!!.05mov}
        PadCH ( 'LastName' + ', ' + 'FirstName', #32, 30 )        {!!.TP}
        + ' '+
        PadCH ( 'Ctry-Zip', #32, 9 )                              {!!.03a}{!!.TP}
        + ' ' +
        PadCH ( 'City', #32, 30 )                                 {!!.TP}
        + ' '+
        PadCH ( 'Street', #32, 30 )                               {!!.TP}
        + ' ' +
        PadCH ( 'Telephone', #32, 15 ), #32, MaxCols );           {!!.TP}

{    FilterOn := False;                           removed !!.03 }
{    FW := Nil;                                   removed !!.03 }
{    Filtering := False;                          removed !!.03 }

  {$IFDEF BRUseShell}
    If Not ConnectLowBrowser (
        New (PLowWinBrowser, Init ( True, ADrvOrFileBlockPtr,
            ANumberOfEltsPerRow, ANumberOfRows, AKeyNr, ALKey, AHKey,
            Dummy, False )), BRHeader, '') Then Begin
  {$ENDIF}
  {$IFDEF BRUseIsam}
    If Not ConnectLowBrowser (
        New (PLowWinBrowser, Init ( True, ADrvOrFileBlockPtr,
            ANumberOfEltsPerRow, ANumberOfRows, AKeyNr, ALKey, AHKey,
            Person, False )), BRHeader, '') Then Begin
  {$ENDIF}
      Fail;
    End;
  End;


  Destructor TDemoWin.Done;

  Begin
  {$IFDEF BRUseShell}
    If IFDNetSupported <> NoNet Then
        KillTimer ( HWindow, 1 );
  {$ENDIF}
  {$IFDEF BRUseIsam}
    If BTNetSupported <> NoNet Then
        KillTimer ( HWindow, 1 );
  {$ENDIF}
    TBrowserWindow.Done;
  End;


  Function TDemoWin.BuildRow ( Var RR : RowRec ) : Integer;

  Begin
    BuildRow := NoError;
    With Person.Pers Do Begin
      If RR.Status <> NoError Then Begin
        RR.Row := PadCH ( '****  ' + RR.IKS, #32, MaxCols );
      End Else Begin
        RR.Row := Copy ( PadCH ( Name + ', ' + Vorname, #32, 30 ), 1, 30 );
        RR.Row := PadCH ( RR.Row
            + ' '+
            PadCH ( Trim (Land) + '-' + Trim (Plz), #32, 9 )      {!!.03a}
            + ' ' +
            PadCH ( WohnOrt, #32, 30 )
            + ' '+
            PadCH ( Strasse, #32, 30 )
            + ' ' +
            PadCH ( Telefon, #32, 15 ), #32, MaxCols )
      End;
    End;
  End;


  Function TDemoWin.PerformFilter ( Var RR    : RowRec;              {!!.03}
                                    Var UseIt : Boolean ) : Integer;
  Var
    LResult        : Integer;                                   {!!.51}
    TempS         : String;
    FltInProgress : Boolean;

  Begin
    PerformFilter := NoError;
    UseIt := True;
    If FilterIsOn ( FltInProgress ) Then Begin                       {!!.03}
      LResult := GetThisRec ( RR );                             {!!.51}
      PerformFilter := LResult;                                 {!!.51}
      If LResult > LockError Then Exit;                         {!!.51}
      If LResult <> NoError Then Begin                          {!!.51}
        RR.Status := BRNoFilterResult;
      End Else Begin
        If Length (Person.Pers.Vorname) > 0 Then Begin
          UseIt := Odd (Ord (Person.Pers.Vorname [1]));
        End Else Begin
          UseIt := False;
        End;
      End;
      If UseIt Then Begin
        PerformFilter := BuildBrowScreenRow ( RR );
      End;
    End;
  End;


  Procedure TDemoWin.ShowFilterWorking ( CallState : Integer;        {!!.03}
                                         Rejected : Boolean );

  Const
    Focused : HWnd = 0;
    FW      : PFilterWindow = Nil;

  Var
    Msg      : TMsg;
    Break    : Boolean;
    WP       : THandle;
    HW       : HWnd;
    HasFocus : Boolean;

  Begin
    Case CallState Of
      -1 : Begin
        Focused := GetFocus;
        If Focused = 0 Then Focused := HWindow;
        EnableWindow ( HWindow, False );
      End;
      0: Begin
        If Rejected And ( FW = Nil ) Then Begin
          FW := PFilterWindow (Application^.MakeWindow
              ( New (PFilterWindow, Init ( @Self )) ));
          If Focused <> HWindow Then Begin
            SetWindowPos ( FW^.HWindow, 1, 10, 10, 40, 20,
                swp_NoMove Or swp_NoSize Or swp_NoActivate);
            WP := BeginDeferWindowPos ( 2 );
            If WP <> 0 Then Begin
              WP := DeferWindowPos ( Wp, GetActiveWindow, 0, 0, 0, 0, 0,
                  swp_NoMove Or swp_NoSize Or swp_NoZOrder );
              WP := DeferWindowPos ( Wp, FW^.HWindow, HWindow, 0, 0, 0, 0,
                  swp_NoMove Or swp_NoSize Or swp_ShowWindow );
              EndDeferWindowPos ( WP );
            End;
          End Else Begin
            SetWindowPos ( FW^.HWindow, HWindow, 10, 10, 40, 20,
                swp_NoMove Or swp_NoSize Or swp_ShowWindow );
            SetFocus ( FW^.HWindow );
          End;
        End;
        Break := False;
         While PeekMessage ( Msg, 0, 0, 0, pm_Remove ) Do Begin
          If FW <> Nil Then Begin
            If (Msg.Message = WM_NCLButtonUp)
                And (Msg.Hwnd = FW^.HWindow) Then Break := True;
            If (Msg.Message = WM_KeyUp) And (Msg.Hwnd = FW^.HWindow)
                And (Msg.wParam = vk_Escape) Then Break := True;
          End;
          TranslateMessage ( Msg );
          DispatchMessage ( Msg );
        End;
        If FW <> Nil Then FW^.SetFilterCaption ( Rejected );
        If Break Then Begin
          If YesNo ( 'Filter aborted' ) Then Begin {!!.TP}
            Rejected := False;
            EnableFilter ( False );
          End Else Begin
            InValidateRect ( HWindow, Nil, False );
          End;
        End;
      End;
      1 : Begin
        HW := GetFocus;
        HasFocus := (HW = HWindow) Or (HW = 0);
        If FW <> Nil Then Begin
          EnableWindow ( HWindow, True );
          If HasFocus Or ( HW = FW^.HWindow ) Then SetFocus ( HWindow );
          Dispose ( FW, Done );
          FW := Nil;
        End Else Begin
          EnableWindow ( HWindow, True );
          If HasFocus Then SetFocus ( HWindow );
        End;
        InValidateRect ( HWindow, Nil, False );
      End;
    End;
  End;


  Procedure TDemoWin.ShowErrorOccured ( EClass : Integer );     {!!.51}


  Begin
{    EndFiltering;                                 removed !!.03 }
    MessageBeep ( 0 );
    If EClass > 1 Then Begin                                    {!!.51}
      If YesNo ( 'Abort program' ) Then Begin {!!.TP}
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
    Dialog := New (PEditDialog, Init ( @Self, 'BTWINDEMO_DIA', ATitle ));
    EditDataRec := Application^.ExecDialog ( Dialog ) = id_OK;
  End;


  Function TDemoWin.HandleChar ( Var Msg : TMessage ) : Boolean;  {!!.03a}

  Var
    S : String [1];

  Begin
    HandleChar := False;
    S := Chr (Msg.wParam);                                        {!!.05mod}
    S := NLStrToKey ( S, PDemoApp (Application)^.SourceDestCP,    {!!.05mod}
        PDemoApp (Application)^.OtherCP );                        {!!.05mod}
    Case S [1] Of
      '0'..'9', 'A'..'Z' : Begin                                  {!!.05mod}
        SetAndUpdateBrowserScreen ( S, 0 );                       {!!.05mod}
        HandleChar := True;
      End;
    End; {Case}
  End;


  Procedure TDemoWin.ProcessAbout ( Var Msg: TMessage );

  Begin
  {$IFDEF BRUseIsam}
    MessageBox ( HWindow, 'B-Tree Filer Demo for Windows' + #13#10 {!!.TP}
        + 'Copyright (C) 1990-94 ENZ EDV-Beratung GmbH', 'About', mb_OK );
  {$ENDIF}
  {$IFDEF BRUseShell}
    MessageBox ( HWindow, 'B-Tree Shell Demo for Windows' + #13#10 {!!.04a}
        + 'Copyright (C) 1990-94 ENZ EDV-Beratung GmbH', 'About', mb_OK );
  {$ENDIF}
  End;


{$IFDEF BRUseIsam}
  {$I IsStuff.Inc}
{$ENDIF}


{$IFDEF BRUseShell}
  Procedure TDemoWin.NewRec ( Var Msg: TMessage);

  Var
    SaveTimer : Boolean;

  Begin
    SaveTimer := SetSuppressTimer ( True );
    PersD.IFDClearWorkBuffer;
    If EditDataRec ( ' New ' ) Then Begin
      PersD.IFDAddRec;
      If MaxErrorClass > 0 Then Begin
        DbError ( 'Record addition failed') {!!.TP}
      End Else Begin
        With PersD Do Begin
          SetAndUpdateBrowserScreen (
              IFDBuildKeyWithNumber ( GetCurrentKeyNr ), IFDGetWorkRefNr );
        End;
      End;
    End;
    SetSuppressTimer ( SaveTimer );
  End;


  Procedure TDemoWin.EditRec ( Var Msg: TMessage);

  Var
    Match,
    SaveTimer : Boolean;

  Begin
    SaveTimer := SetSuppressTimer ( True );
    If GetCurrentRec ( Match ) = 0 Then Begin      {!!.03}
      If Match Then Begin                          {!!.03}
        If EditDataRec ( ' Edit ' ) Then Begin
          PersD.IFDChangeRec;
          If MaxErrorClass > 0 Then Begin
            DbError ( 'Record change failed'); {!!.TP}
          End Else Begin
            With PersD Do Begin
              SetAndUpdateBrowserScreen (
                  IFDBuildKeyWithNumber ( GetCurrentKeyNr ),
                  IFDGetWorkRefNr );
            End;
          End;
        End;
      End;
    End;
    SetSuppressTimer ( SaveTimer );
  End;


  Procedure TDemoWin.DelRec ( Var Msg: TMessage);

  Var
    Match,
    SaveTimer : Boolean;

  Begin
    SaveTimer := SetSuppressTimer ( True );
    If GetCurrentRec ( Match ) = 0 Then Begin                {!!.03}
      If Match Then Begin                                    {!!.03}
        If EditDataRec ( ' Delete ' ) Then Begin
          PersD.RestoreWorkBuffer;
          PersD.IFDDeleteRec ( IPNone );
          If MaxErrorClass > 0 Then Begin
            DbError ( 'Record deletion failed'); {!!.TP}
          End Else Begin
            UpDateBrowserScreen;
          End;
        End;
      End;
    End;
    SetSuppressTimer ( SaveTimer );
  End;


  Procedure TDemoWin.Search ( Var Msg: TMessage);

  Var
    SaveTimer : Boolean;

  Begin
    SaveTimer := SetSuppressTimer ( True );
    PersD.IFDClearWorkBuffer;
    If EditDataRec ( ' Search ' ) Then Begin
      PersD.IFDSearchRec
          ( 1, BuildPersKey ( Person, 1 ));
      If MaxErrorClass > 0 Then Begin
        DbError ( 'Search failed') {!!.TP}
      End Else Begin
        With PersD Do Begin
          SetAndUpdateBrowserScreen (
              IFDBuildKeyWithNumber ( IFDGetActiveKeyNr ), IFDGetWorkRefNr );
        End;
      End;
    End;
    SetSuppressTimer ( SaveTimer );
  End;
{$ENDIF}


  Procedure TDemoWin.SetFilterMenuItem ( ItemOn : Boolean );    {!!.51}

  Begin
    If ItemOn Then Begin                                        {!!.51}
      ModifyMenu ( Attr.Menu, idm_FilterOnOff, mf_ByCommand,
          idm_FilterOnOff, '&Filter On' );
    End Else Begin
      ModifyMenu ( Attr.Menu, idm_FilterOnOff, mf_ByCommand,
          idm_FilterOnOff, '&Filter Off' );
    End;
    DrawMenuBar ( HWindow );
  End;


  Procedure TDemoWin.EnableFilter ( FilterOn : Boolean );       {!!.51}

  Begin
    TBrowserWindow.EnableFilter ( FilterOn );  {modified !!.03} {!!.51}
    If Not FilterOn Then Begin                                  {!!.51}
{      EndFiltering;                         removed  !!.03}
      SetSuppressTimer ( False );
    End Else Begin
      SetSuppressTimer ( True );
    End;
    SetFilterMenuItem ( FilterOn );                             {!!.51}
  End;


  Procedure TDemoWin.FilterOnOff ( Var Msg: TMessage );

  Var
    Dummy : Boolean;

  Begin
    EnableFilter ( Not FilterIsOn  ( Dummy ) );        {modified !!.03}
    UpDateBrowserScreen;
  End;


  Procedure TDemoWin.ShowStatus ( Var Msg: TMessage);

  Var
    LResult   : Integer;                                        {!!.51}
    UK        : LongInt;
    S, T      : String;
    Buffer    : Array [Byte] Of Char;
    SaveTimer : Boolean;

  Begin
    SaveTimer := SetSuppressTimer ( True );
  {$IFDEF BRUseShell}
    UK := PersD.IFDUsedKeys ( GetCurrentKeyNr );
    LResult := MaxErrorClass;                                   {!!.51}
  {$ENDIF}
  {$IFDEF BRUseIsam}
    UK := BTUsedKeys ( PersFB, GetCurrentKeyNr );
    LResult := BTIsamErrorClass;                                {!!.51}
  {$ENDIF}
    If LResult = NoError Then Begin                             {!!.51}
      Str ( UK : 0, S );
    {$IFDEF BRUseShell}
      Str ( PersD.IFDGetInternalDialogID : 0, T );
    {$ENDIF}
    {$IFDEF BRUseIsam}
      Str ( BTGetInternalDialogID ( PersFB ) : 0, T );
    {$ENDIF}
      S := 'Records: ' + S + '    Task : ' + T; {!!.TP}
      StrPCopy ( Buffer, S );
      MessageBox ( GetActiveWindow, @Buffer,'Status',
          mb_OK Or mb_TaskModal );
    End;
    SetSuppressTimer ( SaveTimer );
  End;


  Procedure TDemoWin.WMQueryEndSession ( Var Msg: TMessage );

  Begin
    DbError ( 'Please close the first database window!'); {!!.TP}
    DefWndProc ( Msg );
    Msg.Result := 0;
  End;


  Procedure TDemoWin.LButtonDblClk ( Var Msg: TMessage );

  Begin
    EditRec ( Msg );
  End;


  Procedure TDemoWin.GetWindowClass ( Var WndClass : TWndClass );

  Begin
    TWindow.GetWindowClass ( WndClass );
    WndClass.Style := cs_DblClks;
    WndClass.hIcon := LoadIcon ( hInstance, 'BTWINDEMO_ICON' );
  End;


  Constructor TDemoApp.Init ( AName : PChar );

  Begin
    If Not TApplication.Init ( AName ) Then Fail;

    SourceDestCP := CPDos437;                                        {!!.05}
    SourceDestCP := SourceDestCP Shl 8 Or GetCodePageConst;          {!!.05}
    OtherCP := BuildCodePageMask ( CPWin1252 )                       {!!.05}
        Or BuildCodePageMask ( CPDos850 );                           {!!.05}

    If Not InitDataBase Then Begin
      DbError ( 'Unable to open database'); {!!.TP}
      Halt;
    End;
    If Not InitBrowser Then Fail;
  End;


  Destructor TDemoApp.Done;

  Begin
    TApplication.Done;
      {-Order must be this way because Isam or Shell are called indirectly}
  {$IFDEF BRUseIsam}
    BTCloseFileBlock ( PersFB );
    BTExitIsam;
  {$ENDIF}
  {$IFDEF BRUseShell}
    PersD.Done;
    IFDExit;
  {$ENDIF}
  End;


  Procedure TDemoApp.InitMainWindow;

  Begin
    MainWindow := New (PStartWin, Init);
  End;

{$IFDEF BRUseShell}
  Procedure InformApp ( PRInfo   : PRebuildInfo;
                        DrName   : IFDriverName;
                        CallType : ReXCallType ); Far;

  Var
    MsgT      : TMsg;
    SaveTimer : Boolean;

  Begin
    Case CallType Of
      ReXFirst : Begin
        If Application^.MainWindow <> Nil Then Begin
          If (TypeOf (Application^.MainWindow^) = TypeOf (TDemoWin))
              And (Application^.MainWindow^.HWindow <> 0) Then Begin
            With PDemoWin (Application^.MainWindow)^ Do Begin
              EnableWindow ( HWindow, False );
              SaveTimer := SetSuppressTimer ( True );
            End;
          End;
        End;
      End;
      ReXPerform: Begin
        If PRInfo <> Nil Then Begin
          If PRInfo^.HWindow <> 0 Then Begin
            While PeekMessage ( MsgT, 0, 0, 0, pm_Remove ) Do Begin
              If Not IsDialogMessage ( PRInfo^.HWindow, MsgT ) Then Begin
                TranslateMessage ( MsgT );
                DispatchMessage ( MsgT );
              End;
            End;
          End;
        End;
      End;
      ReXFinal : Begin
        If Application^.MainWindow <> Nil Then Begin
          If (TypeOf (Application^.MainWindow^) = TypeOf (TDemoWin))
              And (Application^.MainWindow^.HWindow <> 0) Then Begin
            With PDemoWin (Application^.MainWindow)^ Do Begin
              EnableWindow ( HWindow, True );
              SetSuppressTimer ( SaveTimer );
            End;
          End;
        End;
      End;
    End;
  End;
{$ENDIF}


  Function TDemoApp.InitDataBase : Boolean;

  Var
    LResult  : Integer;                                         {!!.51}
    Dialog   : PDialog;
    NoNetCom : Boolean;


{$IFDEF BRUseShell}

    Function InitForShell : Boolean;

    Var
      NetType : IFDNetSupportType;

    Begin
      InitForShell := False;
      Case LResult Of                                           {!!.51}
        id_NetSingle : NetType := NoNet;
        id_NetMSNet : NetType := MSNet;
        id_NetNovell : NetType := Novell;
      End; {Case}
      IFDInitEMS ( NetType, MemAvail - 200000, 13, 13 );
      If MaxErrorClass > 0 Then Exit;
      IFDSetDefaultYesNoRequest ( UserAnswer );
      AskRebuildIndexOnly := True;                                   {!!.05}
      PersD.Init ( 'ADDRESS', TheFileBlockName, False, True, False, True,
          SizeOf (PersonDef), 2, @Person, SizeOf (PersonDef), BuildPersKey,
          DefaultNrRetries );
      If MaxErrorClass <> NoError Then Exit;
      PersD.IFDDefineKey ( 1, 30, False );
      If MaxErrorClass <> NoError Then Exit;
      PersD.IFDDefineKey ( 2, 8, True );
      If MaxErrorClass <> NoError Then Exit;
      PersD.IFDSetReXUserProc ( DisplayRebuildInfo );

      PersD.IFDSetCharConvert ( DemoCharConvert, Nil );              {!!.05}

      SetInformProc ( InformApp );

      PersD.IFDOpenFile;
      If MaxErrorClass <> NoError Then Exit;
      InitForShell := True;
    End;
{$ENDIF}

{$IFDEF BRUseIsam}
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

      BTSetCharConvert ( PersFB, DemoCharConvert, Nil, False );      {!!.05}
      InitForIsam := True;
    End;
{$ENDIF}

  Begin
    InitDataBase := False;

  {$IFDEF BRUseShell}
    NoNetCom := IFDNoNetCompiled ( True );
      {-Ignore the error code}
  {$ENDIF}

  {$IFDEF BRUseIsam}
    NoNetCom := BTNoNetCompiled;
      {-Ignore the error code}
  {$ENDIF}

    If NoNetCom Then Begin
      LResult := id_NetSingle;                                  {!!.51}
    End Else Begin
      LResult := ExecDialog ( New (PNetDialog,                  {!!.51}
          Init ( MainWindow, 'BTWINDEMO_NET' )) );
      If LResult = id_Cancel Then Exit;                         {!!.51}
    End;

  {$IFDEF BRUseShell}
    If Not InitForShell Then Begin
      Exit;
    End;
  {$ENDIF}

  {$IFDEF BRUseIsam}
    If Not InitForIsam Then Begin
      Exit;
    End;
  {$ENDIF}

    InitDataBase := True;
  End;


  Function TDemoApp.InitBrowser : Boolean;

  Var
    PD    : PDemoWin;
    SW    : PWindowsObject;
    Dummy : LongInt;

  Begin
    InitBrowser := False;

  {$IFDEF BRUseShell}
    PD := New (PDemoWin, Init ( Nil, 'B-Tree Shell Demo', @PersD,
        1, 50 , 1, '', ''  ));
  {$ENDIF}
  {$IFDEF BRUseIsam}
    PD := New (PDemoWin, Init ( Nil, 'B-Tree Filer Demo', PersFB, {!!.TP}
        1, 50 , 1, '', ''  ));
  {$ENDIF}

    If PD = Nil Then Exit;
    PD^.Attr.Menu := LoadMenu ( HInstance, 'BTWINDEMO_MENU' );
    PD^.Create;

    SW := Application^.MainWindow;
    Application^.MainWindow := PD;
    Dispose ( SW, Done );
    PD^.Show ( sw_Show );
    InitBrowser := True;
    PD^.SetAndUpdateBrowserScreen ( '', 0 );

  {$IFDEF BRUseShell}
   If IFDNetSupported <> NoNet Then
       SetTimer ( PD^.HWindow, 1, 5000, Nil );
  {$ENDIF}
  {$IFDEF BRUseIsam}
   If BTNetSupported <> NoNet Then
       SetTimer ( PD^.HWindow, 1, 10000, Nil );
  {$ENDIF}

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
  SaveExitProc := ExitProc;
  ExitProc := @DemoExitProc;
  If DemoApp.Init ( 'Demo Application') Then Begin
    DemoApp.Run;
    DemoApp.Done;
  End;
End.
