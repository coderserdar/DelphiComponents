unit sStrings;

interface

const
  MaxAlphaStrID = 59000;
  s_MsgDlgWarning = MaxAlphaStrID - 2;
  s_MsgDlgError = MaxAlphaStrID - 3;
  s_MsgDlgInformation = MaxAlphaStrID - 4;
  s_MsgDlgConfirm = MaxAlphaStrID - 5;
  s_MsgDlgYes = MaxAlphaStrID - 6;
  s_MsgDlgNo = MaxAlphaStrID - 7;
  s_MsgDlgOK = MaxAlphaStrID - 8;
  s_MsgDlgCancel = MaxAlphaStrID - 9;
  s_MsgDlgHelp = MaxAlphaStrID - 10;
  s_MsgDlgHelpNone = MaxAlphaStrID - 11;
  s_MsgDlgHelpHelp = MaxAlphaStrID - 12;
  s_MsgDlgAbort = MaxAlphaStrID - 13;
  s_MsgDlgRetry = MaxAlphaStrID - 14;
  s_MsgDlgIgnore = MaxAlphaStrID - 15;
  s_MsgDlgAll = MaxAlphaStrID - 16;
  s_MsgDlgNoToAll = MaxAlphaStrID - 17;
  s_MsgDlgYesToAll = MaxAlphaStrID - 18;

  s_RestoreStr = MaxAlphaStrID - 19;
  s_MoveStr = MaxAlphaStrID - 20;
  s_SizeStr = MaxAlphaStrID - 21;
  s_MinimizeStr = MaxAlphaStrID - 22;
  s_MaximizeStr = MaxAlphaStrID - 23;
  s_CloseStr = MaxAlphaStrID - 24;

  s_HintClose = MaxAlphaStrID - 25;
  s_HintMaximize = MaxAlphaStrID - 26;
  s_HintMinimize = MaxAlphaStrID - 27;
  s_HintRestore = MaxAlphaStrID - 28;
  s_HintHelp = MaxAlphaStrID - 29;

  s_FileOpen = MaxAlphaStrID - 30;

  s_Calculator = MaxAlphaStrID - 31;
  s_GradBuilder = MaxAlphaStrID - 32;
  s_HotGradBuilder = MaxAlphaStrID - 33;
  s_Panels = MaxAlphaStrID - 34;

  s_AvailSkins = MaxAlphaStrID - 36;
  s_InternalSkin = MaxAlphaStrID - 37;

  s_ErrorSettingCount = MaxAlphaStrID - 38;
  s_ListBoxMustBeVirtual = MaxAlphaStrID - 39;
implementation

{$R sStrings.res}

end.