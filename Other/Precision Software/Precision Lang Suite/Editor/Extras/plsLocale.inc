procedure LocalizeMsgDlgs;
var
  sec,lc,fn:string;
  Ini:TMemIniFile;
begin
  lc:=GetVariable('ScriptLangName');
  if lc='' then
    exit; 
  fn:=ExtractFilePath(ExcludeTrailingPathDelimiter(GetVariable('LibraryPath')))+'Langs\Messages.'+lc+'.lng';
  if fileexists(fn) then
  begin
    Ini:=TMemIniFile.Create(fn);
    try
      sec:='constants';
      LocalizeMsgDlgCaption(mtInformation,Ini.ReadString(sec,'lng_MsgDlgInformation',''));
      LocalizeMsgDlgCaption(mtConfirmation,Ini.ReadString(sec,'lng_MsgDlgConfirm',''));
      LocalizeMsgDlgCaption(mtWarning,Ini.ReadString(sec,'lng_MsgDlgWarning',''));
      LocalizeMsgDlgCaption(mtError,Ini.ReadString(sec,'lng_MsgDlgError','')); 
      LocalizeButtonCaption(mbYes,Ini.ReadString(sec,'lng_MsgBtnYes',''));  
      LocalizeButtonCaption(mbNo,Ini.ReadString(sec,'lng_MsgBtnNo',''));
      LocalizeButtonCaption(mbOK,Ini.ReadString(sec,'lng_MsgBtnOK',''));
      LocalizeButtonCaption(mbCancel,Ini.ReadString(sec,'lng_MsgBtnCancel',''));
      LocalizeButtonCaption(mbAbort,Ini.ReadString(sec,'lng_MsgBtnAbort',''));
      LocalizeButtonCaption(mbRetry,Ini.ReadString(sec,'lng_MsgBtnRetry',''));
      LocalizeButtonCaption(mbIgnore,Ini.ReadString(sec,'lng_MsgBtnIgnore',''));
      LocalizeButtonCaption(mbAll,Ini.ReadString(sec,'lng_MsgBtnAll',''));
      LocalizeButtonCaption(mbNoToAll,Ini.ReadString(sec,'lng_MsgBtnNoToAll',''));
      LocalizeButtonCaption(mbYesToAll,Ini.ReadString(sec,'lng_MsgBtnYesToAll',''));
      LocalizeButtonCaption(mbHelp,Ini.ReadString(sec,'lng_MsgBtnHelp',''));
      LocalizeButtonCaption(mbClose,Ini.ReadString(sec,'lng_MsgBtnClose',''));
    finally
      Ini.free;
    end;
  end; 
end;

