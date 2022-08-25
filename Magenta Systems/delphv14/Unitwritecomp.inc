  WriteInteger (section, 'ArraySize_Value', ArraySize.Value) ;
  SaveSList (section + '.CodeMasks_Lines', CodeMasks.Lines) ;
  WriteString (section, 'FilePrefix_Text', FilePrefix.Text) ;
  SaveFont ('FontDialog_Font', FontDialog.Font) ;
  SaveSList (section + '.FormComps_Lines', FormComps.Lines) ;
  WriteString (section, 'OutDir_Text', OutDir.Text) ;
  SaveSList (section + '.OutFiles_Lines', OutFiles.Lines) ;
  SaveSList (section + '.SavedProps_Lines', SavedProps.Lines) ;
  SaveSList (section + '.SuppProps_Lines', SuppProps.Lines) ;
