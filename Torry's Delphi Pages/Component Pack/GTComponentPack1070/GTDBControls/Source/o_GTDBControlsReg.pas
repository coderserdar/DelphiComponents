unit o_GTDBControlsReg;

interface
uses
   Classes
  ,o_GTDBGridExporter
  ,o_GTDBControls
  ,o_GTDBColumnManager
  ,o_GTDBLookUpEdit
  ;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GT DBControls',[
                                       TgtDBGridExporter
                                      ,TgtDBEdit
                                      ,TgtDBMemo
                                      ,TgtDBImage
                                      ,TgtDBListBox
                                      ,TgtDBComboBox
                                      ,TgtDBLookUpListBox
                                      ,TgtDBLookUpComboBox
                                      ,TgtDBRichEdit
                                      ,TgtDBProgressBar
                                      ,TgtDBTrackBar
                                      ,TgtDBUpDown
                                      ,TgtDBShape
                                      ,TgtDBGauge
                                      ,TgtDBDateTimePicker
                                      ,TgtDBColumnManager
                                      ,TgtDBLookUpEdit
                                     ]
                                     )
                                     ;
end;

end.
