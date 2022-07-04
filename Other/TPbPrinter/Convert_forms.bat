@echo Converts all Delphi 5/6 forms (text) in this forlder to Delphi 3/4 binary forms.
@echo.
@echo Note: if you get an error: 'Bad command ..' edit the bat-file.
rename *.dfm *.tmp

@rem Add the full path to convert.exe in the line below if you get an error: 'Bad command....'
@rem Then rerun the bat-file.
@rem ------------------------------------------------------------------------------------------------------
convert.exe *.tmp
@rem ------------------------------------------------------------------------------------------------------

del *.tmp
@pause
