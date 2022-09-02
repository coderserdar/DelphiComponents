cvtophlp -h btfiler.hxt btfwin.out
if errorlevel 1 goto Exit
helpc btfwin.out /r- /w31 /wd- /ep+
if errorlevel 1 goto Exit
copy btfwin.jmb btfwin.hpj
hcp btfwin.hpj
if errorlevel 1 goto Exit
if exist btfwin.rtf del btfwin.rtf
if exist btfwin.hpj del btfwin.hpj
if exist btfwin.out del btfwin.out
if exist btfwin.ph  del btfwin.ph
:Exit
