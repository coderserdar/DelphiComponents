cvtophlp btfiler.hxt btfiler.out
if errorlevel 1 goto Exit
hl -x -i -p -e100 btfiler.out -obtfiler.tph
if errorlevel 1 goto Exit
if exist btfiler.out del btfiler.out
if exist btfiler.ph  del btfiler.ph
:Exit
