for %%i in (*.pas) do "C:\Borland\Delphi5\BIN\dcc32" %%i -M -N..\D5 -O..\D5 -R..\D5
"C:\Borland\Delphi5\BIN\dcc32" ZRpt5.dpk -M -N..\D5 -O..\D5 -R..\D5
"C:\Borland\Delphi5\BIN\dcc32" DclZRpt5.dpk -M -N..\D5 -O..\D5 -R..\D5
del ..\D5\ZRpt5.dcu
del ..\D5\DclZRpt5.dcu
copy ZRpt5.bpl ..\D5
copy DclZRpt5.bpl ..\D5
copy ZRpt5.dcp ..\D5
del *.bpl
del *.dcp

