for %%i in (*.pas) do "C:\Borland\Delphi4\BIN\dcc32" %%i -M -N..\D4 -O..\D4 -R..\D4
"C:\Borland\Delphi4\BIN\dcc32" ZRpt4.dpk -M -N..\D4 -O..\D4 -R..\D4
"C:\Borland\Delphi4\BIN\dcc32" DclZRpt4.dpk -M -N..\D4 -O..\D4 -R..\D4
del ..\D4\ZRpt4.dcu
del ..\D4\DclZRpt4.dcu
copy ZRpt4.bpl ..\D4
copy DclZRpt4.bpl ..\D4
copy ZRpt4.dcp ..\D4
del *.bpl
del *.dcp

