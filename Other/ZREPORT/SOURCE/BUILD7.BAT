set BDP=C:\Borland\Delphi7
set BDV=7
for %%i in (*.pas) do "%BDP%\BIN\dcc32" %%i     -M -N..\D%BDV% -O..\D%BDV% -R..\D%BDV%
"%BDP%\BIN\dcc32" ZRpt%BDV%.dpk    -M -N..\D%BDV% -O..\D%BDV% -R..\D%BDV%
"%BDP%\BIN\dcc32" DclZRpt%BDV%.dpk -M -N..\D%BDV% -O..\D%BDV% -R..\D%BDV%
del ..\D%BDV%\ZRpt%BDV%.dcu
del ..\D%BDV%\DclZRpt%BDV%.dcu
copy ZRpt%BDV%.bpl    ..\D%BDV%
copy DclZRpt%BDV%.bpl ..\D%BDV%
copy ZRpt%BDV%.dcp    ..\D%BDV%
del *.bpl
del *.dcp

