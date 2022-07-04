@rem "UPX" executable packer. Home: http://upx.sf.net  http://sf.net/projects/upx

@set rate=%2
@if "%rate%"=="" set rate=8

@del /Q %1.ex~ >nul 2>nul
@set upx_ot=--no-env -%rate% --crp-ms=999999 --nrv2d --force
@call upx.exe %upx_ot% -k %1.exe
@del /Q %1.ex~ >nul 2>nul
