@set params=
@if "%1"=="/r" @set params=/r

@set mask=*.txt/*.pas/*.inc/*.dpr/*.dpk/*.cpp/*.h/*.todo/*.bat/*.lst

@echo -
@echo trim space for files recursively (switch /r) or not
@echo for files: %mask%
@echo -
@echo Press any key to run
@pause >nul

call trim_file.exe %params% mask=%mask%

@echo Done.
