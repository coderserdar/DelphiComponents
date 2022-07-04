cls
@echo ### Cleaning up the source dirs... ###

cd Example
del *.~* *.dcu *.ddp

cd..

cd Component
del *.~* *.dcu *.ddp

cd..

cd DBConverter
del *.~* *.dcu *.ddp

cd..

cd VerySimpleExample
del *.~* *.dcu *.ddp

cd..

@echo.
@echo ### Cleaning finished! ###
@echo.
@echo ### Press any key! ###
@pause > nul