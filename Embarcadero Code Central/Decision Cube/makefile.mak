suf=$(v)0
bin=bin
lib=lib$(suf)

.path.bpl=$(bin)
.path.dfm=$(lib)
.path.res=$(lib)

#------------------------------------------------------------------------------
OPT = -B -LE$(bin) -W -N$(lib) -LN$(lib) -GD
DCC = "C:\Program Files\Borland\Delphi$(v)\Bin\dcc32.exe"
#------------------------------------------------------------------------------
FORMS = FxDCube.dfm FxDimEdt.dfm FxPBar.dfm
RES   = FxConsts.res

all: compile $(FORMS) $(RES)

compile:
  $(DCC) $(OPT) fds$(suf).dpk
  $(DCC) $(OPT) dclfds$(suf).dpk

{src}.res.res:
  @copy $< $@
{src}.dfm.dfm:
  @copy $< $@
