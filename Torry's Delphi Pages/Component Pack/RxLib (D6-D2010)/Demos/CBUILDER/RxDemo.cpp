//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#include "vclutils.hpp"
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("mainunit.cpp", RxDemoMainForm);
USERES("RxDemo.res");
USEFORM("About.cpp", AboutForm);
USEFORM("ctrls.cpp", ControlsForm);
USEFORM("DBAware.cpp", DBAwareForm);
USEUNIT("LinkUnit.cpp");
USEFORM("Tools.cpp", ToolsForm);
USEFORM("PageDemo.cpp", ClientAssistant);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 if (!ActivatePrevInstance(__classid(TRxDemoMainForm)->ClassName(),
                   EmptyStr)) /* allows only one instance of application */
                 {
                 Application->Initialize();
                 Application->Title = "RX Library Demo";
                 Application->CreateForm(__classid(TRxDemoMainForm), &RxDemoMainForm);
                 Application->Run();
                 }
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
