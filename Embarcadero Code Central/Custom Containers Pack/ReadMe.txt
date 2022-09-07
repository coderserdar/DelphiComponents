Custom Containers Pack (CCPack)
for 
Delphi 5

Sergey Orlik
product manager
Inprise Representative Office (Moscow)
Russia, C.I.S. and Baltic States

E-mail: sorlik@inprise.ru
WWW: http://www.inprise.ru
Personal Home Page: http://www.geocities.com/SiliconValley/Way/9006/index.html

Custom Containers Pack (CCPack) : Copyright (c) 1997-99 Sergey Orlik.
Introduction


The next logical step in evolution of VCL IDEs (Delphi and C++Builder) is a real Visual Composite Component and Container Creation. Custom Containers Pack (CCPack)

 is created to initiate the new wave of the next generation 3rd party VCL libraries with a lot of new components. 

CCPack is the next generation of two well-known Delphi add-ins: Custom Forms Pack and Composite Components Pack.
1. Purposes
To provide an easy mechanism for compound existent VCL into the new native VCL components.
To provide an easy mechanism for creating new base form and data module classes with new published properties and events.

   

With CCPack developers have an answer to discussion “Inheritance vs. Composition in CBD (Component Based Development)”. Delphi support both of them. 
2. Details
The technology of Custom Modules allows to produce real Composite Components (aka compound components and super-components) and Container (Form/Data Module/Frame) Inheritance.

In defference with well-known form’s .dfm-resource conversation technology, the using of Custom Modules with predefined composite containers allows to produce rich composites with abilities of redesign them in design-time without code loss.
3. Solution
Custom Containers Pack (CCPack)

 is an integrated tool and component mini-library to produce and maintain composite controls (or simply “composites”) and other containers (forms, data modules and frames). The process of building composite components looks like ActiveForm and Frame creating, but the result is the native VCL component. You can create new composites just as usual forms.

CCPack is an expert tool which allows to easy inherit TForm and TDataModule with custom published properties and events and use them in Delphi and C++Builder visual form's designer.


CCPack provides an ingenious and easy way to effectively create a native VCL compound component. Now Delphi and C++Builder developers can visually create Composite Components by simply dragging and dropping existing components onto a one of 3 predefined containers and frames:


Container Class	Types of Custom Controls
 TBox	This class descends from TCustomPanel. It is the base class for most compound components.
 TControlGroupBox	Use this class (descendant of TcustomGroupBox) when you want to have to create a compound component with Caption on the border line (looks like the RadioGroup).
 TControlScrollBox	This class descends from TScrollBox. It is the base scrolling container for compound controls. 
ToolBarBox introduced in previous versions of CCPack is not supported in CCPack for Delphi 5. This is related with changes in behaviour in the latest versions of common controls and VCL library.

CCPack for Delphi 5 provides single design-time package ccpack50. It contains 6 units. The most important units for container components developer are:
Boxes.pas - implements 3 base composite containers
CCReg provides a special registration routines for custom container classes
4. Using
Install ccpack50 design-time package to the Delphi 5 IDE. The Custom Container Wizard is then available using File | New command, wich opens the New Items dialog box:


When you open new container instance, Delphi displays a graphical representation of the container. You can add new properties and event-handlers to containers in code manually. Published properties for composite containers will be visible in Object Inspector after registering  composite container in IDE as an original VCL component (use standard RegisterComponents routine). Now you can use your own composite from the Delphi Palette. If you want to publish properties/events for your form or data module to show them in the Object Inspector you need register class (not instance!) via using RegisterCustomContainer routine. 
5. Notes
You can create your own non-visual composites based on TDataModule.Custom data-modules may be registered in IDE’s Component Palette with RegisterComponent routine without any problems because TDataModule is a descendant of TComponent.  



Change default IDE-generated names for sub-components placed on composite container to avoid any possible naming conflicts. 



You can manually delegate any properties and methods of sub-components by creating new properties and methods for composite itself. 
