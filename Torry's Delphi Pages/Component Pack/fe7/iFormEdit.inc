// in this file you can define iFormEditor library features

{$define FE_PopupMenu}
{$define FE_PropertyEditor}
{$define FE_EditorProperties}


{$IFDEF VER130}
  {$IFDEF BCB}			// C++Buider 5.0
  {$ObjExportAll on}
  {$ENDIF}

  {$DEFINE D4+}
  {$DEFINE D5+}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE D4+}
  {$DEFINE D5+}
  {$DEFINE D6+}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE D4+}
  {$DEFINE D5+}
  {$DEFINE D6+}
  {$DEFINE D7+}
{$ENDIF}

