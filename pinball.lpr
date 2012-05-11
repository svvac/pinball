program pinball;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, ubouncingobject, ugamesignals, umovingobject, uobject, upoint,
  ushape, utils, objectcollection, uvector, callbackcollection, eventhandler,
  signal, comparable, hashtable, integerhash, stringhash;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

