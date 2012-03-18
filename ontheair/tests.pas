program tests;

{$mode objfpc}{$H+}

uses eventhandler, signal, classes, sysutils;

type
FooSignal = Class(oSignal)
    who: string;
end;
BarSignal = Class(oSignal);

Foo = Class(TObject)
    procedure sayAThing(s: oSignal);
end;

procedure Foo.sayAThing(s: oSignal);
var sig: FooSignal;
begin
    //if s.ClassType <> FooSignal then raise Exception.create('Handled invalid signal ' + s.getName());
    
    sig := s as FooSignal;
    writeln('Hello ' + sig.who + '!');
end;


var ed: oEventHandler;
    s1: FooSignal;
    s2: BarSignal;
    a: Foo;

begin
    ed := oEventHandler.create();

    a := Foo.create();
    
    s1 := FooSignal.create(ed);
    s2 := BarSignal.create(ed);

    ed.register(s1);
    ed.register(s2);

    ed.bind(s1, @a.sayAThing);
    ed.bind(s1, @a.sayAThing);
    ed.bind(s2, @a.sayAThing);

    s1.who := 'John';
    ed.emit(s1);

    s1.who := 'Jenny';
    ed.emit(s1);

    ed.emit(s2);


    writeln('done.');
end.

