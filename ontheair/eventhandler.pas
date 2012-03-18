unit eventhandler;

interface

{$mode objfpc}{$H+}

{$UNITPATH ./hash}
{$UNITPATH .}

uses Classes, signal, stringhash, sysutils, callbackcollection;

type

ESignalUnknownException = class(Exception);
ESignalOverrideException = class(Exception);

oEventHandler = class(tObject)
    protected
        pKnownSignals: tStringHash;
    public
        constructor create();
        destructor destroy(); override;

        procedure register(sig: oSignal);
        procedure bind(sig: oSignal; c: tSignalCallback);
        procedure emit(sig: oSignal);
end;


implementation

constructor oEventHandler.create();
begin
    pKnownSignals := tStringHash.create(false);
end;

destructor oEventHandler.destroy();
begin
    pKnownSignals.deleteAll();
    pKnownSignals.free();
end;

procedure oEventHandler.register(sig: oSignal);
var c: oCallbackCollection;
begin
    if pKnownSignals.containsKey(sig.getName()) then
        raise ESignalOverrideException.create('Will override signal ' + sig.getName());
    
    c := oCallbackCollection.create();
    
    pKnownSignals.setValue(sig.getName(), c);
end;

procedure oEventHandler.bind(sig: oSignal; c: tSignalCallback);
var col: oCallbackCollection;
    o: TObject;
begin
    if not pKnownSignals.containsKey(sig.getName()) then
        raise ESignalUnknownException.create('No such signal ' + sig.getName());
    
    o := pKnownSignals.getValue(sig.getName());

    if o.ClassType <> oCallbackCollection then
        raise Exception.create('Inconsistency in object storage. Got ' + o.ClassName + ', expected oCallbackCollection');
    
    col := o as oCallbackCollection;
    
    col.push(c);
end;

procedure oEventHandler.emit(sig: oSignal);
var i: integer;
    c: oCallbackCollection;
    o: TObject;
begin
    if not pKnownSignals.containsKey(sig.getName()) then
        raise ESignalUnknownException.create('No such signal ' + sig.getName());
    
    o := pKnownSignals.getValue(sig.getName());

    if o.ClassType <> oCallbackCollection then
        raise Exception.create('Inconsistency in object storage. Got ' + o.ClassName + ', expected oCallbackCollection');
    
    c := o as oCallbackCollection;
    
    for i:= 0 to c.count() - 1 do begin
        c.get(i)(sig);
    end;
end;


end.