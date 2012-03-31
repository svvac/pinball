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

// create(void)
// Initializes an EventHandler
constructor oEventHandler.create();
begin
    // Create the string hash mapping signal names to callbacks
    pKnownSignals := tStringHash.create(false);
end;

// destroy(void)
// Makes sure we free used memory on object destruction
destructor oEventHandler.destroy();
begin
    pKnownSignals.deleteAll();
    pKnownSignals.free();
end;


// register(sig: oSignal)
// Registers the signal type `sig' to the event handler, allowing methods to listen for it
procedure oEventHandler.register(sig: oSignal);
var c: oCallbackCollection;
begin
    // Make sure no signal with the same ID is already defined.
    // We use the class name to ID a signal
    if pKnownSignals.containsKey(sig.getName()) then
        raise ESignalOverrideException.create('Will override signal ' + sig.getName());
    
    // Create a callback collection to store methods listening for this signal
    c := oCallbackCollection.create();
    
    pKnownSignals.setValue(sig.getName(), c);
end;

// bind(sig: oSignal, c: tSignalCallback)
// Sets the callback method `c' as a listener for the signal `sig'
procedure oEventHandler.bind(sig: oSignal; c: tSignalCallback);
var col: oCallbackCollection;
    o: TObject;
begin
    // Check if the signal is properly registred
    if not pKnownSignals.containsKey(sig.getName()) then
        raise ESignalUnknownException.create('No such signal ' + sig.getName());
    
    o := pKnownSignals.getValue(sig.getName());

    // Ensure we got a oCallbackCollection
    if o.ClassType <> oCallbackCollection then
        raise Exception.create('Inconsistency in object storage. Got ' + o.ClassName + ', expected oCallbackCollection');
    
    col := o as oCallbackCollection;
    
    col.push(c);
end;

// emit(sig: oSignal)
// Emits the signal `sig' and call all registred listener methods.
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
        c.get(i)(sig);   // Calls the callback c.get(i) with `sig' as an argument
    end;
end;


end.