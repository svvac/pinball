unit printable;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}  // Avoid useless instance counter shit

interface

uses
	// stdlib
    Classes
    ;

// Defines the printable interface
type iPrintable = interface
    function toString() : string;
end;

implementation

end.
