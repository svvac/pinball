unit printable;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils;

type iPrintable = interface
    function toString() : string;
end;

implementation

end.
