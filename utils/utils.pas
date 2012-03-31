unit utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function max(a, b: integer) : integer;
function min(a, b: integer) : integer;

implementation

// integer max(a, b: integer)
// returns the max between a and b
function max(a, b: integer) : integer;
begin
    if a > b then max := a
             else max := b;
end;

// integer min(a, b: integer)
// returns the min between a and b
function min(a, b: integer) : integer;
begin
    if a < b then min := a
             else min := b;
end;

end.
