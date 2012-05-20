unit utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, printable;

function max(a, b: integer) : integer;
function min(a, b: integer) : integer;

procedure d(i: integer; a, b: string);
procedure d(i: integer; a, b, c: string);

function verbosity() : integer;
procedure verbosity(i: integer);

function s(i: real) : string;
function s(i: integer) : string;
function s(i: iPrintable) : string;

implementation

var __debug_verbose: integer = 0;

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

procedure d(i: integer; a, b: string);
begin
    d(i, a, b, #10);
end;

procedure d(i: integer; a, b, c: string);
begin
    if i < verbosity() then write(a + ': ' + b + c);
end;

function verbosity() : integer;
begin
    verbosity := __debug_verbose;
end;

procedure verbosity(i: integer);
begin
    if i < 0 then __debug_verbose += i else __debug_verbose := i;
end;

function s(i: real) : string;
begin
    s := FloatToStr(i);
end;

function s(i: integer) : string;
begin
    s := IntToStr(i);
end;

function s(i: iPrintable) : string;
begin
    s := i.toString();
end;

end.
