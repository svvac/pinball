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
function s(i: string) : string;

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

// d(i: integer; a, b: string)
// Displays the debug message `b' of verbosity `i', sent by `a' and terminated
// by a newline
procedure d(i: integer; a, b: string);
begin
    // Calls the real debug function, with a newline as line ending
    d(i, a, b, #10);
end;

// d(i: integer; a, b, c: string)
// Displays a debug message `b' of verbosity `i', sent by `a', and terminated
// by `c' (Typically a newline)
procedure d(i: integer; a, b, c: string);
begin
    if i <= verbosity() then write('[' + a + '] ' + b + c);
end;

// integer verbosity()
// Returns the current verbosity level. The higher the more verbose
function verbosity() : integer;
begin
    verbosity := __debug_verbose;
end;

// verbosity(i: integer)
// if `i' is positive or null, changes the current verbosity level to `i'
// if `i' is negative, add it to the current verbosity level (i.e. verbosity decrease)
procedure verbosity(i: integer);
begin
    if i < 0 then __debug_verbose += i else __debug_verbose := i;
end;

// Returns a string representation of an integer
function s(i: real) : string;
begin
    s := FloatToStr(i);
end;

// Returns a string representation of a float
function s(i: integer) : string;
begin
    s := IntToStr(i);
end;

// Returns a string representation of an object implementing the Printable interface
function s(i: iPrintable) : string;
begin
    s := i.toString();
end;

function s(i: string) : string;
begin
    s := i;
    case i of
        'alpha':                s := 'α';
        'beta':                 s := 'β';
        'gamma':                s := 'γ';
        'delta':                s := 'δ';
        'epsilon':              s := 'ε';
        'eta':                  s := 'η';
        'theta':                s := 'θ';
        'iota':                 s := 'ι';
        'kappa':                s := 'κ';
        'lambda':               s := 'λ';
        'mu':                   s := 'μ';
        'nu':                   s := 'ν';
        'zeta':                 s := 'ξ';
        'pi':                   s := 'π';
        'rho':                  s := 'ρ';
        'sigma':                s := 'σ';
        'tau':                  s := 'τ';
        'phi':                  s := 'φ';
        'khi':                  s := 'χ';
        'psi':                  s := 'ψ';
        'omega':                s := 'ω';
        '->':                   s := '→';
        '<-':                   s := '←';
        '=>':                   s := '⇒';
        '<=':                   s := '⇐';
    end;
end;

end.
