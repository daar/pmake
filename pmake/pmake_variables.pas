unit pmake_variables;

{$mode objfpc}{$H+}

interface

const
  _ON              = True;
  _OFF             = False;

  UNKNOWN          = $00;
  DEBUG            = $01;
  RELEASE          = $02;

const
  //major version number for PMake, e.g. the "2" in PMake 2.4.3
  PMAKE_MAJOR_VERSION = 0;
  //minor version number for PMake, e.g. the "4" in PMake 2.4.3
  PMAKE_MINOR_VERSION = 0;
  //patch version number for PMake, e.g. the "3" in PMake 2.4.3
  PMAKE_PATCH_VERSION = 2;
  //tweak version number for PMake, e.g. the "1" in PMake X.X.X.1. Releases use tweak < 20000000 and development versions use the date format CCYYMMDD for the tweak level.
  PMAKE_TWEAK_VERSION = 0;

//The version number combined, eg. 2.8.4.20110222-ged5ba for a Nightly build. or 2.8.4 for a Release build.
function PMAKE_VERSION: string; inline;

type
  string255 = string[255];

procedure set_(name: string255; value: boolean);
procedure set_(name: string255; value: integer);
procedure set_(name: string255; value: string255);
procedure set_(name: string255; value: double);

function val_(name: string255): string255;
function val_(name: string255): boolean;
function val_(name: string255): integer;
function val_(name: string255): double;

procedure replace_variable_macros(var tmp: string);

procedure pmakecache_write;
procedure pmakecache_read;

implementation

uses
  Classes, SysUtils, crc, XMLConf, pmake_utilities;

type
  PMK_type = (ptBoolean, ptInteger, ptString, ptDouble);

  pPMK_Link = ^PMK_Link;

  PMK_Link = record
    next, prev: pointer;
  end;

  pPMK_boolean = ^PMK_boolean;

  PMK_boolean = record
    next, prev: pointer;
    type_: PMK_type;
    name: string255;
    value: boolean;
  end;

  pPMK_integer = ^PMK_integer;

  PMK_integer = record
    next, prev: pointer;
    type_: PMK_type;
    name: string255;
    value: integer;
  end;

  pPMK_string = ^PMK_string;

  PMK_string = record
    next, prev: pointer;
    type_: PMK_type;
    name: string255;
    value: string;
  end;

  pPMK_double = ^PMK_double;

  PMK_double = record
    next, prev: pointer;
    type_: PMK_type;
    name: string255;
    value: double;
    Count: integer;
  end;

  pPMK_ListBase = ^PMK_ListBase;

  PMK_ListBase = record
    first, last: pointer;
  end;

var
  cache: TXMLConfig;
  varlist: PMK_ListBase;

function callocN(Size: PtrUInt): pointer;
var
  p: pointer;
begin
  p := GetMem(Size);
  FillByte(p^, Size, 0);
  exit(p);
end;

procedure remlink(vlink: pointer);
var
  link: pPMK_Link;
begin
  link := pPMK_Link(vlink);

  if link = nil then
    exit;

  if link^.next <> nil then
    pPMK_Link(link^.next)^.prev := link^.prev;

  if link^.prev <> nil then
    pPMK_Link(link^.prev)^.next := link^.next;

  if pointer(varlist.last) = pointer(link) then
    varlist.last := link^.prev;

  if pointer(varlist.first) = pointer(link) then
    varlist.first := link^.next;
end;

procedure addtail(vlink: pointer);
var
  link: pPMK_Link;
begin
  link := pPMK_Link(vlink);

  if link = nil then
    exit;

  link^.next := nil;
  link^.prev := varlist.last;

  if varlist.last <> nil then
    pPMK_Link(varlist.last)^.next := link;

  if varlist.first = nil then
    varlist.first := link;

  varlist.last := link;
end;

procedure delete_variable(name: string255);
var
  v: pPMK_boolean;
begin
  v := varlist.first;

  while v <> nil do
  begin
    if v^.name = name then
    begin
      remlink(v);
      exit;
    end;

    v := v^.next;
  end;
end;

function find_variable(name: string255): pointer;
var
  v: pPMK_boolean;
begin
  v := varlist.first;

  while v <> nil do
  begin
    if v^.name = name then
      exit(v);

    v := v^.next;
  end;

  exit(nil);
end;

procedure set_(name: string255; value: boolean);
var
  v: pPMK_boolean;
begin
  v := find_variable(name);

  //if it already exists, then delete first
  if v <> nil then
    delete_variable(name);

  v := GetMem(sizeof(PMK_boolean));

  //add data to list
  v^.type_ := ptBoolean;
  v^.name := name;
  v^.value := value;

  //add item to bottom of list
  addtail(v);
end;

procedure set_(name: string255; value: integer);
var
  v: pPMK_integer;
begin
  v := find_variable(name);

  //if it already exists, then delete first
  if v <> nil then
    delete_variable(name);

  v := GetMem(sizeof(PMK_integer));

  //add data to list
  v^.type_ := ptInteger;
  v^.name := name;
  v^.value := value;

  //add item to bottom of list
  addtail(v);
end;

procedure set_(name: string255; value: string255);
var
  v: pPMK_string;
begin
  v := find_variable(name);

  //if it already exists, then delete first
  if v <> nil then
    delete_variable(name);

  v := callocN(sizeof(PMK_string));

  //add data to list
  v^.type_ := ptString;
  v^.name := name;
  v^.value := value;

  //add item to bottom of list
  addtail(v);
end;

procedure set_(name: string255; value: double);
var
  v: pPMK_double;
begin
  v := find_variable(name);

  //if it already exists, then delete first
  if v <> nil then
    delete_variable(name);

  v := callocN(sizeof(PMK_double));

  //add data to list
  v^.type_ := ptDouble;
  v^.name := name;
  v^.value := value;

  //add item to bottom of list
  addtail(v);
end;

function val_(name: string255): boolean;
var
  v: pPMK_boolean;
begin
  v := find_variable(name);

  if v <> nil then
    exit(v^.value)
  else
    exit(False);
end;

function val_(name: string255): integer;
var
  v: pPMK_integer;
begin
  v := find_variable(name);

  if v <> nil then
    exit(v^.value)
  else
    exit(0);
end;

function val_(name: string255): string255;
var
  v: pPMK_string;
begin
  v := find_variable(name);

  if v <> nil then
    exit(v^.value)
  else
    exit('');
end;

function val_(name: string255): double;
var
  v: pPMK_double;
begin
  v := find_variable(name);

  if v <> nil then
    exit(v^.value)
  else
    exit(0);
end;

procedure replace_variable_macros(var tmp: string);
var
  v: pPMK_boolean;
begin
  v := varlist.first;

  while v <> nil do
  begin
    case v^.type_ of
      ptBoolean:
        tmp := StringReplace(tmp, '$(' + v^.name + ')', BoolToStr(v^.value), [rfReplaceAll]);
      ptInteger:
        tmp := StringReplace(tmp, '$(' + v^.name + ')', IntToStr(pPMK_integer(v)^.value),
          [rfReplaceAll]);
      ptString:
        tmp := StringReplace(tmp, '$(' + v^.name + ')', pPMK_string(v)^.value, [rfReplaceAll]);
      ptDouble:
        tmp := StringReplace(tmp, '$(' + v^.name + ')', FloatToStr(pPMK_double(v)^.value),
          [rfReplaceAll]);
    end;

    v := v^.next;
  end;
end;

function PMAKE_VERSION: string; inline;
begin
  if PMAKE_TWEAK_VERSION <> 0 then
    PMAKE_VERSION := Format('%d.%d.%d.%d', [PMAKE_MAJOR_VERSION,
      PMAKE_MINOR_VERSION, PMAKE_PATCH_VERSION, PMAKE_TWEAK_VERSION])
  else
    PMAKE_VERSION := Format('%d.%d.%d', [PMAKE_MAJOR_VERSION,
      PMAKE_MINOR_VERSION, PMAKE_PATCH_VERSION]);
end;

function FREEPASCAL: boolean; inline;
begin
  exit(true);
  {$note need to implement!}
end;

procedure pmakecache_write;
var
  pmakecrc: cardinal;
  f: TStrings;
  i: integer;
  tmp: string;
begin
  cache := TXMLConfig.Create(nil);

  cache.FileName := 'PMakeCache.txt';
  cache.Clear;

  //write PMake.txt data to cache
  if pmakefiles <> nil then
  begin
    cache.Setvalue('PMake/count', pmakefiles.Count);
    f := TStringList.Create;
    for i := 0 to pmakefiles.Count - 1 do
    begin
      f.LoadFromFile(pmakefiles[i]);
      pmakecrc := crc32(0, @f.Text[1], length(f.Text));
      str(pmakecrc: 10, tmp);

      cache.Setvalue(Format('PMake/item%s/path', [i + 1]), pmakefiles[i]);
      cache.Setvalue(Format('PMake/item%s/crc', [i + 1]), pmakecrc);
    end;
    f.Free;
  end;

  cache.Setvalue('PMAKE_SOURCE_DIR/value', val_('PMAKE_SOURCE_DIR'));
  cache.Setvalue('PMAKE_BINARY_DIR/value', val_('PMAKE_BINARY_DIR'));

  cache.Setvalue('PMAKE_PAS_COMPILER_VERSION/value', val_('PMAKE_PAS_COMPILER_VERSION'));
  cache.Setvalue('PMAKE_HOST_SYSTEM_PROCESSOR/value', val_('PMAKE_HOST_SYSTEM_PROCESSOR'));
  cache.Setvalue('PMAKE_HOST_SYSTEM_NAME/value', val_('PMAKE_HOST_SYSTEM_NAME'));

  cache.Flush;
  cache.Free;
end;

procedure pmakecache_read;
var
  pmakecrc: cardinal;
  f: TStrings;
  i: integer;
  tmp: string;
begin
  cache := TXMLConfig.Create(nil);
  cache.LoadFromFile('PMakeCache.txt');

  set_('PMAKE_SOURCE_DIR', cache.Getvalue('PMAKE_SOURCE_DIR/value', ''));
  set_('PMAKE_CURRENT_SOURCE_DIR', cache.Getvalue('PMAKE_SOURCE_DIR/value', ''));

  set_('PMAKE_BINARY_DIR', cache.Getvalue('PMAKE_BINARY_DIR/value', ''));
  set_('PMAKE_CURRENT_BINARY_DIR', cache.Getvalue('PMAKE_BINARY_DIR/value', ''));

  set_('PMAKE_PAS_COMPILER_VERSION', cache.Getvalue('PMAKE_PAS_COMPILER_VERSION/value', ''));
  set_('PMAKE_HOST_SYSTEM_PROCESSOR', cache.Getvalue('PMAKE_HOST_SYSTEM_PROCESSOR/value', ''));
  set_('PMAKE_HOST_SYSTEM_NAME', cache.Getvalue('PMAKE_HOST_SYSTEM_NAME/value', ''));

  cache.Free;
end;

end.
