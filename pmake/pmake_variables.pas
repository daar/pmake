unit pmake_variables;

{$mode objfpc}{$H+}

interface

uses
  XMLConf;

type
  PMK_type = (ptBoolean, ptInteger, ptFloat, ptString, ptFileCache);

  pPMK_Link = ^PMK_Link;
  PMK_Link = record
    next, prev: pointer;
  end;

  pPMK_FileCache = ^PMK_FileCache;
  PMK_FileCache = record
    next, prev: pointer;
    path: PChar;
    crc: word;
  end;

  pPMK_variant = ^PMK_variant;
  PMK_variant = record
    next, prev: pointer;
    vtype: PMK_type;
    name: shortstring;
    case word of
      ptBoolean:   (PM_Boolean  : boolean);
      ptInteger:   (PM_Integer  : integer);
      ptFloat:     (PM_Float    : double);
      ptString:    (PM_String   : shortstring);
      ptFileCache: (PM_FileCache: pointer);
  end;

  PMK_ListBase = record
    first, last: pointer;
  end;

  PMK_Bool = (_OFF_, _ON_);

var
  varlist: PMK_ListBase;

const
  UNKNOWN          = $00;
  DEBUG            = $01;
  RELEASE          = $02;

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

procedure set_(name: shortstring; value: boolean);
procedure set_(name: shortstring; value: integer);
procedure set_(name: shortstring; value: shortstring);
procedure set_(name: shortstring; value: double);

function val_(name: shortstring): shortstring;
function val_(name: shortstring): boolean;
function val_(name: shortstring): integer;
function val_(name: shortstring): double;

procedure option(option_variable, description: shortstring; initial_value: PMK_Bool);
function option(option_variable: shortstring): boolean;

procedure replace_variable_macros(var tmp: string);

procedure pmakecache_write;
procedure pmakecache_read;

function find_variable(name: shortstring): pointer;

var
  cache: TXMLConfig;

implementation

uses
  Classes, SysUtils, crc16, pmake_utilities, compiler;

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

function RemoveSpecialChars(const str: shortstring): shortstring;
const
  InvalidChars: set of char = [',', '.', '/', '!', '@', '#', '$', '%',
                               '^', '&', '*', '''', '"', ';', '(', ')',
                               ':', '|', '[', ']'];
var
  i: cardinal;
begin
  Result := '';
  for i := 1 to Length(str) do
    if not (str[i] in InvalidChars) then
      Result := Result + str[i];
end;

procedure clean_variable_name(var name: shortstring);
var
  tmp: string;
  len: cardinal;
begin
  tmp := Trim(name);
  if pos('$(', tmp) = 1 then
    Delete(tmp, 1, 2);

  len := length(tmp);
  if pos(')', tmp) = len then
    Delete(tmp, len, 1);

  name := RemoveSpecialChars(tmp);
end;

procedure delete_variable(name: shortstring);
var
  v: pPMK_variant;
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

function find_variable(name: shortstring): pointer;
var
  v: pPMK_variant;
begin
  clean_variable_name(name);

  v := varlist.first;

  while v <> nil do
  begin
    if v^.name = name then
      exit(v);

    v := v^.next;
  end;

  exit(nil);
end;

function create_variable(name: shortstring): pPMK_variant;
var
  v: pPMK_variant;
begin
  clean_variable_name(name);

  v := find_variable(name);

  //delete if exists to prevent multiple instances of the same variable
  if v <> nil then
    delete_variable(name);

  v := callocN(sizeof(PMK_variant));
  v^.name := name;

  exit(v);
end;

procedure set_(name: shortstring; value: boolean);
var
  v: pPMK_variant;
begin
  v := create_variable(name);

  //add data to variant
  v^.vtype := ptBoolean;
  v^.PM_Boolean := value;

  //add item to bottom of list
  addtail(v);
end;

procedure set_(name: shortstring; value: integer);
var
  v: pPMK_variant;
begin
  v := create_variable(name);

  //add data to variant
  v^.vtype := ptInteger;
  v^.PM_Integer := value;

  //add item to bottom of list
  addtail(v);
end;

procedure set_(name: shortstring; value: shortstring);
var
  v: pPMK_variant;
begin
  v := create_variable(name);

  //add data to variant
  v^.vtype := ptString;
  v^.PM_String := value;

  //add item to bottom of list
  addtail(v);
end;

procedure set_(name: shortstring; value: double);
var
  v: pPMK_variant;
begin
  v := create_variable(name);

  //add data to variant
  v^.vtype := ptFloat;
  v^.PM_Float := value;

  //add item to bottom of list
  addtail(v);
end;

function val_(name: shortstring): boolean;
var
  v: pPMK_variant;
begin
  v := find_variable(name);

  if v <> nil then
    exit(v^.PM_Boolean)
  else
    exit(False);
end;

function val_(name: shortstring): integer;
var
  v: pPMK_variant;
begin
  v := find_variable(name);

  if v <> nil then
    exit(v^.PM_Integer)
  else
    exit(0);
end;

function val_(name: shortstring): shortstring;
var
  v: pPMK_variant;
begin
  v := find_variable(name);

  if v <> nil then
    exit(v^.PM_String)
  else
    exit('');
end;

function val_(name: shortstring): double;
var
  v: pPMK_variant;
begin
  v := find_variable(name);

  if v <> nil then
    exit(v^.PM_Float)
  else
    exit(0);
end;

procedure option(option_variable, description: shortstring; initial_value: PMK_Bool);
begin
  case initial_value of
    _OFF_: set_(option_variable, False);
    _ON_ : set_(option_variable, True);
  end;
end;

function option(option_variable: shortstring): boolean;
var
  v: pPMK_variant;
begin
  v := find_variable(option_variable);

  if v <> nil then
    exit(False)
  else
  begin
    if v^.vtype <> ptBoolean then
      exit(False)
    else
      exit(v^.PM_Boolean);
  end;
end;

procedure replace_variable_macros(var tmp: string);
var
  v: pPMK_variant;
begin
  v := varlist.first;

  while v <> nil do
  begin
    case v^.vtype of
      ptBoolean:
        tmp := StringReplace(tmp, '$(' + v^.name + ')', BoolToStr(v^.PM_Boolean), [rfReplaceAll]);
      ptInteger:
        tmp := StringReplace(tmp, '$(' + v^.name + ')', IntToStr(v^.PM_Integer), [rfReplaceAll]);
      ptFloat:
        tmp := StringReplace(tmp, '$(' + v^.name + ')', FloatToStr(v^.PM_Float), [rfReplaceAll]);
      ptString:
        tmp := StringReplace(tmp, '$(' + v^.name + ')', v^.PM_String, [rfReplaceAll]);
    end;

    v := v^.next;
  end;
end;

function PMAKE_VERSION: string; inline;
begin
  if PMAKE_TWEAK_VERSION <> 0 then
    PMAKE_VERSION := Format('%d.%d.%d.%d', [PMAKE_MAJOR_VERSION, PMAKE_MINOR_VERSION, PMAKE_PATCH_VERSION, PMAKE_TWEAK_VERSION])
  else
    PMAKE_VERSION := Format('%d.%d.%d', [PMAKE_MAJOR_VERSION, PMAKE_MINOR_VERSION, PMAKE_PATCH_VERSION]);
end;

procedure pmakecache_write;
var
  pmakecrc: word;
  f: TStrings;
  i: integer;
  v: pPMK_variant;
begin
  cache.Clear;

  //write all variables to cache
  v := varlist.first;

  while v <> nil do
  begin
    case v^.vtype of
      ptBoolean:
      begin
        cache.SetValue(WideString(v^.name + '/type'), 'boolean');
        cache.SetValue(WideString(v^.name + '/value'), v^.PM_Boolean);
      end;
      ptInteger:
      begin
        cache.SetValue(WideString(v^.name + '/type'), 'integer');
        cache.SetValue(WideString(v^.name + '/value'), v^.PM_Integer);
      end;
      ptFloat:
      begin
        cache.SetValue(WideString(v^.name + '/type'), 'float');
        cache.SetValue(WideString(v^.name + '/value'), WideString(FloatToStr(v^.PM_Float)));
      end;
      ptString:
      begin
        cache.SetValue(WideString(v^.name + '/type'), 'string');
        cache.SetValue(WideString(v^.name + '/value'), WideString(v^.PM_String));
      end;
    end;

    v := v^.next;
  end;

  //todo: rewrite this part
  if pmakefiles <> nil then
  begin
    cache.SetValue('PMake/count', pmakefiles.Count);
    cache.SetValue('PMake/type', 'filecache');
    f := TStringList.Create;
    for i := 0 to pmakefiles.Count - 1 do
    begin
      f.LoadFromFile(pPMakeItem(pmakefiles[i])^.fname);
      pmakecrc := crc_16(@f.Text[1], length(f.Text));

      cache.SetValue(WideString(Format('PMake/item%d/filename', [i + 1])), WideString(pmakefiles[i]));
      cache.SetValue(WideString(Format('PMake/item%d/crc', [i + 1])), pmakecrc);
    end;
    f.Free;
  end;

  cache.Flush;

  OutputLn('-- PMakeCache.txt file has been written');
end;

procedure pmakecache_read;
begin
  //todo: rewrite this part, see pmakecache_write
  //see: http://wiki.lazarus.freepascal.org/XML_Tutorial#Usage_Examples
  set_('PMAKE_SOURCE_DIR', shortstring(cache.GetValue('PMAKE_SOURCE_DIR/value', '')));
  set_('PMAKE_CURRENT_SOURCE_DIR', shortstring(cache.GetValue('PMAKE_SOURCE_DIR/value', '')));
  set_('PMAKE_PAS_COMPILER', shortstring(cache.GetValue('PMAKE_PAS_COMPILER/value', '')));

  set_('PMAKE_TOOL_DIR', shortstring(cache.GetValue('PMAKE_TOOL_DIR/value', '')));

  set_('PMAKE_BINARY_DIR', shortstring(cache.GetValue('PMAKE_BINARY_DIR/value', '')));
  set_('PMAKE_CURRENT_BINARY_DIR', shortstring(cache.GetValue('PMAKE_BINARY_DIR/value', '')));

  set_('PMAKE_PAS_COMPILER_VERSION', shortstring(cache.GetValue('PMAKE_PAS_COMPILER_VERSION/value', '')));
  set_('PMAKE_HOST_SYSTEM_PROCESSOR', shortstring(cache.GetValue('PMAKE_HOST_SYSTEM_PROCESSOR/value', '')));
  set_('PMAKE_HOST_SYSTEM_NAME', shortstring(cache.GetValue('PMAKE_HOST_SYSTEM_NAME/value', '')));
end;

end.
