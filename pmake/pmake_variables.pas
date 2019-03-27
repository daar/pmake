unit pmake_variables;

{$mode objfpc}{$H+}

interface

uses
  pmake_utilities;

type
  PMK_type = (ptBoolean, ptInteger, ptFloat, ptString, ptFileCache);

  pPMK_FileCache = ^PMK_FileCache;
  PMK_FileCache = record
    next, prev: pointer;
    fname: shortstring;
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
      ptFileCache: (PM_FileCache: pPMK_ListBase);
  end;

  PMK_Bool = (_OFF_ = 0, _ON_ = 1);

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
  PMAKE_PATCH_VERSION = 3;
  //tweak version number for PMake, e.g. the "1" in PMake X.X.X.1. Releases use tweak < 20000000 and development versions use the date format CCYYMMDD for the tweak level.
  PMAKE_TWEAK_VERSION = 0;

//The version number combined, eg. 2.8.4.20110222-ged5ba for a Nightly build. or 2.8.4 for a Release build.
function PMAKE_VERSION: string; inline;

procedure set_(name: shortstring; value: boolean);
procedure set_(name: shortstring; value: integer);
procedure set_(name: shortstring; value: shortstring);
procedure set_(name: shortstring; value: double);
procedure set_(name: shortstring; value: pPMK_ListBase);

function vals(name: shortstring): shortstring;
function valb(name: shortstring): boolean;
function vali(name: shortstring): integer;
function vald(name: shortstring): double;
function VALfc(name: shortstring): pPMK_ListBase;

procedure option(option_variable, description: shortstring; initial_value: PMK_Bool);
function option(option_variable: shortstring): boolean;

procedure replace_variable_macros(var tmp: string);

procedure pmakecache_write;
procedure pmakecache_read;

function find_variable(name: shortstring): pointer;

implementation

uses
  DOM,
  XMLRead,
  XMLWrite,
  Classes, SysUtils, crc16, pmake_api, compiler;

function RemoveSpecialChars(const str: shortstring): shortstring;
const
  InvalidChars: set of char =
    [',', '.', '/', '!', '@', '#', '$', '%', '^',
    '&', '*', '''', '"', ';', '(', ')', ':', '|', '[', ']'];
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
  len: SizeInt;
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
  addtail(@varlist, v);
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
  addtail(@varlist, v);
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
  addtail(@varlist, v);
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
  addtail(@varlist, v);
end;

procedure set_(name: shortstring; value: pPMK_ListBase);
var
  v: pPMK_variant;
begin
  v := create_variable(name);

  //add data to variant
  v^.vtype := ptFileCache;
  v^.PM_FileCache := value;

  //add item to bottom of list
  addtail(@varlist, v);
end;

function valb(name: shortstring): boolean;
var
  v: pPMK_variant;
begin
  v := find_variable(name);

  if v <> nil then
    exit(v^.PM_Boolean)
  else
    exit(False);
end;

function vali(name: shortstring): integer;
var
  v: pPMK_variant;
begin
  v := find_variable(name);

  if v <> nil then
    exit(v^.PM_Integer)
  else
    exit(0);
end;

function vals(name: shortstring): shortstring;
var
  v: pPMK_variant;
begin
  v := find_variable(name);

  if v <> nil then
    exit(v^.PM_String)
  else
    exit('');
end;

function vald(name: shortstring): double;
var
  v: pPMK_variant;
begin
  v := find_variable(name);

  if v <> nil then
    exit(v^.PM_Float)
  else
    exit(0);
end;

function VALfc(name: shortstring): pPMK_ListBase;
var
  v: pPMK_variant;
begin
  v := find_variable(name);

  if v <> nil then
    exit(v^.PM_FileCache)
  else
    exit(nil);
end;

procedure option(option_variable, description: shortstring; initial_value: PMK_Bool);
begin
  case initial_value of
    _OFF_: set_(option_variable, False);
    _ON_ : set_(option_variable, True);
  end;

  set_(option_variable + '_DESCRIPTION', description);
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

var
  doc: TXMLDocument;
  root: TDOMElement;

procedure write_pmakecache_init;
begin
  if doc = nil then
    doc := TXMLDocument.Create;

  //create the root node
  root := doc.CreateElement('PMakeCache');
  doc.AppendChild(root);
end;

procedure write_pmakecache_finish;
begin
  //write to XML
  writeXMLFile(doc, 'PMakeCache.txt');

  FreeAndNil(doc);
end;

procedure write_boolean_pmakecache(variable: string; value: boolean);
var
  elem: TDOMElement;
begin
  elem := doc.CreateElement(WideString(variable));
  root.AppendChild(elem);

  TDOMElement(elem).SetAttribute('type', 'boolean');
  TDOMElement(elem).SetAttribute('value', WideString(BoolToStr(value, True)));
end;

procedure write_integer_pmakecache(variable: string; value: integer);
var
  elem: TDOMElement;
begin
  elem := doc.CreateElement(WideString(variable));
  root.AppendChild(elem);

  TDOMElement(elem).SetAttribute('type', 'integer');
  TDOMElement(elem).SetAttribute('value', WideString(IntToStr(value)));
end;

procedure write_float_pmakecache(variable: string; value: double);
var
  elem: TDOMElement;
begin
  elem := doc.CreateElement(WideString(variable));
  root.AppendChild(elem);

  TDOMElement(elem).SetAttribute('type', 'float');
  TDOMElement(elem).SetAttribute('value', WideString(FloatToStr(value)));
end;

procedure write_string_pmakecache(variable: string; value: string);
var
  elem: TDOMElement;
begin
  elem := doc.CreateElement(WideString(variable));
  root.AppendChild(elem);

  TDOMElement(elem).SetAttribute('type', 'string');
  TDOMElement(elem).SetAttribute('value', WideString(value));
end;

procedure write_filecache_pmakecache(variable: string; crc: word; fname: string);
var
  elem: TDOMElement;
  itm: TDOMElement;
begin
  elem := TDOMElement(doc.DocumentElement.FindNode(WideString(variable)));

  if elem = nil then
  begin
    elem := doc.CreateElement(WideString(variable));
    root.AppendChild(elem);
    TDOMElement(elem).SetAttribute('type', 'filecache');
  end;

  itm := doc.CreateElement('item');
  elem.AppendChild(itm);

  TDOMElement(itm).SetAttribute('crc', WideString(IntToStr(crc)));
  TDOMElement(itm).SetAttribute('filename', WideString(fname));
end;

procedure pmakecache_write;
var
  pmakecrc: word;
  f: TStrings;
  i: integer;
  v: pPMK_variant;
begin
  write_pmakecache_init;

  //write all variables to cache
  v := varlist.first;

  while v <> nil do
  begin
    case v^.vtype of
      ptBoolean:
        write_boolean_pmakecache(v^.name, v^.PM_Boolean);
      ptInteger:
        write_integer_pmakecache(v^.name, v^.PM_Integer);
      ptFloat:
        write_float_pmakecache(v^.name, v^.PM_Float);
      ptString:
        write_string_pmakecache(v^.name, v^.PM_String);
    end;

    v := v^.next;
  end;

  //write the actual PMAKE file cache as well
  if pmakefiles <> nil then
  begin
    f := TStringList.Create;
    for i := 0 to pmakefiles.Count - 1 do
    begin
      f.LoadFromFile(pPMakeItem(pmakefiles[i])^.fname);
      pmakecrc := crc_16(@f.Text[1], length(f.Text));

      write_filecache_pmakecache('PMAKE_TXT', pmakecrc, pPMakeItem(pmakefiles[i])^.fname);
    end;
    f.Free;
  end;

  write_pmakecache_finish;

  OutputLn('-- PMakeCache.txt file has been written');
end;

procedure pmakecache_read;
var
  child: TDOMNode;
  j: integer;
  nl: TDOMNodeList;
  name: string;
  list: pPMK_ListBase;
  itm: pPMK_FileCache;
begin
  if not FileExists('PMakeCache.txt') then
    message(FATAL_ERROR, '(1009) fatal error: cannot find PMakeCache.txt, rerun pmake');

  try
    ReadXMLFile(doc, 'PMakeCache.txt');

    //get the first child under the root
    child := doc.FirstChild.FirstChild;

    while child <> nil do
    begin
      name := string(child.NodeName);

      case child.Attributes.GetNamedItem('type').NodeValue of
        'boolean': set_(name, StrToBool(string(child.Attributes.GetNamedItem('value').NodeValue)));
        'integer': set_(name, StrToInt(string(child.Attributes.GetNamedItem('value').NodeValue)));
        'float': set_(name, StrToFloat(string(child.Attributes.GetNamedItem('value').NodeValue)));
        'string': set_(name, string(child.Attributes.GetNamedItem('value').NodeValue));
        'filecache':
        begin
          nl := child.ChildNodes;

          list := callocN(sizeof(PMK_ListBase));

          for j := 0 to nl.Count - 1 do
          begin
            itm := callocN(sizeof(PMK_FileCache));

            itm^.crc := StrToInt(string(nl.Item[j].Attributes.GetNamedItem('crc').NodeValue));
            itm^.fname := string(nl.Item[j].Attributes.GetNamedItem('filename').NodeValue);

            //add item to bottom of list
            addtail(list, itm);
          end;

          set_(name, list);
        end;
      end;

      child := child.NextSibling;
    end;
  finally
    FreeAndNil(doc);
  end;
end;

end.
