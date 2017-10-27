unit pmake_variables;

{$mode objfpc}{$H+}

interface

uses
  XMLConf;

const
  _ON = True;
  _OFF = False;

  UNKNOWN = $00;
  DEBUG = $01;
  Release = $02;

{  (* INFORMATION *)
  //PMAKE_ARGC: string = '';
  //PMAKE_ARGV0: string = '';
  //PMAKE_AR: string = '';

  //if you are building in-source, this is the same as PMAKE_SOURCE_DIR, otherwise this is the top level directory of your build tree
  PMAKE_BINARY_DIR: string = '';

  //PMAKE_CACHEFILE_DIR: string = '';
  //PMAKE_CACHE_MAJOR_VERSION: string = '';
  //PMAKE_CACHE_MINOR_VERSION: string = '';
  //PMAKE_CACHE_PATCH_VERSION: string = '';
  //PMAKE_CFG_INTDIR: string = '';

  //this is the complete path of the pmake which runs currently (e.g. /usr/local/bin/pmake). Note that if you have custom commands that invoke pmake -E, it is very important to use PMAKE_COMMAND as the PMake executable, because PMake might not be on the system PATH.
  //PMAKE_COMMAND : string = '';

  //PMAKE_CROSSCOMPILING: boolean = False;
  //PMAKE_CTEST_COMMAND: string = '';

  //if you are building in-source, this is the same as PMAKE_CURRENT_SOURCE_DIR, otherwise this is the directory where the compiled or generated files from the current PMakeLists.txt will go to.
  //PMAKE_CURRENT_BINARY_DIR : string = '';

  //PMAKE_CURRENT_LIST_DIR: string = '';
  //PMAKE_CURRENT_LIST_FILE: string = '';
  //PMAKE_CURRENT_LIST_LINE: string = '';
  PMAKE_CURRENT_SOURCE_DIR: string = '';
  //PMAKE_DL_LIBS: string = '';
  //PMAKE_EDIT_COMMAND: string = '';
  //PMAKE_EXECUTABLE_SUFFIX: string = '';
  //PMAKE_EXTRA_GENERATOR: string = '';
  //PMAKE_EXTRA_SHARED_LIBRARY_SUFFIXES: string = '';
  //PMAKE_GENERATOR: string = '';
  //PMAKE_GENERATOR_TOOLSET: string = '';
  //PMAKE_HOME_DIRECTORY: string = '';
  //PMAKE_IMPORT_LIBRARY_PREFIX: string = '';
  //PMAKE_IMPORT_LIBRARY_SUFFIX: string = '';
  //PMAKE_JOB_POOL_COMPILE: string = '';
  //PMAKE_JOB_POOL_LINK: string = '';
  //PMAKE_LINK_LIBRARY_SUFFIX: string = '';
  //PMAKE_MAKE_PROGRAM: string = '';
  //PMAKE_MINIMUM_REQUIRED_VERSION: string = '';
  //PMAKE_PARENT_LIST_FILE: string = '';
  PMAKE_PROJECT_Name: string255 = '';
  //PMAKE_RANLIB: string = '';
  //PMAKE_ROOT: string = '';
  //PMAKE_SCRIPT_MODE_FILE: string = '';
  //PMAKE_SHARED_LIBRARY_PREFIX: string = '';
  //PMAKE_SHARED_LIBRARY_SUFFIX: string = '';
  //PMAKE_SHARED_MODULE_PREFIX: string = '';
  //PMAKE_SHARED_MODULE_SUFFIX: string = '';
  //PMAKE_SIZEOF_VOID_P: string = '';
  //PMAKE_SKIP_INSTALL_RULES: string = '';
  //PMAKE_SKIP_RPATH: string = '';
  PMAKE_SOURCE_DIR: string = '';
  //PMAKE_STANDARD_LIBRARIES: string = '';
  //PMAKE_STATIC_LIBRARY_PREFIX: string = '';
  //PMAKE_STATIC_LIBRARY_SUFFIX: string = '';
  //PMAKE_TOOLCHAIN_FILE: string = '';
  //PMAKE_VERBOSE_MAKEFILE: string = '';
  //PMAKE_VS_DEVENV_COMMAND: string = '';
  //PMAKE_VS_INTEL_Fortran_PROJECT_VERSION: string = '';
  //PMAKE_VS_MSBUILD_COMMAND: string = '';
  //PMAKE_VS_MSDEV_COMMAND: string = '';
  //PMAKE_VS_PLATFORM_TOOLSET: string = '';
  //PMAKE_XCODE_PLATFORM_TOOLSET: string = '';
  //PROJECT_BINARY_DIR: string = '';
  //PROJECT_Name: string255 = '';
  //PROJECT_SOURCE_DIR: string = '';
  PROJECT_VERSION: string = '';
  PROJECT_VERSION_MAJOR: integer = 0;
  PROJECT_VERSION_MINOR: integer = 0;
  PROJECT_VERSION_PATCH: integer = 0;
  PROJECT_VERSION_TWEAK: integer = 0;
}
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

{
var
  (*BEHAVIOR *)
  //BUILD_SHARED_LIBS: string = '';
  //PMAKE_ABSOLUTE_DESTINATION_FILES: string = '';
  //PMAKE_APPBUNDLE_PATH: string = '';
  //PMAKE_AUTOMOC_RELAXED_MODE: string = '';
  //PMAKE_BACKWARDS_COMPATIBILITY: string = '';
  PMAKE_BUILD_TYPE: byte = UNKNOWN;
//PMAKE_COLOR_MAKEFILE: string = '';
//PMAKE_CONFIGURATION_TYPES: string = '';
//PMAKE_DEBUG_TARGET_PROPERTIES: string = '';
//PMAKE_DISABLE_FIND_PACKAGE_<PackageName>: string = '';
//PMAKE_ERROR_DEPRECATED: string = '';
//PMAKE_ERROR_ON_ABSOLUTE_INSTALL_DESTINATION: string = '';
//PMAKE_SYSROOT: string = '';
//PMAKE_FIND_LIBRARY_PREFIXES: string = '';
//PMAKE_FIND_LIBRARY_SUFFIXES: string = '';
//PMAKE_FIND_NO_INSTALL_PREFIX: string = '';
//PMAKE_FIND_PACKAGE_WARN_NO_MODULE: string = '';
//PMAKE_FIND_ROOT_PATH: string = '';
//PMAKE_FIND_ROOT_PATH_MODE_INCLUDE: string = '';
//PMAKE_FIND_ROOT_PATH_MODE_LIBRARY: string = '';
//PMAKE_FIND_ROOT_PATH_MODE_PACKAGE: string = '';
//PMAKE_FIND_ROOT_PATH_MODE_PROGRAM: string = '';
//PMAKE_FRAMEWORK_PATH: string = '';
//PMAKE_IGNORE_PATH: string = '';
//PMAKE_INCLUDE_PATH: string = '';
//PMAKE_INCLUDE_DIRECTORIES_BEFORE: string = '';
//PMAKE_INCLUDE_DIRECTORIES_PROJECT_BEFORE: string = '';
//PMAKE_INSTALL_DEFAULT_COMPONENT_Name: string255 = '';
//PMAKE_INSTALL_PREFIX: string = '';
//PMAKE_LIBRARY_PATH: string = '';
//PMAKE_MFC_FLAG: string = '';
//PMAKE_MODULE_PATH: string = '';
//PMAKE_NOT_USING_CONFIG_FLAGS: string = '';
//PMAKE_POLICY_DEFAULT_CMP<NNNN>: string = '';
//PMAKE_POLICY_WARNING_CMP<NNNN>: string = '';
//PMAKE_PREFIX_PATH: string = '';
//PMAKE_PROGRAM_PATH: string = '';
//PMAKE_PROJECT_<PROJECT-name>_INCLUDE: string = '';
//PMAKE_SKIP_INSTALL_ALL_DEPENDENCY: string = '';
//PMAKE_STAGING_PREFIX: string = '';
//PMAKE_SYSTEM_IGNORE_PATH: string = '';
//PMAKE_SYSTEM_INCLUDE_PATH: string = '';
//PMAKE_SYSTEM_LIBRARY_PATH: string = '';
//PMAKE_SYSTEM_PREFIX_PATH: string = '';
//PMAKE_SYSTEM_PROGRAM_PATH: string = '';
//PMAKE_USER_MAKE_RULES_OVERRIDE: string = '';
//PMAKE_WARN_DEPRECATED: string = '';
//PMAKE_WARN_ON_ABSOLUTE_INSTALL_DESTINATION: string = '';

(* SYSTEM *)
//APPLE: string = '';
//BORLAND: string = '';

//is TRUE when using a FreePascal compiler
function FREEPASCAL: boolean; inline;

var
//PMAKE_CL_64: string = '';
//PMAKE_COMPILER_2005: string = '';
//PMAKE_HOST_APPLE: string = '';
PMAKE_HOST_SYSTEM_Name: string255 = '';
PMAKE_HOST_SYSTEM_PROCESSOR: string = '';
//PMAKE_HOST_SYSTEM: string = '';
//PMAKE_HOST_SYSTEM_VERSION: string = '';
//PMAKE_HOST_UNIX: string = '';
//PMAKE_HOST_WIN32: string = '';
//PMAKE_LIBRARY_ARCHITECTURE_REGEX: string = '';
//PMAKE_LIBRARY_ARCHITECTURE: string = '';
//PMAKE_OBJECT_PATH_MAX: string = '';
//PMAKE_SYSTEM_Name: string255 = '';
//PMAKE_SYSTEM_PROCESSOR: string = '';
//PMAKE_SYSTEM: string = '';
//PMAKE_SYSTEM_VERSION: string = '';
//CYGWIN: string = '';
//ENV: string = '';
//MSVC10: string = '';
//MSVC11: string = '';
//MSVC12: string = '';
//MSVC60: string = '';
//MSVC70: string = '';
//MSVC71: string = '';
//MSVC80: string = '';
//MSVC90: string = '';
//MSVC_IDE: string = '';
//MSVC: string = '';
//MSVC_VERSION: string = '';
//UNIX: string = '';
//WIN32: string = '';
//XCODE_VERSION: string = '';

(* BUILD *)
//PMAKE_ARCHIVE_OUTPUT_DIRECTORY: string = '';
//PMAKE_AUTOMOC_MOC_OPTIONS: string = '';
//PMAKE_AUTOMOC: string = '';
//PMAKE_AUTORCC: string = '';
//PMAKE_AUTORCC_OPTIONS: string = '';
//PMAKE_AUTOUIC: string = '';
//PMAKE_AUTOUIC_OPTIONS: string = '';
//PMAKE_BUILD_WITH_INSTALL_RPATH: string = '';
//PMAKE_<CONFIG>_POSTFIX: string = '';
//PMAKE_DEBUG_POSTFIX: string = '';
//PMAKE_EXE_LINKER_FLAGS_<CONFIG>: string = '';
//PMAKE_EXE_LINKER_FLAGS: string = '';
//PMAKE_Fortran_FORMAT: string = '';
//PMAKE_Fortran_MODULE_DIRECTORY: string = '';
//PMAKE_GNUtoMS: string = '';
//PMAKE_INCLUDE_CURRENT_DIR_IN_INTERFACE: string = '';
//PMAKE_INCLUDE_CURRENT_DIR: string = '';
//PMAKE_INSTALL_Name_DIR: string = '';
//PMAKE_INSTALL_RPATH: string = '';
//PMAKE_INSTALL_RPATH_USE_LINK_PATH: string = '';
//PMAKE_PAS_VISIBILITY_PRESET: string = '';
//PMAKE_LIBRARY_OUTPUT_DIRECTORY: string = '';
//PMAKE_LIBRARY_PATH_FLAG: string = '';
//PMAKE_LINK_DEF_FILE_FLAG: string = '';
//PMAKE_LINK_DEPENDS_NO_SHARED: string = '';
//PMAKE_LINK_INTERFACE_LIBRARIES: string = '';
//PMAKE_LINK_LIBRARY_FILE_FLAG: string = '';
//PMAKE_LINK_LIBRARY_FLAG: string = '';
//PMAKE_MACOSX_BUNDLE: string = '';
//PMAKE_MACOSX_RPATH: string = '';
//PMAKE_MAP_IMPORTED_CONFIG_<CONFIG>: string = '';
//PMAKE_MODULE_LINKER_FLAGS_<CONFIG>: string = '';
//PMAKE_MODULE_LINKER_FLAGS: string = '';
//PMAKE_NO_BUILTIN_CHRPATH: string = '';
//PMAKE_NO_SYSTEM_FROM_IMPORTED: string = '';
//PMAKE_OSX_ARCHITECTURES: string = '';
//PMAKE_OSX_DEPLOYMENT_TARGET: string = '';
//PMAKE_OSX_SYSROOT: string = '';
//PMAKE_PDB_OUTPUT_DIRECTORY: string = '';
//PMAKE_PDB_OUTPUT_DIRECTORY_<CONFIG>: string = '';
//PMAKE_POSITION_INDEPENDENT_CODE: string = '';
//PMAKE_RUNTIME_OUTPUT_DIRECTORY: string = '';
//PMAKE_SHARED_LINKER_FLAGS_<CONFIG>: string = '';
//PMAKE_SHARED_LINKER_FLAGS: string = '';
//PMAKE_SKIP_BUILD_RPATH: string = '';
//PMAKE_SKIP_INSTALL_RPATH: string = '';
//PMAKE_STATIC_LINKER_FLAGS_<CONFIG>: string = '';
//PMAKE_STATIC_LINKER_FLAGS: string = '';
//PMAKE_TRY_COMPILE_CONFIGURATION: string = '';
//PMAKE_USE_RELATIVE_PATHS: string = '';
//PMAKE_VISIBILITY_INLINES_HIDDEN: string = '';
//PMAKE_WIN32_EXECUTABLE: string = '';
//EXECUTABLE_OUTPUT_PATH: string = '';
//LIBRARY_OUTPUT_PATH: string = '';

(* LANGUAGE *)
var
//PMAKE_COMPILER_IS_GNUPAS: string = '';
//PMAKE_Fortran_MODDIR_DEFAULT: string = '';
//PMAKE_Fortran_MODDIR_FLAG: string = '';
//PMAKE_Fortran_MODOUT_FLAG: string = '';
//PMAKE_INTERNAL_PLATFORM_ABI: string = '';
//PMAKE_PAS_ARCHIVE_APPEND: string = '';
//PMAKE_PAS_ARCHIVE_CREATE: string = '';
//PMAKE_PAS_ARCHIVE_FINISH: string = '';
//PMAKE_PAS_COMPILE_OBJECT: string = '';
//PMAKE_PAS_COMPILER_ABI: string = '';
//PMAKE_PAS_COMPILER_ID: string = '';
//PMAKE_PAS_COMPILER_LOADED: string = '';
PMAKE_PAS_COMPILER: string = '';
//PMAKE_PAS_COMPILER_EXTERNAL_TOOLCHAIN: string = '';
//PMAKE_PAS_COMPILER_TARGET: string = '';
PMAKE_PAS_COMPILER_VERSION: string = '';
//PMAKE_PAS_CREATE_SHARED_LIBRARY: string = '';
//PMAKE_PAS_CREATE_SHARED_MODULE: string = '';
//PMAKE_PAS_CREATE_STATIC_LIBRARY: string = '';
//PMAKE_PAS_FLAGS_DEBUG: string = '';
//PMAKE_PAS_FLAGS_MINSIZEREL: string = '';
//PMAKE_PAS_FLAGS_RELEASE: string = '';
//PMAKE_PAS_FLAGS_RELWITHDEBINFO: string = '';
//PMAKE_PAS_FLAGS: string = '';
//PMAKE_PAS_IGNORE_EXTENSIONS: string = '';
//PMAKE_PAS_IMPLICIT_INCLUDE_DIRECTORIES: string = '';
//PMAKE_PAS_IMPLICIT_LINK_DIRECTORIES: string = '';
//PMAKE_PAS_IMPLICIT_LINK_FRAMEWORK_DIRECTORIES: string = '';
//PMAKE_PAS_IMPLICIT_LINK_LIBRARIES: string = '';
//PMAKE_PAS_LIBRARY_ARCHITECTURE: string = '';
//PMAKE_PAS_LINKER_PREFERENCE_PROPAGATES: string = '';
//PMAKE_PAS_LINKER_PREFERENCE: string = '';
//PMAKE_PAS_LINK_EXECUTABLE: string = '';
//PMAKE_PAS_OUTPUT_EXTENSION: string = '';
//PMAKE_PAS_PLATFORM_ID: string = '';
//PMAKE_PAS_SIMULATE_ID: string = '';
//PMAKE_PAS_SIMULATE_VERSION: string = '';
//PMAKE_PAS_SIZEOF_DATA_PTR: string = '';
//PMAKE_PAS_SOURCE_FILE_EXTENSIONS: string = '';
//PMAKE_USER_MAKE_RULES_OVERRIDE_PAS: string = '';
}
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

var
  cache: TXMLConfig;

implementation

uses
  Classes, SysUtils, crc, pmake_utilities;

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
