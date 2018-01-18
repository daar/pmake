unit make_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure make_execute;
procedure search_pmake(const path: string);

implementation

uses
  XMLConf,
  crc16,
  pmake_utilities, pmake_variables, pmake_api, compiler;

type
  TRunMode = (rmBuild, rmInstall, rmClean);

const
  CmdOptions: array[1..9] of TCmdOption = (
    (name: 'build'; descr: 'Build all targets in the project.'),
    (name: 'clean'; descr: 'Clean all units and folders in the project'),
    (name: 'install'; descr: 'Install all targets in the project.'),
    (name: 'package'; descr: 'Create a package (zip)'),
    (name: '--compiler'; descr: 'Use indicated binary as compiler'),
    (name: '--debug'; descr: 'Do not delete the make2 source file.'),
    (name: '--force'; descr: 'Force building make2.exe'),
    (name: '--help'; descr: 'This message.'),
    (name: '--verbose'; descr: 'Be more verbose.')
    );

var
  sline: TStringList;
  force_build: boolean = False;
  debug: boolean = False;
  RunMode: TRunMode = rmBuild;
  make2_params: TStrings;

procedure output_line(var sline: TStringList);
var
  fpc_msg: TFPCMessage;
begin
  fpc_msg := ParseFPCCommand(sline[0]);

  WriteFPCCommand(fpc_msg, [mCompiling, mDebug, mError, mFail, mHint, mLinking, mNote, mOption, mUnitInfo, mWarning]);

  sline.Delete(0);
end;

//make2 execution
procedure command_callback(line: string; active: boolean);
begin
  //parse the output
  sline.Text := sline.Text + line;

  while sline.Count > 1 do
    output_line(sline);

  if not active then
  begin
    while sline.Count > 0 do
      output_line(sline);
  end;
end;

//make2 compilation
procedure make2_callback(line: string; active: boolean);
var
  fpc_msg: TFPCMessage;
begin
  if not verbose then
    exit;

  //parse the output
  sline.Text := sline.Text + line;

  while sline.Count > 1 do
  begin
    fpc_msg := ParseFPCCommand(sline[0]);
    UpdatePMakePostions(fpc_msg, [mCompiling, mDebug, mError, mFail, mHint, mLinking, mNote, mOption, mUnitInfo, mWarning], pmakefiles);
    sline.Delete(0);
  end;

  if not active then
  begin
    while sline.Count > 0 do
    begin
      fpc_msg := ParseFPCCommand(sline[0]);
      UpdatePMakePostions(fpc_msg, [mCompiling, mDebug, mError, mFail, mHint, mLinking, mNote, mOption, mUnitInfo, mWarning], pmakefiles);
      sline.Delete(0);
    end;
  end;
end;

function check_rebuild_make2: boolean;
var
  i, j: integer;
  f: TStrings;
  pmakecrc: word;
  p: string;
  c: word;
  found: boolean;
begin
  if not FileExists('PMakeCache.txt') then
    exit(True);

  if not FileExists(macros_expand('make2$(EXE)')) then
    exit(True);

  //return true if the PMake.txt is count is different
  if cache.GetValue('PMake/count', 0) <> pmakefiles.Count then
    exit(True);

  //return true if a crc / PMake.txt combination is not found
  f := TStringList.Create;
  for i := 0 to pmakefiles.Count - 1 do
  begin
    p := cache.GetValue(widestring(Format('PMake/item_%d/filename', [i + 1])), widestring(''));
    c := cache.GetValue(widestring(Format('PMake/item_%d/crc', [i + 1])), 0);

    f.LoadFromFile(p);
    pmakecrc := crc_16(@f.Text[1], length(f.Text));

    found := False;
    for j := 0 to pmakefiles.Count - 1 do
      if pPMakeItem(pmakefiles[j])^.fname = p then
      begin
        found := true;
        break;
      end;

    //either the PMake.txt file does not exist, or the crc changed
    if (found = False) or (c <> pmakecrc) then
    begin
      f.Free;
      exit(True);
    end;
  end;
  f.Free;

  exit(False);
end;

//todo: replace this by a PMake interpreter that finds and follows the add_subdirectory function calls
procedure search_pmake(const path: string);
var
  info: TSearchRec;
  p: pPMakeItem;
begin
  if FindFirst(path + '*', faAnyFile, info) = 0 then
  begin
    try
      repeat
        if (info.Attr and faDirectory) = 0 then
        begin
          //add PMake.txt to the file list
          if info.Name = 'PMake.txt' then
          begin
            p := GetMem(SizeOf(TPMakeItem));
            p^.fname := path + info.Name;
            pmakefiles.Add(p);
          end;
        end
        else
        //start the recursive search
        if (info.Name <> '.') and (info.Name <> '..') then
          search_pmake(IncludeTrailingBackSlash(path + info.Name));

      until FindNext(info) <> 0
    finally
      FindClose(info);
    end;
  end;
end;

procedure make2_build;
var
  make2, f: TStrings;
  src_name: string;
  i, exit_code: integer;
  param: TStrings;
  p: pPMakeItem;
begin
  OutputLn('-- Processing PMake.txt files');

  make2 := TStringList.Create;

  make2.Add('program make2;');
  make2.Add('uses {$IFDEF UNIX} cthreads, {$ENDIF} make2_main, pmake_api, pmake_variables;');
  make2.Add('begin');
  make2.Add('  init_make2;');

  //insert code from PMake.txt files
  f := TStringList.Create;
  for i := 0 to pmakefiles.Count - 1 do
  begin
    p := pPMakeItem(pmakefiles[i]);

    f.LoadFromFile(p^.fname);

    make2.Add('  add_subdirectory(''%s'');', [ExtractFilePath(p^.fname)]);

    //determine the start and end position of the PMake.txt in the source file
    p^.startpos := make2.Count;
    p^.endpos := p^.startpos + f.Count;

    make2.Add(f.Text);
  end;
  f.Free;

  make2.Add('  execute_make2;');
  make2.Add('end.');

  src_name := GetTempFileName('.', 'pmake');
  make2.SaveToFile(src_name);

  param := TStringList.Create;
  param.Add('-viq');

  //add the unit search path where the pmake executable is locate
  param.Add('-FU' + UnitsOutputDir(val_('PMAKE_BINARY_DIR')));
  param.Add('-Fu' + val_('PMAKE_TOOL_DIR'));
  param.Add(src_name);
  param.Add(macros_expand('-omake2$(EXE)'));

  sline := TStringList.Create;
  exit_code := command_execute(val_('PMAKE_PAS_COMPILER'), param, @make2_callback, false);
  sline.Free;

  param.Free;

  //remove the object and source files
  if verbose then
    OutputLn('-- Deleting temporary files');

  if not debug then
    DeleteFile(src_name);

  DeleteFile(ChangeFileExt(src_name, '.o'));

  if exit_code <> 0 then
    messagefmt(FATAL_ERROR, 'fatal error: cannot compile %s', [macros_expand('make2$(EXE)')]);
end;

procedure usage;
var
  i: integer;
begin
  OutputLn('PMake the pascal build tool. Version ' + PMAKE_VERSION + ' [' +
{$I %DATE%}
    + '] for ' +
{$I %FPCTARGETCPU%}
    );
  OutputLn('Copyright (c) 2016-2017 by Darius Blaszyk');
  OutputLn('');
  OutputLn('Usage ');
  OutputLn('  make [options] <path-to-source>');
  OutputLn('');
  OutputLn('Options');

  for i := low(CmdOptions) to high(CmdOptions) do
    OutputLn(Format('  %-16s %s', [CmdOptions[i].name, CmdOptions[i].descr]));

  halt(1);
end;

procedure parse_commandline;
var
  i, j: integer;
  found: boolean;
begin
  i := 1;

  while i <= ParamCount do
  begin
    found := False;
    for j := low(CmdOptions) to high(CmdOptions) do
    begin
      if ParamStr(i) = CmdOptions[j].name then
      begin
          found := True;

          case CmdOptions[j].name of
            'build': make2_params.Add('build');
            'clean': make2_params.Add('clean');
            'install': make2_params.Add('install');
            'package':
            begin
              make2_params.Add('package');
              inc(i);
              make2_params.Add(ParamStr(i));
            end;
            '--compiler':
            begin
              if i < ParamCount then
              begin
                Inc(i);
                set_('PMAKE_PAS_COMPILER', ParamStr(i));
                if not FileExists(ParamStr(i)) then
                  message(FATAL_ERROR, 'fatal error: cannot find the supplied compiler');
              end
              else
              begin
                StdErrLn('error: please supply a valid path for the compiler');
                usage;
              end;
            end;
            '--force': force_build := True;
            '--debug': debug := True;
            '--help': usage;
            '--verbose': verbose := True;
          end;
        if found then
          break;
      end;
    end;

    if not found then
    begin
      StdErrLn('error: invalid commandline parameter ' + ParamStr(i));
      usage;
    end;

    Inc(i);
  end;
end;

//a custom sort for the pmake file list
function ComparePath(Item1: Pointer; Item2: Pointer): Integer;
var
  path1, path2: string;
begin
  path1 := ExtractFilePath(pPMakeItem(Item1)^.fname);
  path2 := ExtractFilePath(pPMakeItem(Item2)^.fname);

  if path1 < path2 then
    Result := -1
  else
    if path1 = path2 then
      Result := 0
    else
      Result := 1;
end;

procedure make_execute;
var
  exit_code: integer;
begin
  cache := TXMLConfig.Create(nil);
  if FileExists('PMakeCache.txt') then
    cache.LoadFromFile('PMakeCache.txt')
  else
    message(FATAL_ERROR, 'fatal error: cannot find PMakeCache.txt, rerun pmake');

  pmakecache_read;

  make2_params := TStringList.Create;
  parse_commandline;

  pmakefiles := TFPList.Create;
  search_pmake(val_('PMAKE_SOURCE_DIR'));
  pmakefiles.Sort(@ComparePath);

  if check_rebuild_make2 or force_build then
    make2_build;

  pmakecache_write;

  sline := TStringList.Create;
  exit_code := command_execute(macros_expand('$(PMAKE_BINARY_DIR)make2$(EXE)', nil), make2_params, @command_callback, false);

  make2_params.Free;
  sline.Free;

  pmakefiles.Free;

  if exit_code <> 0 then
    messagefmt(FATAL_ERROR, 'fatal error: cannot execute %s', [macros_expand('make2$(EXE)')]);
end;

end.

