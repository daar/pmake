program pmake;

{$mode objfpc}{$H+}
{ $define debug}

uses 
{$IFDEF UNIX}
  cthreads, 
{$ENDIF} 
{$ifdef debug}
  HeapTrc, 
{$endif}
  Classes, SysUtils,
  upmake, compiler;

procedure create_and_build_make;
var
  BasePath: string = '';
  tmp: TStringList;
  fpc_out: TStrings;
  fname: string;
  fpc_msg: TFPList;
begin
  if FileExists(ExpandMacros('make$(EXE)')) then
    exit;

  BasePath := IncludeTrailingBackSlash(GetCurrentDir);
  fname := GetTempFileName('.', 'pmake');

  writeln('-- Creating makefile');

  tmp := TStringList.Create;

  tmp.Add('program make;');
  tmp.Add('uses upmake;');
  tmp.Add('begin');
  tmp.Add('  if pmake_changed then');
  tmp.Add('    build_make2;');
  tmp.Add('  run_make2;');
  tmp.Add('end.');

  fname := GetTempFileName('.', 'pmake');
  tmp.SaveToFile(fname);

  fpc_out := RunCompilerCommand(ExpandMacros('make$(EXE)'), fname);
  fpc_msg := ParseFPCCommand(fpc_out, BasePath);
  WriteFPCCommand(fpc_msg, ShowMsg);

  fpc_out.Free;
  tmp.Free;

  //remove the object and source files
  if verbose then
    writeln('-- Deleting temporary files');

  DeleteFile(fname);
  DeleteFile(ChangeFileExt(fname, '.o'));
end;

begin
  check_options(ctPMake);

  if verbose then
    writeln('-- FPC compiler ', fpc);

  if (fpc = '') or (not FileExists(fpc)) then
  begin
    writeln('error: cannot find the FPC compiler');
    usage(ctPMake);
  end;

  if verbose then
    ShowMsg := AllMessages;

  create_and_build_make;

  if not FileExists(ExpandMacros('make2$(EXE)')) then
    build_make2;

  writeln('-- Generating done');
  writeln('-- Build file has been written to: ', ExpandFileName('make' + ExtractFileExt(ParamStr(0))));
end.
