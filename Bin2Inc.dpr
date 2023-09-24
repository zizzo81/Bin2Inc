program Bin2Inc;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Classes,// Windows,
  Exceptions in 'Exceptions.pas';

const
  VERSION = '1.0';
  YEAR = 2002;

function CopyrightLine: String; inline;
begin
  Result := Format('%s v. %s Copyright '#169' Christian Cristofori, %d.', [ChangeFileExt(ExtractFileName(ParamStr(0)), ''), VERSION, YEAR])
end;

function AlignmentToStr(const A: Byte): String; inline;
begin
  Result := '';
  case A of
    1: Result := 'Byte';
    2: Result := 'Word';
    4: Result := 'Cardinal';
    8: Result := 'UInt64'
  end
end;

var
  Files: TStringList;
  Recursive: Boolean = False;

procedure FindFiles(const Folder: String; const Mask: String);
var
  SR: TSearchRec;
  R: Integer;
begin
  R := FindFirst(IncludeTrailingPathDelimiter(Folder) + Mask, faAnyFile, SR);
  while R = 0 do
  begin
    if SR.Attr and faDirectory = 0 then
      Files.Add(IncludeTrailingPathDelimiter(Folder) + SR.Name);
    R := FindNext(SR)
  end;
  SysUtils.FindClose(SR);

  if Recursive then
  begin
    R := FindFirst(IncludeTrailingPathDelimiter(Folder) + '*', faAnyFile, SR);
    while R = 0 do
    begin
      if (SR.Attr and faDirectory = faDirectory) and (SR.Name <> '.') and (SR.Name <> '..') then
        FindFiles(IncludeTrailingPathDelimiter(Folder) + SR.Name, Mask);
      R := FindNext(SR)
    end;
    SysUtils.FindClose(SR)
  end
end;

var
  Alignment: Byte = 0;
  UseLowercase: Boolean = False;
  OutputStream: TFileStream;
  Spacer: String = #32#32;
  Columns: Byte = 8;

procedure WriteToFile(const S: String = '');
var
  A: AnsiString;
  P: Int64;
begin
  A := AnsiString(S + #13#10);
  // Preallocates output stream by 1MB
  if OutputStream.Position + Length(A) > OutputStream.Size then
  begin
    P := OutputStream.Position;
    OutputStream.Size := OutputStream.Size + 1024 * 1024;
    OutputStream.Position := P;
  end;

  OutputStream.Write(A[1], Length(A))
end;

function PurgeFilename(const Filename: String): String;
var
  I: Integer;
begin
  Result := UpperCase(Format('BIN_%s', [ExtractFileName(Filename)]));
  for I := 1 to Length(Result) do
    if not CharInSet(Result[I], ['A'..'Z', '0'..'9']) then
      Result[I] := '_';
  while Pos(Result, '__') > 0 do
    Result := StringReplace(Result, '__', '_', [rfReplaceAll]);
  while (Length(Result) > 0) and ((Result[1] = '_') or CharInSet(Result[1], ['0'..'9'])) do
    Delete(Result, 1, 1)
end;

function AddFile(const Filename: String): Byte;
var
  FStream: TFileStream;
  Align, B: Byte;
  W: Word;
  DW: Cardinal;
  UI: UInt64;
  I: Int64;
  S: String;
begin
  Result := 0;

  try
    FStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  except
    Exit
  end;
  try

    if (Alignment > 0) and (FStream.Size mod Alignment = 0) then
      Align := Alignment
    else if FStream.Size mod 8 = 0 then
      Align := 8
    else if FStream.Size mod 4 = 0 then
      Align := 4
    else if FStream.Size mod 2 = 0 then
      Align := 2
    else
      Align := 1;

    // Write out the
    WriteToFile(Format(Spacer + '// generated from file "%s"', [ExtractFileName(Filename)]));
    WriteToFile(Format(Spacer + '%S: Array[0..%d] of %s =', [PurgeFilename(Filename), (FStream.Size div Align) - 1, AlignmentToStr(Align)]));
    WriteToFile(Spacer + '(');

    FStream.Position := 0;
    S := '';
    // TODO: to create this faster, load a big buffer into memory and use it as
    //       a byte array to read from, less reads from disk, faster execution.
    for I := 0 to (FStream.Size div Align) - 1 do
    begin
      case Align of
        1:
        begin
          FStream.Read(B, Align);
          S := S + '$';
          if UseLowercase then
            S := S + LowerCase(IntToHex(B, Align * 2))
          else
            S := S + IntToHex(B, Align * 2);
          S := S + ', ';
        end;
        2:
        begin
          FStream.Read(W, Align);
          S := S + '$';
          if UseLowercase then
            S := S + LowerCase(IntToHex(W, Align * 2))
          else
            S := S + IntToHex(W, Align * 2);
          S := S + ', ';
        end;
        4:
        begin
          FStream.Read(DW, Align);
          S := S + '$';
          if UseLowercase then
            S := S + LowerCase(IntToHex(DW, Align * 2))
          else
            S := S + IntToHex(DW, Align * 2);
          S := S + ', ';
        end;
        8:
        begin
          FStream.Read(UI, Align);
          S := S + '$';
          if UseLowercase then
            S := S + LowerCase(IntToHex(UI, Align * 2))
          else
            S := S + IntToHex(UI, Align * 2);
          S := S + ','#32;
        end;
      end;

      // If I'm on a repetition I need to create a new line.
      if (I > 0) and ((I + 1) mod Columns = 0) then
      begin
        // Removes the trailing comma if I'm on the last interaction of the cycle.
        if I = (FStream.Size div Align) - 1 then
        begin
          S := Trim(S);
          if S[Length(S)] = ',' then
            Delete(S, Length(S), 1)
        end;
        // Writes to file.
        WriteToFile(Spacer + Spacer + Trim(S));
        // Empty the output buffer.
        S := ''
      end
    end;

    // If I have still something in the buffer to put out...
    S := Trim(S);
    if Length(S) > 0 then
    begin
      // Removes last comma.
      if S[Length(S)] = ',' then
        Delete(S, Length(S), MaxInt);
      // Writes to file.
      WriteToFile(Spacer + Spacer + S)
    end;

    // Closes the array.
    WriteToFile(Spacer + ');');

    Result := Align
  finally
    FStream.Free
  end
end;

var
  OverWrite: Boolean = False;
  OutputFileName: String = 'file.inc';
  S: String;
  I, X: Integer;
begin
  try
    Files := TStringList.Create;
    try
      // Reads parameters from command line.
      for I := 1 to ParamCount do
      begin
        // Check is the switch is one of the defined.
        X := Pos(UpCase((Copy(ParamStr(I), 2, 1) + #32)[1]), 'ACLRYIOST');
        // Check for initial "-" or "/" symbol.
        if not CharInSet((Copy(ParamStr(I), 1, 1) + #32)[1], ['-', '/']) or (X = 0) then
          raise ExitException.Create('Invalid parameter "%s".', [ParamStr(I)], True);

        // Check and apply each option.
        case X of
          1:  // -a:#
            if (Length(ParamStr(I)) = 4) and
               CharInSet((Copy(ParamStr(I), 3, 1) + #32)[1], [#32, ':']) and
               CharInSet((Copy(ParamStr(I), 4, 1) + #32)[1], ['1', '2', '4', '8']) then
              Alignment := Ord((Copy(ParamStr(I), 4, 1) + #32)[1]) - 48
            else
              raise ExitException.Create('Invalid parameter "%s".', [ParamStr(I)], True);
          2: // -c:# or -c:##
            if ((Length(ParamStr(I)) = 4) or (Length(ParamStr(I)) = 5)) and
               CharInSet((Copy(ParamStr(I), 3, 1) + #32)[1], [#32, ':']) and
               CharInSet((Copy(ParamStr(I), 4, 1) + #32)[1], ['0'..'9']) and
               CharInSet((Copy(ParamStr(I), 5, 1) + '0')[1], ['0'..'9']) then
            begin
              if Length(ParamStr(I)) = 4 then
                Columns := Ord((Copy(ParamStr(I), 4, 1) + '0')[1]) - 48
              else
                Columns := (Ord((Copy(ParamStr(I), 4, 1) + '0')[1]) - 48) * 10 + (Ord((Copy(ParamStr(I), 5, 1) + '0')[1]) - 48)
            end else
              raise ExitException.Create('Invalid parameter "%s".', [ParamStr(I)], True);
          3: // -l
            if Length(ParamStr(I)) = 2 then
              UseLowercase := True
            else
              raise ExitException.Create('Invalid parameter "%s".', [ParamStr(I)], True);
          4:  // -r
            if Length(ParamStr(I)) = 2 then
              Recursive := True
            else
              raise ExitException.Create('Invalid parameter "%s".', [ParamStr(I)], True);
          5:  // -y
            if Length(ParamStr(I)) = 2 then
              OverWrite := True
            else
              raise ExitException.Create('Invalid parameter "%s".', [ParamStr(I)], True);
          6:  // -i:filemask
            if (Length(ParamStr(I)) > 3) and CharInSet((Copy(ParamStr(I), 3, 1) + #32)[1], [#32, ':']) then
              FindFiles(GetCurrentDir, Copy(ParamStr(I), 4, MaxInt))
            else
              raise ExitException.Create('Invalid parameter "%s".', [ParamStr(I)], True);
          7:  // -o:filename
            if (Length(ParamStr(I)) > 3) and CharInSet((Copy(ParamStr(I), 3, 1) + #32)[1], [#32, ':']) then
              OutputFileName := Copy(ParamStr(I), 4, MaxInt)
            else
              raise ExitException.Create('Invalid parameter "%s".', [ParamStr(I)], True);
          8:  // -s:# or -s:##
            if ((Length(ParamStr(I)) = 4) or (Length(ParamStr(I)) = 5)) and
               CharInSet((Copy(ParamStr(I), 3, 1) + #32)[1], [#32, ':']) and
               CharInSet((Copy(ParamStr(I), 4, 1) + #32)[1], ['0'..'9']) and
               CharInSet((Copy(ParamStr(I), 5, 1) + '0')[1], ['0'..'9']) then
            begin
              if Length(ParamStr(I)) = 4 then
                Spacer := StringOfChar(#32, Ord((Copy(ParamStr(I), 4, 1) + '0')[1]) - 48)
              else
                Spacer := StringOfChar(#32, (Ord((Copy(ParamStr(I), 4, 1) + '0')[1]) - 48) * 10 +
                  (Ord((Copy(ParamStr(I), 5, 1) + '0')[1]) - 48))
            end else
              raise ExitException.Create('Invalid parameter "%s".', [ParamStr(I)], True);
          9:  // -t
            if Length(ParamStr(I)) = 2 then
              Spacer := #9
            else
              raise ExitException.Create('Invalid parameter "%s".', [ParamStr(I)], True);
        end
      end;

      // Check that at least one file was found.
      if Files.Count = 0 then
        raise ExitException.Create('No files to include have been found.');

      if FileExists(OutputFileName) and not OverWrite then
      begin
        while True do
        begin
          Write(Format('Overwrite file "%s"? (Y/N) ', [OutputFileName]));
          ReadLn(S);
          S := Trim(UpperCase(S));
          if S = 'Y' then
            Break
          else if S = 'N' then
            Exit
        end
      end;

      // Start creation.
      WriteLn('Starting creating "' + OutputFileName + '" file.');
      Write('Alignment: ');
      if Alignment = 0 then
        WriteLn('automatic.')
      else
        WriteLn(Format('%d bytes (%s).', [Alignment, AlignmentToStr(Alignment)]));
      WriteLn(Format('Columns: %d', [Columns]));

      // Create output file.
      OutputStream := TFileStream.Create(OutputFileName, fmCreate or fmOpenReadWrite or fmShareDenyWrite);
      try
        WriteToFile('// Created using ' + CopyrightLine);
        WriteToFile(Format('// executed on %s @ %s, as:', [FormatDateTime('yyyy-mm-dd', Now), FormatDateTime('hh:nn:ss', Now)]));
        WriteToFile('// ' + CmdLine);
        WriteToFile();
        WriteToFile('const');

        // While we have files in the list...
        while Files.Count > 0 do
        begin
          Write('Including "');
          Write(StringReplace(Files[0], GetCurrentDir, '', [rfIgnoreCase]));
          Write('"... ');

          // Add the file to the INC.
          X := AddFile(Files[0]);
          if X > 0 then
          begin
            if X <> Alignment then
              WriteLn(Format('done, aligned to %d byte(s) (%s)', [X, AlignmentToStr(X)]))
            else
              WriteLn('done.')
          end else
            WriteLn('FAILED!');

          // Remove file from the list.
          Files.Delete(0);

          // If we have other files to add, insert a separation line.
          if Files.Count > 0 then
            WriteToFile()
        end
      finally
        // Truncates unused preallocated space.
        OutputStream.Size := OutputStream.Position;
        OutputStream.Free
      end
    finally
      Files.Free
    end
  except
    on E: ExitException do
    begin
      WriteLn(CopyrightLine);
      WriteLn;
      WriteLn('Usage:');
      WriteLn(#9 + ExtractFileName(ParamStr(0)) + ' [-a:#] [-r] -i:filemask [-i:filemask...] [-o:file.inc]');
      WriteLn;
      WriteLn('-a'#9'Specifies a desired alignment (1, 2, 4, 8) if file can''t be aligned');
      WriteLn(#9'or no alignment is specified, best alignment possible is determined');
      WriteLn(#9'automatically.');
      WriteLn('-c:##'#9'Specifies number of columns of values to use, defaults to 8.');
      WriteLn('-l'#9'Uses lowercase for hexadecimal values. Defaults to uppercase.');
      WriteLn('-r'#9'Searches for files recursively.');
      WriteLn('-y'#9'Automatically overwrites existing file.');
      WriteLn('-i'#9'Specifies one or more filemask for files to include in the file.');
      WriteLn('-o'#9'Specifies the filename for output file, defaults to file.inc');
      WriteLn('-s:##'#9'Uses a given number of spaces as indent. Default to 2.');
      WriteLn('-t'#9'Uses a tabulation as indent.');
      // TODO: allow to specify a intra-column spacing
      if Length(E.Message) > 0 then
      begin
        WriteLn;
        WriteLn;
        WriteLn(E.Message)
      end
    end;
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message)
  end
end.
