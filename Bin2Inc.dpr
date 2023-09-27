program Bin2Inc;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils, Classes;

const
  VERSION = '1.1';
  YEAR = 2023;

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

procedure PrintHelp(const S: String = ''); inline;
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
  if Length(S) > 0 then
  begin
    WriteLn;
    WriteLn;
    WriteLn(S)
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

var
  FilesCounter: Integer = 0;
  ConstNames: TStringList;

function PurgeFilename(const Filename: String): String;
var
  I: Integer;
begin
  // Removes path and keeps filename and extension only.
  Result := UpperCase(ExtractFileName(Filename));

  // Replace unwanted characters with underscore symbol.
  for I := 1 to Length(Result) do
    if not CharInSet(Result[I], ['A'..'Z', '0'..'9']) then
      Result[I] := '_';

  // Remove duplicates.
  while Pos(Result, '__') > 0 do
    Result := StringReplace(Result, '__', '_', [rfReplaceAll]);

  // Remove initial invalid characters.
  while (Length(Result) > 0) and ((Result[1] = '_') or CharInSet(Result[1], ['0'..'9'])) do
    Delete(Result, 1, 1);

  // Just in case...
  if Length(Result) = 0 then
  begin
    Result := Format('FILE_%d', [FilesCounter]);
    Inc(FilesCounter)
  end;

  Result := Format('BIN_%s', [Result]);

  // Avoid duplicates.
  while ConstNames.IndexOf(Result) > -1 do
  begin
    Result := Format('BIN_FILE_%d', [FilesCounter]);
    Inc(FilesCounter)
  end;

  ConstNames.Add(Result)
end;

function AddFile(const Filename: String): Byte;
var
  FStream: TFileStream;
  MStream: TMemoryStream;
  Align: Byte;
  PB: PByte;
  PW: PWord;
  PDW: PCardinal;
  PUI: PUInt64;
  I: Int64;
  S: String;
begin
  Result := 0;

  // Opens file in readonly mode.
  try
    FStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  except
    Exit
  end;

  try
    // Decide best possible alignment for the file.
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

    // Write out the header of the file.
    WriteToFile(Format(Spacer + '// generated from file "%s"', [ExtractFileName(Filename)]));
    WriteToFile(Format(Spacer + '%S: Array[0..%d] of %s =', [PurgeFilename(Filename), (FStream.Size div Align) - 1, AlignmentToStr(Align)]));
    WriteToFile(Spacer + '(');

    MStream := TMemoryStream.Create;
    try
      FStream.Position := 0;
      S := '';

      while FStream.Position < FStream.Size do
      begin
        MStream.Clear;

        // Reads up to 100k items per time.
        I := Align * 100000;
        if I > FStream.Size - FStream.Position then
          I := FStream.Size - FStream.Position;

        MStream.CopyFrom(FStream, I);
        PB := MStream.Memory;
        PW := MStream.Memory;
        PDW := MStream.Memory;
        PUI := MStream.Memory;

        for I := 0 to (MStream.Size div Align) - 1 do
        begin

          S := S + '$';
          case Align of
            1: S := S + IntToHex(PB^, Align * 2);
            2: S := S + IntToHex(PW^, Align * 2);
            4: S := S + IntToHex(PDW^, Align * 2);
            8: S := S + IntToHex(PUI^, Align * 2)
          end;
          S := S + ','#32;

          Inc(PB);
          Inc(PW);
          Inc(PDW);
          Inc(PUI);

          // If I'm on a repetition I need to create a new line.
          if (I > 0) and ((I + 1) mod Columns = 0) then
          begin
            // Removes the trailing comma if I'm on the last interaction of the cycle.
            if (FStream.Position = FStream.Size) and (I = (MStream.Size div Align) - 1) then
            begin
              S := Trim(S);
              if S[Length(S)] = ',' then
                Delete(S, Length(S), 1)
            end;

            // Applies lower if necessary
            if UseLowercase then
              S := LowerCase(S);

            // Writes to file.
            WriteToFile(Spacer + Spacer + Trim(S));

            // Empty the output buffer.
            S := ''
          end
        end
      end
    finally
      MStream.Free
    end;

    // If I have still something in the buffer to put out...
    S := Trim(S);
    if Length(S) > 0 then
    begin
      // Removes last comma.
      if S[Length(S)] = ',' then
        Delete(S, Length(S), MaxInt);

      // Applies lower if necessary
      if UseLowercase then
        S := LowerCase(S);

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

type
  ExitException = class(Exception);

var
  OverWrite: Boolean = False;
  OutputFileName: String = 'file.inc';
  S: String;
  I, X: Integer;
begin
  try
    Files := TStringList.Create;
    ConstNames := TStringList.Create;
    try
      // Reads parameters from command line.
      for I := 1 to ParamCount do
      begin
        // Check is the switch is one of the defined.
        X := Pos(UpCase((Copy(ParamStr(I), 2, 1) + #32)[1]), 'ACLRYIOST');
        // Check for initial "-" or "/" symbol.
        if not CharInSet((Copy(ParamStr(I), 1, 1) + #32)[1], ['-', '/']) or (X = 0) then
          raise ExitException.Create(Format('Invalid parameter "%s".', [ParamStr(I)]));

        // Check and apply each option.
        case X of
          1:  // -a:#
            if (Length(ParamStr(I)) = 4) and
               CharInSet((Copy(ParamStr(I), 3, 1) + #32)[1], [#32, ':']) and
               CharInSet((Copy(ParamStr(I), 4, 1) + #32)[1], ['1', '2', '4', '8']) then
              Alignment := Ord((Copy(ParamStr(I), 4, 1) + #32)[1]) - 48
            else
              raise ExitException.Create(Format('Invalid parameter "%s".', [ParamStr(I)]));
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
              raise ExitException.Create(Format('Invalid parameter "%s".', [ParamStr(I)]));
          3: // -l
            if Length(ParamStr(I)) = 2 then
              UseLowercase := True
            else
              raise ExitException.Create(Format('Invalid parameter "%s".', [ParamStr(I)]));
          4:  // -r
            if Length(ParamStr(I)) = 2 then
              Recursive := True
            else
              raise ExitException.Create(Format('Invalid parameter "%s".', [ParamStr(I)]));
          5:  // -y
            if Length(ParamStr(I)) = 2 then
              OverWrite := True
            else
              raise ExitException.Create(Format('Invalid parameter "%s".', [ParamStr(I)]));
          6:  // -i:filemask
            if (Length(ParamStr(I)) > 3) and CharInSet((Copy(ParamStr(I), 3, 1) + #32)[1], [#32, ':']) then
              FindFiles(GetCurrentDir, Copy(ParamStr(I), 4, MaxInt))
            else
              raise ExitException.Create(Format('Invalid parameter "%s".', [ParamStr(I)]));
          7:  // -o:filename
            if (Length(ParamStr(I)) > 3) and CharInSet((Copy(ParamStr(I), 3, 1) + #32)[1], [#32, ':']) then
              OutputFileName := Copy(ParamStr(I), 4, MaxInt)
            else
              raise ExitException.Create(Format('Invalid parameter "%s".', [ParamStr(I)]));
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
              raise ExitException.Create(Format('Invalid parameter "%s".', [ParamStr(I)]));
          9:  // -t
            if Length(ParamStr(I)) = 2 then
              Spacer := #9
            else
              raise ExitException.Create(Format('Invalid parameter "%s".', [ParamStr(I)]))
        end
      end;

      // Check that at least one file was found.
      if Files.Count = 0 then
        raise ExitException.Create('No files to include have been found.');

      // Asck for overwrite confirmation if necessary.
      if FileExists(OutputFileName) and not OverWrite then
        while True do
        begin
          Write(Format('Overwrite file "%s"? (Y/N) ', [OutputFileName]));
          ReadLn(S);
          S := Trim(UpperCase(S));
          if S = 'Y' then
            Break
          else if S = 'N' then
            Exit
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
          S := StringReplace(Files[0], GetCurrentDir, '', [rfIgnoreCase]);
          while (Length(S) > 0) and (S[1] = '\') do
            Delete(S, 1, 1);
          Write(Format('Including "%s"...'#32, [S]));

          // Add the file to the INC.
          X := AddFile(Files[0]);
          if X = 0 then
            WriteLn('FAILED!')
          else if X <> Alignment then
            WriteLn(Format('done, aligned to %d byte(s) (%s)', [X, AlignmentToStr(X)]))
          else
            WriteLn('done.');

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
      Files.Free;
      ConstNames.Free
    end
  except
    on E: ExitException do
      PrintHelp(E.Message);
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message)
  end
end.
