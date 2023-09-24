program IncTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils, Classes;

{$I 'bin\test.inc'}

var
  MStream, FStream: TMemoryStream;
  I, C: Int64;
  M, F: PByteArray;
  B: Boolean = True;
begin
  try
    MStream := TMemoryStream.Create;
    FStream := TMemoryStream.Create;
    try
      try
        FStream.LoadFromFile('commedia.txt');
      except
        WriteLn('Unable to find/open "commedia.txt" file.');
        Exit
      end;

      MStream.Write(BIN_COMMEDIA_TXT[0], SizeOf(BIN_COMMEDIA_TXT));

      if MStream.Size <> FStream.Size then
        WriteLn('File stream size differs from constant size.');

      M := MStream.Memory;
      F := FStream.Memory;

      C := MStream.Size - 1;
      if FStream.Size - 1 < C then
        C := FStream.Size - 1;

      {$R-}
      for I := 0 to C do
        if M^[I] <> F^[I] then
        begin
          B := False;
          WriteLn(Format('File differs at index %d: 0x%.2x found, 0x%.2x expected.', [I, F^[I], M^[I]]))
        end;
      {$R+}

      if B then
        WriteLn('File matches include file.')
    finally
      MStream.Free;
      FStream.Free
    end
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message)
  end
end.
