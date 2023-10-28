program IncTest;

{******************************************************************************}
{                                                                              }
{     Bin2Inc Delphi Demo application                                          }
{     Version 1.2 released October, 27th 2023                                  }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ MIT License                                                                  }
{                                                                              }
{ Copyright (c) 2023, Christian Cristofori <github@christiancristofori.it>     }
{                                                                              }
{ Permission is hereby granted, free of charge, to any person obtaining a copy }
{ of this software and associated documentation files (the "Software"), to     }
{ deal in the Software without restriction, including without limitation the   }
{ rights to use, copy, modify, merge, publish, distribute, sublicense, and/or  }
{ sell copies of the Software, and to permit persons to whom the Software is   }
{ furnished to do so, subject to the following conditions:                     }
{                                                                              }
{ The above copyright notice and this permission notice shall be included in   }
{ all copies or substantial portions of the Software.                          }
{                                                                              }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  }
{ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      }
{ FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS }
{ IN THE SOFTWARE.                                                             }
{                                                                              }
{******************************************************************************}

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
