unit Exceptions;

interface

uses
  SysUtils;

type
  ExitException = class(Exception)
  strict private
    FPrintHelp: Boolean;
  public
    constructor Create; overload;
    constructor Create(const AMessage: String); overload;
    constructor Create(const AFormat: String; const AArgs: Array of const); overload;
    constructor Create(const AMessage: String; const APrintHelp: Boolean); overload;
    constructor Create(const AFormat: String; const AArgs: Array of const; const APrintHelp: Boolean); overload;
    constructor Create(const APrintHelp: Boolean); overload;
  end;

implementation

{ ExitException }

constructor ExitException.Create;
begin
  inherited Create('');
  FPrintHelp := False
end;

constructor ExitException.Create(const AMessage: String);
begin
  inherited Create(AMessage);
  FPrintHelp := False
end;

constructor ExitException.Create(const AMessage: String; const APrintHelp: Boolean);
begin
  inherited Create(AMessage);
  FPrintHelp := APrintHelp
end;

constructor ExitException.Create(const APrintHelp: Boolean);
begin
  inherited Create('');
  FPrintHelp := APrintHelp
end;

constructor ExitException.Create(const AFormat: String;
  const AArgs: array of const; const APrintHelp: Boolean);
begin
  inherited Create(Format(AFormat, AArgs));
  FPrintHelp := APrintHelp
end;

constructor ExitException.Create(const AFormat: String; const AArgs: array of const);
begin
  inherited Create(Format(AFormat, AArgs));
  FPrintHelp := False
end;

end.
