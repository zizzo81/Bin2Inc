unit FormMainUnit;

interface

uses
  Forms, Controls, StdCtrls, Classes, ExtCtrls;

type
  TFormMain = class(TForm)
    iSample: TImage;
    pRight: TPanel;
    bLoad: TButton;
    procedure bLoadClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure iSampleClick(Sender: TObject);
  private
    function FullScreenGet: Boolean;
    procedure FullScreenSet(const Value: Boolean);

    property FullScreen: Boolean read FullScreenGet write FullScreenSet;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  // Don't invert order of these two.
  Windows, Graphics,
  // Required to be linked if we want to load a JPEG file.
  JPEG;

// Includes generated include file.
{$I 'sample_jpg.inc'}

procedure TFormMain.bLoadClick(Sender: TObject);
var
  MStream: TMemoryStream;
begin
  // Anti-bounce.
  bLoad.Enabled := False;

  MStream := TMemoryStream.Create;

  // Writes the content of the constant into the stream.
  MStream.Write(BIN_SAMPLE_JPG[0], SizeOf(BIN_SAMPLE_JPG));

  // Reverts stream position.
  MStream.Position := 0;

  try
    // Loads from stream into image.
    iSample.Picture.LoadFromStream(MStream);

    pRight.Visible := False;
    iSample.Cursor := crHandPoint
  except
    bLoad.Enabled := True
  end;

  MStream.Free
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  iSample.Canvas.Pen.Color := clBtnShadow;
  iSample.Canvas.Rectangle(0, 0, iSample.Width, iSample.Height)
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    if FullScreen then
      FullScreen := False
    else
      Application.Terminate
end;

function TFormMain.FullScreenGet: Boolean;
begin
  Result := BorderStyle = bsNone
end;

procedure TFormMain.FullScreenSet(const Value: Boolean);
begin
  if Value = FullScreen then
    Exit;

  if Value then
  begin
    AutoSize := False;
    Color := clBlack;
    BorderStyle := bsNone;
    WindowState := wsMaximized
  end else
  begin
    Color := clBtnFace;
    BorderStyle := bsDialog;
    WindowState := wsNormal;
    AutoSize := True
  end
end;

procedure TFormMain.iSampleClick(Sender: TObject);
begin
  if not pRight.Visible then
    FullScreen := not FullScreen
end;

end.
