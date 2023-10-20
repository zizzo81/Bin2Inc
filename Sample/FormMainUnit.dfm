object FormMain: TFormMain
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = 'Inc2Bin usage demonstration'
  ClientHeight = 433
  ClientWidth = 546
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  TextHeight = 15
  object iSample: TImage
    Left = 0
    Top = 0
    Width = 463
    Height = 433
    Align = alClient
    Center = True
    Proportional = True
    Stretch = True
    OnClick = iSampleClick
    ExplicitTop = 8
    ExplicitWidth = 521
    ExplicitHeight = 425
  end
  object pRight: TPanel
    Left = 463
    Top = 0
    Width = 83
    Height = 433
    Align = alRight
    AutoSize = True
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    ExplicitLeft = 461
    object bLoad: TButton
      Left = 4
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Load'
      TabOrder = 0
      OnClick = bLoadClick
    end
  end
end
