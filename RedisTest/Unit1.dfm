object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 468
  ClientWidth = 700
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 377
    Height = 468
    Align = alLeft
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 488
    Top = 216
    Width = 161
    Height = 81
    Caption = 'BitBtn1'
    TabOrder = 1
  end
  object IdTCPClient1: TIdTCPClient
    ConnectTimeout = 0
    Port = 0
    ReadTimeout = -1
    Left = 344
    Top = 232
  end
end
