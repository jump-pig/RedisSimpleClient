object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 468
  ClientWidth = 774
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
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
    Left = 408
    Top = 16
    Width = 161
    Height = 81
    Caption = 'BitBtn1'
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 575
    Top = 16
    Width = 153
    Height = 81
    Caption = 'BitBtn2'
    TabOrder = 2
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 400
    Top = 128
    Width = 137
    Height = 65
    Caption = 'BitBtn3'
    TabOrder = 3
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 560
    Top = 128
    Width = 65
    Height = 65
    Caption = 'BitBtn4'
    TabOrder = 4
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 408
    Top = 209
    Width = 105
    Height = 41
    Caption = 'BitBtn5'
    TabOrder = 5
    OnClick = BitBtn5Click
  end
  object BitBtn6: TBitBtn
    Left = 544
    Top = 208
    Width = 97
    Height = 41
    Caption = 'BitBtn6'
    TabOrder = 6
    OnClick = BitBtn6Click
  end
  object BitBtn7: TBitBtn
    Left = 408
    Top = 256
    Width = 121
    Height = 57
    Caption = 'BitBtn7'
    TabOrder = 7
    OnClick = BitBtn7Click
  end
  object BitBtn8: TBitBtn
    Left = 400
    Top = 344
    Width = 88
    Height = 32
    Caption = 'sadd'
    TabOrder = 8
    OnClick = BitBtn8Click
  end
  object BitBtn9: TBitBtn
    Left = 494
    Top = 344
    Width = 88
    Height = 32
    Caption = 'SCARD'
    TabOrder = 9
    OnClick = BitBtn9Click
  end
  object BitBtn10: TBitBtn
    Left = 400
    Top = 382
    Width = 88
    Height = 32
    Caption = 'SISMEMBER'
    TabOrder = 10
    OnClick = BitBtn10Click
  end
  object BitBtn11: TBitBtn
    Left = 494
    Top = 382
    Width = 88
    Height = 32
    Caption = 'SMEMBERS'
    TabOrder = 11
    OnClick = BitBtn11Click
  end
  object BitBtn12: TBitBtn
    Left = 400
    Top = 420
    Width = 88
    Height = 32
    Caption = 'SREM'
    TabOrder = 12
    OnClick = BitBtn12Click
  end
  object BitBtn13: TBitBtn
    Left = 632
    Top = 343
    Width = 96
    Height = 33
    Caption = 'SETRANGE'
    TabOrder = 13
    OnClick = BitBtn13Click
  end
  object BitBtn15: TBitBtn
    Left = 632
    Top = 380
    Width = 96
    Height = 33
    Caption = 'GETRANGE'
    TabOrder = 14
    OnClick = BitBtn15Click
  end
  object IdTCPClient1: TIdTCPClient
    ConnectTimeout = 0
    IPVersion = Id_IPv4
    Port = 0
    ReadTimeout = -1
    Left = 344
    Top = 232
  end
end
