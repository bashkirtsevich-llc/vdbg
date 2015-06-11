object ConfigForm: TConfigForm
  Left = 0
  Top = 0
  Caption = #1050#1086#1085#1092#1080#1075#1091#1088#1072#1094#1080#1103' '#1074#1080#1088#1090#1091#1072#1083#1100#1085#1086#1081' '#1084#1072#1096#1080#1085#1099
  ClientHeight = 368
  ClientWidth = 571
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 488
    Top = 338
    Width = 75
    Height = 25
    Caption = #1047#1072#1082#1088#1099#1090#1100
    ModalResult = 2
    TabOrder = 3
  end
  object Button2: TButton
    Left = 105
    Top = 338
    Width = 75
    Height = 25
    Caption = #1047#1072#1087#1080#1089#1072#1090#1100
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 16
    Top = 338
    Width = 75
    Height = 25
    Caption = #1055#1077#1088#1077#1095#1080#1090#1072#1090#1100
    TabOrder = 1
    OnClick = Button3Click
  end
  object Panel1: TPanel
    Left = 16
    Top = 8
    Width = 547
    Height = 324
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 545
      Height = 322
      ActivePage = TabSheet3
      Align = alClient
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = #1057#1080#1089#1090#1077#1084#1072
        object Label2: TLabel
          Left = 16
          Top = 72
          Width = 129
          Height = 13
          Caption = #1047#1072#1075#1088#1091#1079#1086#1095#1085#1086#1077' '#1091#1089#1090#1088#1086#1081#1089#1090#1074#1086':'
        end
        object Label1: TLabel
          Left = 16
          Top = 17
          Width = 62
          Height = 13
          Caption = #1055#1072#1084#1103#1090#1100', '#1052#1073':'
        end
        object lblBios: TLabel
          Left = 16
          Top = 152
          Width = 28
          Height = 13
          Caption = #1041#1048#1054#1057
        end
        object lblVgaBios: TLabel
          Left = 19
          Top = 198
          Width = 51
          Height = 13
          Caption = 'VGA '#1041#1048#1054#1057
        end
        object lblFont: TLabel
          Left = 19
          Top = 246
          Width = 50
          Height = 13
          Caption = 'VGA FONT'
        end
        object lbBoot: TComboBox
          Left = 16
          Top = 88
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 1
          Text = #1053#1043#1052#1044
          Items.Strings = (
            #1053#1043#1052#1044
            #1053#1046#1052#1044
            #1050#1086#1084#1087#1072#1082#1090'-'#1076#1080#1089#1082)
        end
        object lbMemory: TEdit
          Left = 16
          Top = 32
          Width = 143
          Height = 21
          TabOrder = 0
          Text = '32'
        end
        object lbFPU: TCheckBox
          Left = 16
          Top = 129
          Width = 233
          Height = 17
          Caption = #1042#1082#1083#1102#1095#1080#1090#1100' '#1087#1086#1076#1076#1077#1088#1078#1082#1091' '#1084#1072#1090'. '#1089#1086#1087#1088#1086#1094#1077#1089#1089#1086#1088#1072
          TabOrder = 2
        end
        object edtBios: TEdit
          Left = 16
          Top = 171
          Width = 465
          Height = 21
          TabOrder = 4
          Text = 'edtBios'
        end
        object edtVgaBios: TEdit
          Left = 16
          Top = 217
          Width = 465
          Height = 21
          TabOrder = 6
          Text = 'edtVgaBios'
        end
        object btnBrowsBios: TButton
          Left = 487
          Top = 169
          Width = 32
          Height = 25
          Caption = '...'
          TabOrder = 3
          OnClick = btnBrowsBiosClick
        end
        object btnBrowsVgaBios: TButton
          Left = 487
          Top = 215
          Width = 32
          Height = 25
          Caption = '...'
          TabOrder = 5
          OnClick = btnBrowsVgaBiosClick
        end
        object edtFont: TEdit
          Left = 16
          Top = 265
          Width = 465
          Height = 21
          TabOrder = 8
          Text = 'edtVgaFont'
        end
        object btnBrows3: TButton
          Left = 487
          Top = 263
          Width = 32
          Height = 25
          Caption = '...'
          TabOrder = 7
          OnClick = btnBrows3Click
        end
      end
      object TabSheet2: TTabSheet
        Caption = #1053#1046#1052#1044
        ImageIndex = 1
        object Label7: TLabel
          Left = 16
          Top = 176
          Width = 47
          Height = 13
          Caption = #1057#1077#1082#1090#1086#1088#1072':'
        end
        object Label6: TLabel
          Left = 16
          Top = 122
          Width = 46
          Height = 13
          Caption = #1043#1086#1083#1086#1074#1082#1080':'
        end
        object Label5: TLabel
          Left = 16
          Top = 64
          Width = 57
          Height = 13
          Caption = #1062#1080#1083#1080#1085#1076#1088#1099':'
        end
        object Label4: TLabel
          Left = 16
          Top = 240
          Width = 109
          Height = 13
          Caption = #1055#1091#1090#1100' '#1082' '#1086#1073#1088#1072#1079#1091' '#1076#1080#1089#1082#1072':'
        end
        object Label10: TLabel
          Left = 219
          Top = 64
          Width = 110
          Height = 13
          Caption = #1054#1073#1097#1080#1081' '#1088#1072#1079#1084#1077#1088' '#1076#1080#1089#1082#1072':'
        end
        object lbSectors: TEdit
          Left = 16
          Top = 195
          Width = 129
          Height = 21
          TabOrder = 5
          Text = 'lbSectors'
          OnChange = lbCylinderChange
        end
        object lbHeads: TEdit
          Left = 16
          Top = 141
          Width = 129
          Height = 21
          TabOrder = 4
          Text = 'lbHeads'
          OnChange = lbCylinderChange
        end
        object lbCylinder: TEdit
          Left = 16
          Top = 83
          Width = 129
          Height = 21
          TabOrder = 1
          Text = 'lbCylinder'
          OnChange = lbCylinderChange
        end
        object lbHDDFile: TEdit
          Left = 16
          Top = 259
          Width = 473
          Height = 21
          TabOrder = 6
          Text = 'lbHDDFile'
        end
        object lbHDDPresent: TCheckBox
          Left = 16
          Top = 16
          Width = 113
          Height = 17
          Caption = #1053#1046#1052#1044' '#1087#1086#1076#1082#1083#1102#1095#1077#1085
          TabOrder = 0
        end
        object lbDiskSize: TEdit
          Left = 219
          Top = 83
          Width = 154
          Height = 21
          ReadOnly = True
          TabOrder = 2
          Text = 'lbDiskSize'
        end
        object Button4: TButton
          Left = 495
          Top = 259
          Width = 32
          Height = 25
          Caption = '..'
          TabOrder = 7
          OnClick = Button4Click
        end
        object Button7: TButton
          Left = 219
          Top = 139
          Width = 154
          Height = 25
          Caption = #1057#1086#1079#1076#1072#1090#1100' '#1087#1091#1089#1090#1086#1081' '#1076#1080#1089#1082
          TabOrder = 3
          OnClick = Button7Click
        end
      end
      object TabSheet3: TTabSheet
        Caption = #1053#1043#1052#1044
        ImageIndex = 2
        object grpFloppyA: TGroupBox
          Left = 19
          Top = 19
          Width = 238
          Height = 262
          Caption = #1060#1083#1086#1087#1087#1080' A'
          TabOrder = 0
          object Label3: TLabel
            Left = 14
            Top = 21
            Width = 98
            Height = 13
            Caption = #1058#1080#1087' '#1075#1080#1073#1082#1086#1075#1086' '#1076#1080#1089#1082#1072':'
          end
          object Label12: TLabel
            Left = 14
            Top = 109
            Width = 144
            Height = 13
            Caption = #1048#1089#1087#1086#1083#1100#1079#1086#1074#1072#1090#1100' '#1076#1080#1089#1082#1086#1074#1086#1076' '#1055#1050':'
          end
          object Label9: TLabel
            Left = 14
            Top = 205
            Width = 191
            Height = 13
            Caption = #1055#1091#1090#1100' '#1082' '#1086#1073#1088#1072#1079#1091' '#1076#1080#1089#1082#1072' '#1080#1083#1080' '#1091#1089#1090#1088#1086#1081#1089#1090#1074#1091':'
          end
          object lbFloppyAType: TComboBox
            Left = 14
            Top = 40
            Width = 191
            Height = 21
            Style = csDropDownList
            ItemIndex = 0
            TabOrder = 0
            Text = #1054#1090#1089#1091#1090#1089#1090#1074#1091#1077#1090
            Items.Strings = (
              #1054#1090#1089#1091#1090#1089#1090#1074#1091#1077#1090
              '1.2M  5.25"'
              '1.44M 3.5"'
              '2.88M 3.5"'
              '720K  3.5"')
          end
          object lbFloppyADrive: TComboBox
            Left = 14
            Top = 128
            Width = 191
            Height = 21
            Style = csDropDownList
            TabOrder = 2
            OnSelect = lbFloppyADriveSelect
          end
          object Button9: TButton
            Left = 14
            Top = 168
            Width = 123
            Height = 25
            Caption = #1057#1086#1079#1076#1072#1090#1100' '#1087#1091#1089#1090#1086#1081
            TabOrder = 3
            OnClick = Button9Click
          end
          object lbFloppyAFile: TEdit
            Left = 14
            Top = 224
            Width = 153
            Height = 21
            TabOrder = 5
            Text = 'lbFloppyAFile'
          end
          object Button5: TButton
            Left = 173
            Top = 222
            Width = 32
            Height = 25
            Caption = '..'
            TabOrder = 4
            OnClick = Button5Click
          end
          object chkFloppyAReadOnly: TCheckBox
            Left = 14
            Top = 80
            Width = 191
            Height = 17
            Caption = #1058#1086#1083#1100#1082#1086' '#1076#1083#1103' '#1095#1090#1077#1085#1080#1103
            TabOrder = 1
          end
        end
        object grpFloppyB: TGroupBox
          Left = 280
          Top = 19
          Width = 237
          Height = 262
          Caption = #1060#1083#1086#1087#1087#1080' B'
          TabOrder = 1
          object Label13: TLabel
            Left = 14
            Top = 21
            Width = 98
            Height = 13
            Caption = #1058#1080#1087' '#1075#1080#1073#1082#1086#1075#1086' '#1076#1080#1089#1082#1072':'
          end
          object Label14: TLabel
            Left = 14
            Top = 109
            Width = 144
            Height = 13
            Caption = #1048#1089#1087#1086#1083#1100#1079#1086#1074#1072#1090#1100' '#1076#1080#1089#1082#1086#1074#1086#1076' '#1055#1050':'
          end
          object Label15: TLabel
            Left = 14
            Top = 205
            Width = 191
            Height = 13
            Caption = #1055#1091#1090#1100' '#1082' '#1086#1073#1088#1072#1079#1091' '#1076#1080#1089#1082#1072' '#1080#1083#1080' '#1091#1089#1090#1088#1086#1081#1089#1090#1074#1091':'
          end
          object lbFloppyBType: TComboBox
            Left = 14
            Top = 40
            Width = 191
            Height = 21
            Style = csDropDownList
            ItemIndex = 0
            TabOrder = 0
            Text = #1054#1090#1089#1091#1090#1089#1090#1074#1091#1077#1090
            Items.Strings = (
              #1054#1090#1089#1091#1090#1089#1090#1074#1091#1077#1090
              '1.2M  5.25"'
              '1.44M 3.5"'
              '2.88M 3.5"'
              '720K  3.5"')
          end
          object lbFloppyBDrive: TComboBox
            Left = 14
            Top = 128
            Width = 191
            Height = 21
            Style = csDropDownList
            TabOrder = 2
            OnSelect = lbFloppyADriveSelect
          end
          object Button10: TButton
            Left = 14
            Top = 168
            Width = 123
            Height = 25
            Caption = #1057#1086#1079#1076#1072#1090#1100' '#1087#1091#1089#1090#1086#1081
            TabOrder = 3
            OnClick = Button9Click
          end
          object lbFloppyBFile: TEdit
            Left = 14
            Top = 224
            Width = 153
            Height = 21
            TabOrder = 5
            Text = 'lbFloppyBFile'
          end
          object Button11: TButton
            Left = 173
            Top = 222
            Width = 32
            Height = 25
            Caption = '..'
            TabOrder = 4
            OnClick = Button11Click
          end
          object chkFloppyBReadOnly: TCheckBox
            Left = 14
            Top = 80
            Width = 191
            Height = 17
            Caption = #1058#1086#1083#1100#1082#1086' '#1076#1083#1103' '#1095#1090#1077#1085#1080#1103
            TabOrder = 1
          end
        end
      end
      object TabSheet4: TTabSheet
        Caption = #1050#1086#1084#1087#1072#1082#1090' '#1076#1080#1089#1082
        ImageIndex = 3
        object Label8: TLabel
          Left = 3
          Top = 193
          Width = 191
          Height = 13
          Caption = #1055#1091#1090#1100' '#1082' '#1086#1073#1088#1072#1079#1091' '#1076#1080#1089#1082#1072' '#1080#1083#1080' '#1091#1089#1090#1088#1086#1081#1089#1090#1074#1091':'
        end
        object Label11: TLabel
          Left = 3
          Top = 117
          Width = 132
          Height = 13
          Caption = #1048#1089#1087#1086#1083#1100#1079#1086#1074#1072#1090#1100' '#1087#1088#1080#1074#1086#1076' '#1055#1050':'
        end
        object Button6: TButton
          Left = 491
          Top = 210
          Width = 32
          Height = 25
          Caption = '..'
          TabOrder = 2
          OnClick = Button6Click
        end
        object lbCDROMFile: TEdit
          Left = 3
          Top = 212
          Width = 482
          Height = 21
          TabOrder = 3
          Text = 'lbCDROMFile'
        end
        object lbCDRomPresent: TCheckBox
          Left = 14
          Top = 16
          Width = 163
          Height = 17
          Caption = #1050#1086#1084#1087#1072#1082#1090' '#1076#1080#1089#1082' '#1087#1088#1080#1089#1091#1090#1089#1090#1074#1091#1077#1090
          TabOrder = 0
        end
        object Button8: TButton
          Left = 3
          Top = 239
          Width = 155
          Height = 25
          Caption = #1057#1086#1079#1076#1072#1090#1100' '#1086#1073#1088#1072#1079' '#1076#1080#1089#1082#1072
          TabOrder = 4
          OnClick = Button8Click
        end
        object lbCDROMDrive: TComboBox
          Left = 3
          Top = 136
          Width = 482
          Height = 21
          Style = csDropDownList
          TabOrder = 1
          OnSelect = lbCDROMDriveSelect
        end
      end
    end
  end
end
