VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "打地鼠_Rming"
   ClientHeight    =   8370
   ClientLeft      =   225
   ClientTop       =   555
   ClientWidth     =   13905
   DrawMode        =   14  'Copy Pen
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   ScaleHeight     =   8370
   ScaleWidth      =   13905
   StartUpPosition =   2  '屏幕中心
   WindowState     =   2  'Maximized
   Begin VB.PictureBox Picture1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BorderStyle     =   0  'None
      Height          =   1935
      Left            =   2520
      ScaleHeight     =   1935
      ScaleWidth      =   1575
      TabIndex        =   5
      Top             =   3720
      Width           =   1575
   End
   Begin VB.CommandButton Command4 
      Caption         =   "结束"
      Height          =   495
      Left            =   3000
      TabIndex        =   3
      Top             =   240
      Width           =   2175
   End
   Begin VB.CommandButton Command3 
      Caption         =   "暂停"
      Height          =   495
      Left            =   480
      TabIndex        =   2
      Top             =   240
      Width           =   2175
   End
   Begin VB.Timer Timer1 
      Interval        =   1000
      Left            =   11760
      Top             =   360
   End
   Begin VB.CommandButton Command2 
      Height          =   2175
      Left            =   3000
      Style           =   1  'Graphical
      TabIndex        =   1
      Top             =   240
      Width           =   2175
   End
   Begin VB.CommandButton Command1 
      BackColor       =   &H8000000B&
      Height          =   2175
      Left            =   6240
      Style           =   1  'Graphical
      TabIndex        =   0
      Top             =   3480
      Width           =   2175
   End
   Begin VB.Label Label1 
      BeginProperty Font 
         Name            =   "长城行楷体"
         Size            =   26.25
         Charset         =   134
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   2175
      Left            =   4440
      TabIndex        =   4
      Top             =   3720
      Width           =   6255
   End
   Begin VB.Menu start 
      Caption         =   "开始游戏"
   End
   Begin VB.Menu pause 
      Caption         =   "暂停/继续"
   End
   Begin VB.Menu endgame 
      Caption         =   "结束游戏"
   End
   Begin VB.Menu exit 
      Caption         =   "退出程序"
   End
   Begin VB.Menu about 
      Caption         =   "关于作者"
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim i%, WidthPosition%(1 To 26), HeightPosition%(1 To 26), ChrAsc%, Countnum%, Passtime%
Sub Print_Chr()       '自定义显示地鼠子过程
Randomize
For i = 1 To 26
WidthPosition(i) = Int((ScaleWidth - 2175) * Rnd)
Next i

For i = 1 To 26
HeightPosition(i) = Int((ScaleHeight - 2175) * Rnd)
Next i

ChrAsc = Int(Rnd * 25 + 1)
Command2.Top = HeightPosition(ChrAsc)
Command2.Left = WidthPosition(ChrAsc)
Command2.Visible = True
Command2.Caption = ""
Command2.Picture = LoadPicture(App.Path + "\images\" & (ChrAsc + 65) & ".gif")
End Sub
Sub Print_Result()
Cls                         '清屏
Label1.Visible = True

If Passtime = 0 Then           '排除passtime为0
    Command1.Visible = False
    Command2.Visible = False
    Picture1.Visible = True
    Label1.Caption = "慢慢来，别着急！"
Else

    speed = Int(Countnum / (Passtime / 60))  '输入速度计算
    
    If speed > 30 Then      '速度超过30图片奖励
    Picture1.Visible = True
    Picture1 = LoadPicture(App.Path & "\images\1.jpg")
    End If
    
    If Command3.Visible = True Then
        If Command3.Caption = "暂停&P" Then  '判断是否处于暂停状态
        Label1.Visible = False
        Picture1.Visible = False
        End If
    End If

    Label1.Top = ScaleHeight * 0.3        '设置label出现位置和内容
    Picture1.Top = ScaleHeight * 0.3
    Label1.Caption = "你的打字速度约为" & speed & "/分钟"
End If
End Sub
Private Sub Command1_Click()  '开始按钮
Command1.Visible = False
Command3.Visible = True
Command4.Visible = True
Timer1.Enabled = True
Call Print_Chr
End Sub
Private Sub Command2_Click()   '显示地鼠
Call Print_Chr
Command1.Visible = False
End Sub
Private Sub Command3_Click()                    '暂停、继续功能
Timer1.Enabled = Command3.Caption = "继续&G"
Command2.Visible = Command3.Caption = "继续&G"
Command3.Caption = IIf(Command3.Caption = "继续&G", "暂停&P", "继续&G")
Call Print_Result
End Sub

Private Sub Command4_Click()  '结束游戏显示结果
Timer1.Enabled = False
Command2.Visible = False
Command3.Visible = False
Command4.Visible = False
Call Print_Result
End Sub
Private Sub Form_Load()    '预处理、复位
AutoRedraw = True
Command1.Picture = LoadPicture(App.Path + "\images\go.gif")
Form1.Icon = LoadPicture(App.Path + "\images\ico.ico")
Command1.Visible = True
Command1.Top = ScaleHeight * 0.5
Command1.Left = ScaleWidth * 0.4
Command2.Visible = False
Command3.Visible = False
Command4.Visible = False
Picture1.Visible = False
Label1.Visible = False
Command3.Caption = "暂停&P"
Command4.Caption = "结束&S"
pause.Caption = "暂停/继续"
start.Caption = "开始游戏"
Timer1.Interval = 1000
Countnum = 0
Passtime = 0
End Sub

Private Sub Form_KeyPress(KeyAscii As Integer)   '获得输入字母的ascii码
If KeyAscii > 90 Then
KeyAscii = KeyAscii - 32
End If
If KeyAscii = ChrAsc + 65 Then
Countnum = Countnum + 1
Call Print_Chr
End If
End Sub
Private Sub Timer1_Timer()        '计时器
Passtime = Passtime + 1
End Sub
Private Sub start_Click()                 '菜单
Call Form_Load
start.Caption = IIf(start.Caption = "开始游戏", "重新开始", "开始游戏")
End Sub
Private Sub exit_Click()               '菜单
End
End Sub
Private Sub pause_Click()             '菜单
If Command1.Visible = False And Label1.Visible = False Then   '判断是否已经开始了游戏
    Call Command3_Click
    pause.Caption = IIf(pause.Caption = "继续游戏", "暂停一下", "继续游戏")
End If
End Sub
Private Sub endgame_Click()                 '菜单
Call Command4_Click
End Sub
Private Sub about_Click()                      '菜单-关于作者
MsgBox ("刚刚学习VB一个多月，做个东西玩玩，整整做了一下午。 ")
End Sub
