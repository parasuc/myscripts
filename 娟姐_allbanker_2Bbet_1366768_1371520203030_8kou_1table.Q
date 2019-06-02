[General]
SyntaxVersion=2
BeginHotkey=121
BeginHotkeyMod=0
PauseHotkey=0
PauseHotkeyMod=0
StopHotkey=123
StopHotkeyMod=0
RunOnce=1
EnableWindow=
MacroID=83d85a47-e5fc-495b-ad7b-daa3351993e2
Description=娟姐_allbanker_2Bbet_1366768_1371520203030_8kou_1table
Enable=1
AutoRun=0
[Repeat]
Type=0
Number=1
[SetupUI]
Type=2
QUI=
[Relative]
SetupOCXFile=
[Comment]

[Script]
//Set dm = createobject("dm.dmsoft")
If Plugin.GetSysInfo.GetDateTime > "2019-06-29 09:00" Then 
	ExitScript 
End If
logfile = "D:\" & Plugin.GetSysInfo.GetTime  & ".log"
LogStart logfile
TracePrint "任务于" & Plugin.GetSysInfo.GetDateTime & "启动！！！"

//相连两个庄开始打，相连两个闲停止打
Global bankerflag, playerflag
bankerflag = 0
playerflag = 0

//参数配置，一般投注开始的点位每个平台不一样,Global好像不能定义数组
//xarray要定义两个地方
//百家乐程序逻辑，前一个珠路的圆圈，另外，循环判断某一个点的颜色然后进行下注，然后判断本局是否已经下注，防止本局多次下注
//下注读秒开始的判断点，bbin，三个台子就是三个点，这个点的位置随着屏幕分辨率的变化而变化，可能颜色还有差距
Global colorpointx1,colorpointy1, colorpointx2,colorpointy2,colorpointx3,colorpointy3,colorpointcolor, ystart, yspace
//Global btnbanker1,btnbanker2,btnbanker3,btnplayer1,btnplayer2,btnplayer3,btnconfirm1,btnconfirm2,btnconfirm3
//需要设置的地方，一个是庄按钮，一个是确定按钮，一个是第一个圆圈xy，一个是y间隔，一个是x轴坐标（两处），一个是黄色读秒判断点xy坐标
btnbanker1 = array(429,411)
btnbanker2 = array(1, 2)
btnbanker3 = array(1, 2)
btnplayer1 = array(1, 2)
btnplayer2 = array(1, 2)
btnplayer3 = array(1, 2)
btnconfirm1 = array(420, 477)
btnconfirm2 = array(1, 2)
btnconfirm3 = array(1, 2)

//第一台的下注判断点
colorpointx1 = 381
colorpointy1 = 476
colorpointcolor = "3EEBFF"

ystart = 568 //珠路y轴第一个圈的纵坐标
yspace = 22  //y轴珠路两行间距

//桌面珠路的圆圈的位置x坐标，三个台，xarray要定义两个地方，另一个是子线程
xarray1 = array(231, 254, 275, 297, 319, 342, 363, 386, 408, 430, 452, 474, 496)

xarray2 = array(654, 681, 707, 733, 760, 786, 813, 839, 866, 892, 919, 945, 972)
xarray3 = array(1053, 1079, 1104, 1132, 1157, 1183, 1210, 1236, 1263, 1289, 1316, 1342, 1369)


//初始化一些数据
//num1代表当前局数，betwhich代表这句下什么，result代表这局的开局结果，n1本局的揽，firstbet第一局的结果娟姐的打法以第一局结果为标准，wintimes代表赢了多少个基码
//x1，y1，当前局的位置，比如第一台第一局就是x1=0 y1=0
//f1 循环取路的判断标志，代表到达了本台路单的最后
//flag代表本局是否下过注码了没有，防止重复下注
//x, y 代表上一局的珠路的横坐标纵坐标位置
//lastbetstate1 当前输了第几把

Global ludan1,ludan2,ludan3,result1(90),result2(90),result3(90),num1,num2,num3,bet1(90),bet2(90),bet3(90),betwhich1(90), betwhich2(90), betwhich3(90)
Global flag1(90), flag2(90), flag3(90),lastbetstate1,lastbetstate2,lastbetstate3
Global x, y
Global x1, y1, x2, y2, x3, y3, f1, f2, f3, wintimes1,wintimes2,wintimes3, wintimes , n1,n2,n3, firstbet1,firstbet2,firstbet3

x1 = 0
y1 = 0
x2 = 0
y2 = 0
x3 = 0
y3 = 0
x = 0
y = 0
firstbet1 = ""
firstbet2 = ""
firstbet3 = ""
wintimes1 = 0
wintimes2 = 0
wintimes3 = 0
wintimes = 0 

lastbetstate1 = 0
lastbetstate2 = 0
lastbetstate3 = 0

n1 = 0
n2 = 0
n3 = 0

//取路循环结束flag
f1 = 1
f2 = 1
f3 = 1

For m = 0 To 90
	result1(m) = ""
	result2(m) = ""
	result3(m) = ""
	bet1(m) = 0
	bet2(m) = 0
	bet3(m) = 0
	flag1(m) = 0
	flag2(m) = 0
	flag3(m) = 0
	betwhich1(m)=""
	betwhich2(m)=""
	betwhich3(m)=""
Next


Sub getNextResult1
	num1 = 6 * x1 + y1 + 1
	IfColor xarray1(0), ystart, "FFFFFF", 2 Then
		f1 = 0
		x1 = 0
		y1 = 0
		num1 = 0
		ludan1 = ""
		lastbetstate1 = 0
		firstbet1 = ""
		For xx = 0 To 90
			result1(xx) = ""
			bet1(xx) = 0
			flag1(xx) = 0
			betwhich1(xx)=""
		Next
		Exit Sub
	End If
	Call firstbetinit1
	IfColor xarray1(x1), ystart + yspace * y1, "FFFFFF", 2 Then
		f1 = 0
		Exit Sub 
	End If
	IfColor xarray1(x1),ystart + yspace * y1, "3333FF", 2 Then
		result1(num1) = "B"
		ludan1 = ludan1 + "B"
		If y1 = 5 Then 
			x1 = x1 + 1
			y1 = 0
		Else 
			y1 = y1 + 1
		End If
		Exit Sub 
	End If
	IfColor xarray1(x1),ystart + yspace * y1, "FF6600", 2 Then
		result1(num1) = "P"
		ludan1 = ludan1 + "P"
		If y1 = 5 Then 
			x1 = x1 + 1
			y1 = 0
		Else 
			y1 = y1 + 1
		End If
		Exit Sub 
	End If
	IfColor xarray1(x1),ystart + yspace * y1, "00CC00", 2 Then
		result1(num1) = "T"
		ludan1 = ludan1 + "T"
		If y1 = 5 Then 
			x1 = x1 + 1
			y1 = 0
		Else 
			y1 = y1 + 1
		End If
		Exit Sub 
	End If
End Sub


Sub bankerClick1(n)
	MoveTo btnbanker1(0),btnbanker1(1)
	Delay 200
	LeftClick n
	MoveTo btnconfirm1(0), btnconfirm1(1)
	Delay 400
	LeftClick 1
End Sub
Sub bankerClick2(n)
	MoveTo btnbanker2(0),btnbanker2(1)
	Delay 200
	LeftClick n
	MoveTo btnconfirm2(0), btnconfirm2(1)
	Delay 400
	LeftClick 1
End Sub
Sub bankerClick3(n)
	MoveTo btnbanker3(0),btnbanker3(1)
	Delay 200
	LeftClick n
	MoveTo btnconfirm3(0), btnconfirm3(1)
	Delay 400
	LeftClick 1
End Sub


Sub playerClick1(n)
	MoveTo btnplayer1(0),btnplayer1(1)
	Delay 200
	LeftClick n
	MoveTo btnconfirm1(0), btnconfirm1(1)
	Delay 400
	LeftClick 1
End Sub
Sub playerClick2(n)
	MoveTo btnplayer2(0),btnplayer2(1)
	Delay 200
	LeftClick n
	MoveTo btnconfirm2(0), btnconfirm2(1)
	Delay 400
	LeftClick 1
End Sub
Sub playerClick3(n)
	MoveTo btnplayer3(0),btnplayer3(1)
	Delay 200
	LeftClick n
	MoveTo btnconfirm3(0), btnconfirm3(1)
	Delay 400
	LeftClick 1
End Sub


Sub getLudanLoop
	xarray1 = array(231, 254, 275, 297, 319, 342, 363, 386, 408, 430, 452, 474, 496)
	xarray2 = array(654, 681, 707, 733, 760, 786, 813, 839, 866, 892, 919, 945, 972)
	xarray3 = array(1053, 1079, 1104, 1132, 1157, 1183, 1210, 1236, 1263, 1289, 1316, 1342, 1369)
	While True
		Do While True
			Call getNextResult1
			If f1 = 0 Then 
				Exit Do
			End If
		Loop
		Delay 1000
	Wend
	
End Sub 


//这三个循环在子线程循环之前，初始化一次，因为x1还是0，getNextResult1是根据当前的xy确定下一个珠路
Do While True
	Call getNextResult1
	If f1 = 0 Then 
		Exit Do
	End If
Loop

TracePrint "Road Ready"
Call Plugin.Media.Beep(988, 1000)
//打印卤蛋
TracePrint num1 &":"&ludan1
TracePrint num2 &":"&ludan2
TracePrint num3 & ":" & ludan3
TracePrint bankerflag

//多线程同时运行
Dimenv envVar
threadId = BeginThread(getLudanLoop)


//第一局的庄闲判断作为下注的依据，娟姐的打法
Sub firstbetinit1()
	If firstbet1 = "" Then
		If result1(1) <> "T" Then 
			firstbet1 = result1(1)
		ElseIf result1(2) <> "T"
			firstbet1 = result1(2)
		ElseIf result1(3) <> "T"
			firstbet1 = result1(3)
		ElseIf result1(4) <> "T"
			firstbet1 = result1(4)
		ElseIf result1(5) <> "T"
			firstbet1 = result1(5)
		End If
		TracePrint firstbet1
		If firstbet1 = "B" Then 
			secondbet1 = "P"
		Else 
			secondbet1 = "B"
		End If
	End If
End Sub 

//上一轮的x，y数值，通用的函数，用于获取上一局珠路的坐标，从而判断上一局下注的输赢结果
Sub xypos(x11, y11)
	If x11 = 0 and y11 = 0 Then 
	Else 
		If y11 = 0 Then 
			y11 = 5
			If x11 > 0 Then 
				x11 = x11 - 1
			Else 
				x11 = 0
			End If 
		Else 
			y11 = y11-1
		End If
	End If
	x = x11
	y = y11
	//TracePrint x & "," & y
	//TracePrint "x1:"&x1&",y1:"&y1
End Sub

//正下住，也就是单数路是正下，娟姐的私家路
Sub positivebet1(clicks)
	If firstbet1 = "B" Then
		Delay 5000
		Call bankerClick1(clicks)
		betwhich1(num1) = "B"
	Else 
		Delay 5000
		Call playerClick1(clicks)
		betwhich1(num1) = "P"
	End If
	bet1(num1) = clicks
End Sub


Sub negbet1(clicks)
	If firstbet1 = "B" Then
		Delay 5000
		Call playerClick1(clicks)
		betwhich1(num1) = "P"
	Else 
		Delay 5000
		Call bankerClick1(clicks)
		betwhich1(num1) = "B"
	End If
	bet1(num1) = clicks
End Sub


//主循环
While True
	Delay 1000
	IfColor colorpointx1,colorpointy1, colorpointcolor, 2 Then
		//TracePrint "投注"
		Delay 500
		If flag1(num1) = 0 Then //flag是判断本轮是否下注了，一般是90局以下
			flag1(num1) = 1
			
			If num1 > 2 Then 
				temp1 = 1
				temp2 = 1
				While result1(num1 - temp1) = "T"
					temp1 = temp1 + 1
				Wend
				While result1(num1 - temp1 - temp2) = "T"
					temp2 = temp2 + 1
				Wend
				TracePrint result1(num1 - temp1)
				TracePrint result1(num1 - temp1 - temp2)
				If result1(num1 - temp1) = "B" and result1(num1 - temp1 - temp2) = "B" Then 
					bankerflag = 1
					playerflag = 0
				End If
				If result1(num1 - temp1) = "P" and result1(num1 - temp1 - temp2) = "P" Then 
					bankerflag = 0
					playerflag = 1
				End If
			End If
			
			TracePrint "-----------------------------------------------------------"
			TracePrint "bankerflag ="& bankerflag
			If betwhich1(num1 - 1) <> "" Then
				//TracePrint num1
				If result1(num1 - 1) <> "T" Then 
					If result1(num1 - 1) = betwhich1(num1-1) Then //如果赢了
						lastbetstate1 = 0
						wintimes1 = wintimes1 + n1
						TracePrint "赢 !，连输"&lastbetstate1
					Else
								lastbetstate1 = lastbetstate1 + 1
								wintimes1 = wintimes1 - n1
								If lastbetstate1 > 5 Then 
									Call Plugin.Media.Beep(988, 500)
								End If
								If lastbetstate1 > 7 Then  //2^7=128 2^5=32，137最大三百多
									lastbetstate1 = 0
								End If
								TracePrint " 输 ! ，连输："&lastbetstate1
					End If
				End If
			End If
			
			TracePrint "1桌第"&(num1-1)&"局,y1="& y1  &"，利润: "&wintimes1 & " ，揽数"&lastbetstate1
			
			//124公式
			//n1 = 2 ^ lastbetstate1
			//137公式
			n1 = 2 ^ (lastbetstate1 + 1) - 1
			
			//折中打法，就是31那口打成20，1371520203030公式
			If lastbetstate1 = 4 or lastbetstate1 = 5 Then 
				n1 = 20
			End If
			If lastbetstate1 = 6 or lastbetstate1 = 7 Then 
				n1 = 30
			End If
			
			
			If lastbetstate1 = 0 and num1 > 52 Then //第60局以上就不下了
				//pass
			Else 
				If num1 > 2 Then 
					//两个方案，一个是隔4口打一口，一个是两庄再打，注码一个是137正规，一个是1371520203030
					//If num1 mod 4 =0 Then
					If bankerflag = 1 Then 
						Call bankerClick1(n1)
						betwhich1(num1) = "B"//目前只打庄
						bet1(num1) = n1
						TracePrint "下注 "&n1
					End If
				End If
			End If

		End If
	End If

Wend