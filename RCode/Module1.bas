Attribute VB_Name = "Module1"
'Option Explicit

Dim row As Integer
Dim openP As Integer
Dim highP As Integer
Dim lowP As Integer
Dim mx_lng As Integer
Dim mx_sht As Integer
Dim wn As Integer
Dim ls As Integer
Dim df As Long
Dim PTarg As Integer
Dim SLoss As Integer
Dim pl As Long
Dim dayPL As Long
Dim bTrade As Boolean
Dim resCol As Integer

Public Sub Run()

Sheet3.Activate
FwdMALong
'FwdMAShort

End Sub

Public Sub FwdMALong()

wn = 0
ls = 0
row = 20
PTarg = 200
SLoss = 200
pl = 0
dayPL = 0
bTrade = True
resCol = 19

'Sheet7.Activate

Do While Cells(row + 5, 13) <> "" 'row M, for lng


dayPL = 0

If Cells(row, 7) > 0 Then

    resCol = resCol + 1
    If resCol = 25 Then
        resCol = 20
    End If

    bTrade = True
    openP = Cells(row, 2)
    highP = Cells(row, 3)
    lowP = Cells(row, 4)
    
    If highP - openP > PTarg Then
        dayPL = PTarg
        pl = pl + dayPL
        Cells(row, resCol) = dayPL
        bTrade = False
    End If
    If bTrade = True Then
        If openP - lowP > SLoss Then
            dayPL = -SLoss
            pl = pl + dayPL
            Cells(row, resCol) = dayPL
            bTrade = False
        End If
    End If
    Cells(row, resCol) = Cells(row, 5) - openP
    If bTrade = True Then
        For i = 1 To 4
            highP = Cells(row + i, 3)
            lowP = Cells(row + i, 4)
            If highP - openP > PTarg Then
                dayPL = PTarg
                pl = pl + dayPL
                Cells(row + i, resCol) = PTarg
                bTrade = False
                Exit For
            End If
            If openP - lowP > SLoss Then
                dayPL = -SLoss
                pl = pl + dayPL
                Cells(row + i, resCol) = dayPL
                bTrade = False
                Exit For
            End If
            Cells(row + i, resCol) = Cells(row + i, 5) - openP
        Next i
    End If
    
    If bTrade = True Then
        dayPL = Cells(row + 5, 5) - openP
        pl = pl + dayPL
        Cells(row + i, resCol) = dayPL
    End If
End If
row = row + 1
Loop

Cells(7, 10) = pl


End Sub

Public Sub FwdMAShort()

wn = 0
ls = 0
row = 20
PTarg = 200
SLoss = 200
pl = 0
bTrade = True
resCol = 28
dayPL = 0

'Sheet7.Activate

Do While Cells(row + 5, 13) <> "" 'row M, for lng
If Cells(row, 7) < 0 Then

    resCol = resCol + 1
    If resCol = 33 Then
        resCol = 28
    End If

    bTrade = True
    openP = Cells(row, 2)
    highP = Cells(row, 3)
    lowP = Cells(row, 4)
    'mx_lng = Cells(row, 14)
    'mx_sht = Cells(row, 16)
    
    If openP - lowP > PTarg Then
        dayPL = PTarg
        pl = pl + dayPL
        Cells(row, resCol) = dayPL
        bTrade = False
    End If
    If bTrade = True Then
        If highP - openP > SLoss Then
            dayPL = -SLoss
            pl = pl + dayPL
            Cells(row, resCol) = dayPL
            bTrade = False
        End If
    End If
    
    If bTrade = True Then
        For i = 1 To 4
            highP = Cells(row + i, 3)
            lowP = Cells(row + i, 4)
            If openP - lowP > PTarg Then
                dayPL = PTarg
                pl = pl + dayPL
                Cells(row + i, resCol) = dayPL
                bTrade = False
                Exit For
            End If
            If highP - openP > SLoss Then
                dayPL = -SLoss
                pl = pl + dayPL
                Cells(row + i, resCol) = dayPL
                bTrade = False
                Exit For
            End If
            Cells(row + i, resCol) = openP - Cells(row + i, 5)
        Next i
    End If
    
    If bTrade = True Then
        dayPL = openP - Cells(row + 5, 5)
        pl = pl + dayPL
        Cells(row + i, resCol) = dayPL
    End If
End If
row = row + 1
Loop

Cells(8, 10) = pl

End Sub

Public Sub FwdMA()

Dim row As Integer
Dim openP As Integer
Dim mx_lng As Integer
Dim mx_sht As Integer
Dim wn As Integer
Dim ls As Integer
Dim df As Long

wn = 0
ls = 0
row = 20

Sheet2.Activate

Do While Cells(row, 13) <> "" 'row M, for lng
    openP = Cells(row, 2)
    mx_lng = Cells(row, 14)
    mx_sht = Cells(row, 16)
    df = Cells(row, 7)
    If df < -150 Then
        'If Cells(row - 1, 7) > Cells(row - 2, 7) Then
            If mx_sht > 100 Then
                wn = wn + 1
            Else
                ls = ls + 1
            End If
        'End If
    End If
    
    row = row + 1
Loop

row = row + 2
Cells(row, 13) = "w"
Cells(row, 14) = wn
row = row + 1
Cells(row, 13) = "l"
Cells(row, 14) = ls

End Sub

Public Sub revDays()

Dim row As Integer
Dim madiff As Integer
Dim prevD As Integer
Dim currD As Integer

Dim rev As Integer 'reversed
Dim rev1 As Integer 'reversed
Dim rev2 As Integer 'reversed
Dim rev3 As Integer 'reversed

Dim cont As Integer 'continued
Dim cont1 As Integer 'continued
Dim cont2 As Integer 'continued
Dim cont3 As Integer 'continued

Sheet8.Activate

row = 33

cont = 0
cont1 = 0
cont2 = 0
cont3 = 0

rev = 0
rev1 = 0
rev2 = 0
rev3 = 0

    
Do While Cells(row, 1) <> ""
    madiff = Cells(row, 7)
    prevD = Cells(row - 1, 5) - Cells(row - 1, 2)
    currD = Cells(row, 5) - Cells(row, 2)
    
    'MAD +ve prevD DN
    If madiff > 0 Then
        If prevD < 0 Then 'down day
            If currD > 0 Then
                rev = rev + 1
            Else
                cont = cont + 1
            End If
        End If
    End If
    
    
    'MAD +ve prevD UP
    If madiff > 0 Then
        If prevD > 0 Then 'down day
            If currD < 0 Then
                rev1 = rev1 + 1
            Else
                cont1 = cont1 + 1
            End If
        End If
    End If

    
    'MAD -ve prevD UP
    If madiff < 0 Then
        If prevD > 0 Then 'down day
            If currD < 0 Then
                rev2 = rev2 + 1
            Else
                cont2 = cont2 + 1
            End If
        End If
    End If
    
    'MAD -ve prevD DN
    If madiff < 0 Then
        If prevD < 0 Then 'down day
            If currD > 0 Then
                rev3 = rev3 + 1
            Else
                cont3 = cont3 + 1
            End If
        End If
    End If

    row = row + 1
Loop

    Cells(3, 7) = "rev"
    Cells(3, 8) = rev
    Cells(4, 7) = "cont"
    Cells(4, 8) = cont

    Cells(3, 9) = "rev1"
    Cells(3, 10) = rev1
    Cells(4, 9) = "cont1"
    Cells(4, 10) = cont1

    Cells(3, 11) = "rev2"
    Cells(3, 12) = rev2
    Cells(4, 11) = "cont2"
    Cells(4, 12) = cont2

    Cells(3, 13) = "rev3"
    Cells(3, 14) = rev3
    Cells(4, 13) = "cont3"
    Cells(4, 14) = cont3


End Sub
