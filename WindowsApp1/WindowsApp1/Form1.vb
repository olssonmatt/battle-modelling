Public Class Form1
    Dim x1, x2, y2, a, y1, b, c, c1, c2, d, e, Tx, Ty, tt, bigger As Double 'Defines each parameter as double, c is the sqrt(a*b), d/e are the lambda solution, tt = total wartime (textbox7)
    Dim battlefield, reda, bluea, gblue, gred As Graphics 'Defines the battlefield, red troops, and blue troops as graphics 


    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Dim di, da As Integer
        TextBox13.Text = ""
        TextBox4.Text = ""
        TextBox12.Text = ""
        TextBox6.Text = ""
        battlefield = PictureBox1.CreateGraphics
        reda = PictureBox2.CreateGraphics
        bluea = PictureBox3.CreateGraphics
        battlefield.Clear(Color.White)
        reda.Clear(Color.White)
        bluea.Clear(Color.White)
        di = TextBox7.Text
        da = di + 1
        TextBox7.Text = da

        fillb = False
        fillr = False
        simx()
        simy()
        rtt()
        btt()
        PerBF()

    End Sub

    Dim bfr, rt As New SolidBrush(Color.IndianRed) 'Defines the red side of the battlefield and red troops as IndianRed
    Dim bfb, bt As New SolidBrush(Color.RoyalBlue) 'Defines the blue side of the battlefield and blue troops as RoyalBlue
    Dim fillr, fillb As Boolean

    Private Sub simx()
        a = TextBox2.Text
        b = TextBox3.Text
        x1 = TextBox1.Text
        y1 = TextBox5.Text
        bigger = Math.Max(x1, y1)
        Ty = TextBox11.Text
        Tx = TextBox10.Text
        tt = TextBox7.Text
        c = Math.Sqrt(b / a)
        d = 1 + Math.Sqrt(a * b)
        e = 1 - Math.Sqrt(a * b)
        c2 = y1 / (2 * c) + x1 / 2
        c1 = y1 / (2 * c) - x1 / 2
        x2 = (-c1 * d ^ tt) + (c2 * e ^ tt) + (Tx * tt)
        TextBox4.Text = Math.Abs(x2)
    End Sub ''Simulates the battle by analytical solutions to the lanchester equations in x

    Private Sub simy()
        a = TextBox2.Text
        b = TextBox3.Text
        x1 = TextBox1.Text
        y1 = TextBox5.Text
        Ty = TextBox11.Text
        Tx = TextBox10.Text
        tt = TextBox7.Text
        c = Math.Sqrt(b / a)
        d = 1 + Math.Sqrt(a * b)
        e = 1 - Math.Sqrt(a * b)
        c2 = y1 / (2 * c) + x1 / 2
        c1 = y1 / (2 * c) - x1 / 2
        y2 = (c * c1 * d ^ tt) + (c * c2 * e ^ tt) + (Ty * tt)
        TextBox6.Text = Math.Abs(y2)
    End Sub ''Simulates the battle by analytical solutions to the lanchester equations in y

    Private Sub PerBF()
        Dim PerR, PerB As Double
        Dim DrawR, DrawB As Integer
        x1 = TextBox4.Text
        y1 = TextBox6.Text
        battlefield = PictureBox1.CreateGraphics
        PerR = x1 / (y1 + x1)
        PerB = y1 / (y1 + x1)
        TextBox13.Text = PerR * 100
        TextBox12.Text = PerB * 100
        If fillr = True Then
            battlefield.FillRectangle(bfr, 0, 0, 300, 300)
            TextBox13.Text = 100
            TextBox12.Text = 0
            TextBox6.Text = 0.00
            fillr = False
        ElseIf fillb = True Then
            battlefield.FillRectangle(bfb, 0, 0, 300, 300)
            TextBox12.Text = 100
            TextBox13.Text = 0
            TextBox4.Text = 0.00
            fillb = False
        Else
            DrawR = Math.Floor(PerR * 300)
            DrawB = 300 - Math.Ceiling(PerB * 300)
            battlefield.FillRectangle(bfr, 0, 0, 300, DrawR)
            battlefield.FillRectangle(bfb, 0, DrawB, 300, 300)
        End If


    End Sub 'Calculates the percentage of battle field in terms of total troops between both forces to help with the battlefield

    Function redf(ByVal x As Double)
        Return (-1 * c1 * d ^ x) + (c2 * e ^ x) + (Tx * x)
    End Function

    Function bluef(ByVal x As Double)
        Return (c * c1 * d ^ x) + (c * c2 * e ^ x) + (Ty * x)
    End Function

    Function xc(ByVal x As Double) As Integer
        Dim newx As Double
        newx = Int((x / tt) * PictureBox2.Width)
        Return newx
    End Function

    Function yc(ByVal x As Double) As Integer
        Dim newy As Double
        newy = Int(PictureBox2.Height - (PictureBox2.Height * (x / bigger)))
        Return newy
    End Function

    Private Sub rtt()
        Dim i As Double
        gred = PictureBox2.CreateGraphics
        Dim PRed As New Pen(Color.IndianRed)

        For i = 0 To tt Step 0.01
            gred.DrawRectangle(PRed, xc(i), yc(redf(i)), 1, 1)
            If redf(i) <= 0 Then
                fillb = True
            End If
        Next
    End Sub 'Draws the graph of the red troops

    Private Sub btt()
        Dim i As Double
        gblue = PictureBox3.CreateGraphics
        Dim PBlue As New Pen(Color.RoyalBlue)

        For i = 0 To tt Step 0.01
            gblue.DrawRectangle(PBlue, xc(i), yc(bluef(i)), 1, 1)
            If bluef(i) <= 0 Then
                fillr = True
            End If
        Next
    End Sub 'Draws the graph of the blue troops

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        fillb = False
        fillr = False
        simx()
        simy()
        rtt()
        btt()
        PerBF()

        'TextBox1.Text = c1 & c2
    End Sub 'Runs the battle model to a full simulation

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        TextBox1.Text = ""
        TextBox2.Text = ""
        TextBox3.Text = ""
        TextBox4.Text = ""
        TextBox5.Text = ""
        TextBox6.Text = ""
        TextBox7.Text = ""
        TextBox10.Text = ""
        TextBox11.Text = ""
        TextBox12.Text = ""
        TextBox13.Text = ""
        battlefield = PictureBox1.CreateGraphics
        reda = PictureBox2.CreateGraphics
        bluea = PictureBox3.CreateGraphics
        battlefield.Clear(Color.White)
        reda.Clear(Color.White)
        bluea.Clear(Color.White)
    End Sub 'clears all textboxes and picture boxes

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        End
    End Sub 'ends the program

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        If TextBox9.Visible = True Then
            TextBox9.Visible = False
        Else
            TextBox9.Visible = True
        End If
    End Sub 'displays the help textbox

End Class
