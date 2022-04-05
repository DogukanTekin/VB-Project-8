Public Class Form1
    Dim ogrenci_sayisi As String
    Dim zorunlu_akts, secmeli_akts As Integer
    Dim zorunlu_dizi_akts() As Integer = {2, 3, 4, 2, 3, 2, 3, 4, 6, 2}
    Dim secmeli_dizi_akts() As Integer = {3, 3, 2, 3, 3, 2}
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        zorunlucheckbox.CheckOnClick = True
        secmelicheckbox.CheckOnClick = True
        Dim kredi2 As String = "2 AKTS"
        zorunlucheckbox.Items.Add("ATA 1007 Atatürk İlkeleri ve İnkılap Tarihi 1 A Şubesi")
        zorunlucheckbox.Items.Add("UBP 4203 Yazılım Mimarileri A Şubesi")
        zorunlucheckbox.Items.Add("UBP 4205 Görsel Programlama 2 A Şubesi")
        zorunlucheckbox.Items.Add("UBP 4219 Mobil Uygulama Geliştirme 1 A Şubesi")
        zorunlucheckbox.Items.Add("UBP 4211 Açık Kaynak İşletim Sistemi A Şubesi")
        zorunlucheckbox.Items.Add("ATA 1008 Atatürk İlkeleri ve İnkılap Tarihi 1 B Şubesi")
        zorunlucheckbox.Items.Add("UBP 4204 Yazılım Mimarileri B Şubesi")
        zorunlucheckbox.Items.Add("UBP 4206 Görsel Programlama 2 B Şubesi")
        zorunlucheckbox.Items.Add("UBP 4201 Nesne Tabanlı Programlama 1 A Şubesi")
        zorunlucheckbox.Items.Add("UBP 4207 Mesleki Yabancı Dil 1 A Şubesi")
        secmelicheckbox.Items.Add("UBP 4215 .NET Uygulama Geliştirme A Şubesi")
        secmelicheckbox.Items.Add("UİŞ 4001 İşletme Yönetimine Giriş A Şubesi")
        secmelicheckbox.Items.Add("UME 4000 Meslek Etiği A Şubesi")
        secmelicheckbox.Items.Add("UBP 4216 .NET Uygulama Geliştirme B Şubesi")
        secmelicheckbox.Items.Add("UİŞ 4002 İşletme Yönetimine Giriş B Şubesi")
        secmelicheckbox.Items.Add("UME 4001 Meslek Etiği B Şubesi")
        Dim sayisal As Boolean = False
        Do While (sayisal = False)
            ogrenci_sayisi = InputBox("Kaydedilecek Öğrenci Sayısını Giriniz")
            If IsNumeric(ogrenci_sayisi) Then
                If CInt(ogrenci_sayisi) < 0 Then
                    MsgBox("Lütfen 0'dan Küçük Sayı Girmeyiniz")
                Else
                    sayisal = True
                End If
            ElseIf ogrenci_sayisi = "" Then
                MsgBox("Lütfen Boş Bırakmayınız")
            Else
                MsgBox("Lütfen Sadece Sayısal Değer Giriniz")
            End If
        Loop
        CheckBox1.Checked = True
        CheckBox3.Checked = True
        CheckBox5.Checked = True
    End Sub

    Private Sub TextBox2_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox2.KeyPress
        If Not (Char.IsNumber(e.KeyChar) = True) And e.KeyChar <> ChrW(Keys.Back) Then
            e.Handled = True
        End If
        TextBox2.MaxLength = 10
    End Sub

    Private Sub TextBox1_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox1.KeyPress
        If Not (Char.IsNumber(e.KeyChar) = True) And e.KeyChar <> ChrW(Keys.Back) Then
            e.Handled = True
        End If
        TextBox1.MaxLength = 11
    End Sub

    Private Sub TextBox3_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox3.KeyPress
        If Not (Char.IsLetter(e.KeyChar) = True) And e.KeyChar <> ChrW(Keys.Back) And e.KeyChar <> ChrW(Keys.Space) Then
            e.Handled = True
        End If
        TextBox3.MaxLength = 20
    End Sub

    Private Sub TextBox4_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox4.KeyPress
        If Not (Char.IsLetter(e.KeyChar) = True) And e.KeyChar <> ChrW(Keys.Back) And e.KeyChar <> ChrW(Keys.Space) Then
            e.Handled = True
        End If
        TextBox4.MaxLength = 20
    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox1.CheckedChanged
        If CheckBox1.Checked = True Then
            CheckBox2.Checked = False
        Else
            CheckBox2.Checked = True
        End If
    End Sub

    Private Sub CheckBox2_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox2.CheckedChanged
        If CheckBox2.Checked = True Then
            CheckBox1.Checked = False
        Else
            CheckBox1.Checked = True
        End If
    End Sub

    Private Sub CheckBox3_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox3.CheckedChanged
        If CheckBox3.Checked = True Then
            CheckBox4.Checked = False
        Else
            CheckBox4.Checked = True
        End If
    End Sub

    Private Sub CheckBox4_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox4.CheckedChanged
        If CheckBox4.Checked = True Then
            CheckBox3.Checked = False
        Else
            CheckBox3.Checked = True
        End If
    End Sub

    Private Sub CheckBox5_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox5.CheckedChanged
        If CheckBox5.Checked = True Then
            CheckBox6.Checked = False
        Else
            CheckBox6.Checked = True
        End If
    End Sub

    Private Sub CheckBox6_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox6.CheckedChanged
        If CheckBox6.Checked = True Then
            CheckBox5.Checked = False
        Else
            CheckBox5.Checked = True
        End If
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        If TextBox1.Text.Length <> 11 Then
            MsgBox("Lütfen 11 Haneli TC Kimlik Numaranızı Giriniz")
            TextBox1.Focus()
        ElseIf TextBox2.Text.Length <> 10 Then
            MsgBox("Lütfen 10 Haneli Okul Numaranızı Giriniz")
            TextBox2.Focus()
        ElseIf TextBox3.Text = "" Then
            MsgBox("Lütfen Adınızı Giriniz")
            TextBox3.Focus()
        ElseIf TextBox4.Text = "" Then
            MsgBox("Lütfen Soyadınızı Giriniz")
            TextBox4.Focus()
        End If
    End Sub

    Private Sub zorunlu_sil_buton_Click(sender As Object, e As EventArgs) Handles zorunlu_sil_buton.Click
        If zorunlucheckbox.CheckedItems.Count = 0 Then
            MsgBox("Lütfen Silmek İstediğiniz Dersleri İşaretleyiniz")
        Else
            For secili As Integer = zorunlucheckbox.CheckedItems.Count - 1 To 0 Step -1
                zorunlucheckbox.Items.Remove(zorunlucheckbox.CheckedItems(secili))
            Next
        End If
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click

    End Sub

    Private Sub zorunlucheckbox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles zorunlucheckbox.SelectedIndexChanged
        If 
    End Sub

    Private Sub secmeli_sil_buton_Click(sender As Object, e As EventArgs) Handles secmeli_sil_buton.Click
        If secmelicheckbox.CheckedItems.Count = 0 Then
            MsgBox("Lütfen Silmek İstediğiniz Dersleri İşaretleyiniz")
        Else
            For secili As Integer = secmelicheckbox.CheckedItems.Count - 1 To 0 Step -1
                secmelicheckbox.Items.Remove(secmelicheckbox.CheckedItems(secili))
            Next
        End If
        'If CInt(zorunlulabel.Text) <= 22 Then
        '    zorunlulabel.Text = CInt(zorunlulabel.Text) + zorunlu_dizi_akts(zorunlucheckbox.SelectedIndex)
        'End If
    End Sub
End Class
