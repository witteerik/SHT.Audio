<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form1
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.PlayRandomButton = New System.Windows.Forms.Button()
        Me.StopSoundButton = New System.Windows.Forms.Button()
        Me.SuspendLayout()
        '
        'PlayRandomButton
        '
        Me.PlayRandomButton.Location = New System.Drawing.Point(34, 21)
        Me.PlayRandomButton.Name = "PlayRandomButton"
        Me.PlayRandomButton.Size = New System.Drawing.Size(196, 23)
        Me.PlayRandomButton.TabIndex = 0
        Me.PlayRandomButton.Text = "Play sound in random channel"
        Me.PlayRandomButton.UseVisualStyleBackColor = True
        '
        'StopSoundButton
        '
        Me.StopSoundButton.Location = New System.Drawing.Point(34, 60)
        Me.StopSoundButton.Name = "StopSoundButton"
        Me.StopSoundButton.Size = New System.Drawing.Size(196, 23)
        Me.StopSoundButton.TabIndex = 1
        Me.StopSoundButton.Text = "Stop sound"
        Me.StopSoundButton.UseVisualStyleBackColor = True
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(507, 154)
        Me.Controls.Add(Me.StopSoundButton)
        Me.Controls.Add(Me.PlayRandomButton)
        Me.Name = "Form1"
        Me.Text = "Form1"
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents PlayRandomButton As Button
    Friend WithEvents StopSoundButton As Button
End Class
