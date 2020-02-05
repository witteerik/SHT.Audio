'This software is available under the following license:
'MIT/X11 License
'
'Copyright (c) 2020 Erik Witte
'
'Permission is hereby granted, free of charge, to any person obtaining a copy
'of this software and associated documentation files (the ''Software''), to deal
'in the Software without restriction, including without limitation the rights
'to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
'copies of the Software, and to permit persons to whom the Software is
'furnished to do so, subject to the following conditions:
'
'The above copyright notice and this permission notice shall be included in all
'copies or substantial portions of the Software.
'
'THE SOFTWARE IS PROVIDED ''AS IS'', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
'IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
'FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
'AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
'LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
'OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
'SOFTWARE.

Public Class Form1
    Implements SHT.Audio.DuplexPlayer.ISoundPlayerControl

    Private TestSound As SHT.Audio.Sound
    Private Player As SHT.Audio.DuplexPlayer.SoundPlayer
    Private rnd As New Random

    'Dim SpeakerChannels() As Integer = {1, 2, 3, 4, 5, 6, 7, 8, 13, 14, 15, 16}
    Dim SpeakerChannels() As Integer = {1, 2}
    Dim BuffersPlayed As Integer = 0

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Try

            'Loading a test sound from file. 
            Dim ErrorMessage As String = "" 'The ErrorMessage string will contain any reading error messages upon return from LoadWaveFile
            TestSound = SHT.Audio.Sound.LoadWaveFile("[Enter the path to the sound file here]")
            'TestSound = SHT.Audio.Sound.LoadWaveFile("C:\SpeechAndHearingToolsLog\TestFile.wav")


            'Initializing the sound player (with a cross-fade time between subsequently played sounds of 0.05 seconds)
            Player = New SHT.Audio.DuplexPlayer.SoundPlayer(Me,,,,,,,, 0.1)

            'Opening the sound stream
            Player.OpenStream()

            'Starting the sound stream (The sound stream can be kept open even if the sound has reached the end. The reason is that the sound player detects the end of sound, and internally replaces the sound with a silent sound.)
            Player.Start()

        Catch ex As Exception
            MsgBox("An exception occurred. Have you entered the sound file path?" & vbCrLf & vbCrLf & ex.ToString)
        End Try

    End Sub

    Public Sub MessageFromPlayer(ByRef Message As SHT.Audio.DuplexPlayer.ISoundPlayerControl.MessagesFromSoundPlayer) Implements SHT.Audio.DuplexPlayer.ISoundPlayerControl.MessageFromPlayer

        Select Case Message
            Case SHT.Audio.DuplexPlayer.ISoundPlayerControl.MessagesFromSoundPlayer.ApproachingEndOfBufferAlert

            Case SHT.Audio.DuplexPlayer.ISoundPlayerControl.MessagesFromSoundPlayer.EndOfSound

            Case SHT.Audio.DuplexPlayer.ISoundPlayerControl.MessagesFromSoundPlayer.NewBufferTick
                'Threading.Thread.CurrentThread.Join()

                'Dim t As New Threading.WaitCallback(Sub() IncreaseBufferCount())
                'Threading.ThreadPool.QueueUserWorkItem(t)

                IncreaseBufferCount()
            Case Else
                'Just ignores the message
        End Select

    End Sub


    Private Sub IncreaseBufferCount()
        BuffersPlayed += 1
    End Sub


    Private Sub PlayRandomButton_Click(sender As Object, e As EventArgs) Handles PlayRandomButton.Click
        PlaySoundInRandomChannel()
    End Sub

    Private Sub PlaySoundInRandomChannel()

        'Creating a new mixer
        Dim NewMixer = New SHT.Audio.DuplexPlayer.SoundPlayer.DuplexMixer(Player.NumberOfOutputChannels, Player.NumberOfInputChannels)
        NewMixer.OutputRouting.Clear()

        'Setting the mixer to rout channel 1 in the test sound to a random speaker
        Dim RandomSpeakerChannel As Integer = SpeakerChannels(rnd.Next(0, SpeakerChannels.Length))
        NewMixer.OutputRouting.Add(RandomSpeakerChannel, 1)

        'Replacing the existing mixer with the new mixer
        Player.Mixer = NewMixer

        'Sends the test sound to the player to start playing
        Player.SwapOutputSounds(TestSound)

    End Sub

    Private Sub StopSoundButton_Click(sender As Object, e As EventArgs) Handles StopSoundButton.Click
        'Fading out the sound
        Player.Stop(True)
    End Sub

    Private Sub Form1_FormClosing(sender As Object, e As FormClosingEventArgs) Handles MyBase.FormClosing

        'Displosing the sound player
        Player.Stop()
        Player.CloseStream()
        Player.Dispose()
    End Sub
End Class


