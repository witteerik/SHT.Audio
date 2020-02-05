﻿'This software is available under the following license:
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

Imports System.Windows.Forms
Imports System.Runtime.InteropServices
Imports System.Threading
Imports SHT.Audio.DuplexPlayer.PortAudioVB


Public Interface ISoundPlayerControl
    Sub MessageFromPlayer(ByRef Message As MessagesFromSoundPlayer)

    Enum MessagesFromSoundPlayer
        EndOfSound
        ApproachingEndOfBufferAlert
        NewBufferTick
    End Enum

End Interface

Public Class SoundPlayer
    Implements IDisposable

    Private WithEvents MyController As ISoundPlayerControl
    '
    Private CrossFadeProgress As Integer = 0

    Private twopi As Double = 2 * Math.PI 'Or 2 * Math.Acos(-1)

    'Declaration of CALLBACK STUFF
    Private PlaybackBuffer As Single() = New Single(511) {}
    Private RecordingBuffer As Single() = New Single(511) {}
    Private SilentBuffer As Single() = New Single(511) {}
    Private paStreamCallback As PortAudio.PaStreamCallbackDelegate = Function(input As IntPtr, output As IntPtr, frameCount As UInteger, ByRef timeInfo As PortAudio.PaStreamCallbackTimeInfo, statusFlags As PortAudio.PaStreamCallbackFlags, userData As IntPtr) As PortAudio.PaStreamCallbackResult


                                                                         'Sending a buffer tick to the controller
                                                                         If IsBufferTickActive = True Then
                                                                             SendMessageToController(ISoundPlayerControl.MessagesFromSoundPlayer.NewBufferTick)
                                                                         End If

                                                                         Try

                                                                             'INPUT SOUND

                                                                             If RecordingIsActive = True Then
                                                                                 'Getting input sound

                                                                                 'This need to be changed for real time recording, the audio need to be converted to Audio.Sound format, or maybe it could be fixed after stoppong the recording?
                                                                                 Dim InputBuffer(RecordingBuffer.Length - 1) As Single
                                                                                 Marshal.Copy(input, InputBuffer, 0, AudioApiSettings.FramesPerBuffer * RecordingSoundFormat.Channels)
                                                                                 InputBufferHistory.Add(InputBuffer)
                                                                             End If


                                                                             'OUTPUT SOUND
                                                                             If PlaybackIsActive = True Then

                                                                                 'Checking if the current sound should be swapped (if there is a new sound in NewSound)
                                                                                 If NewSound IsNot Nothing Then

                                                                                     'Swapping sound
                                                                                     Select Case CurrentOutputSound
                                                                                         Case OutputSounds.OutputSoundA, OutputSounds.FadingToA
                                                                                             OutputSoundB = NewSound
                                                                                             NewSound = Nothing
                                                                                             CurrentOutputSound = OutputSounds.FadingToB

                                                                                             'Setting reading position
                                                                                             PositionB = 0

                                                                                         Case OutputSounds.OutputSoundB, OutputSounds.FadingToB
                                                                                             OutputSoundA = NewSound
                                                                                             NewSound = Nothing
                                                                                             CurrentOutputSound = OutputSounds.FadingToA

                                                                                             'Setting reading position
                                                                                             PositionA = 0
                                                                                     End Select

                                                                                     'Setting CrossFadeProgress to 0 since a new fade period have begun
                                                                                     CrossFadeProgress = 0

                                                                                 End If

                                                                                 'Checking current positions to see if an EndOfBufferAlert should be sent
                                                                                 Select Case CurrentOutputSound
                                                                                     Case OutputSounds.OutputSoundA, OutputSounds.FadingToA

                                                                                         'Copying the silent buffer if the end of sound A is reached
                                                                                         If PositionA = OutputSoundA.Length - ApproachingEndOfBufferAlert_BufferCount Then
                                                                                             SendMessageToController(ISoundPlayerControl.MessagesFromSoundPlayer.ApproachingEndOfBufferAlert)
                                                                                         End If

                                                                                     Case OutputSounds.OutputSoundB, OutputSounds.FadingToB

                                                                                         'Copying the silent buffer if the end of sound A is reached
                                                                                         If PositionB = OutputSoundB.Length - ApproachingEndOfBufferAlert_BufferCount Then
                                                                                             SendMessageToController(ISoundPlayerControl.MessagesFromSoundPlayer.ApproachingEndOfBufferAlert)
                                                                                         End If

                                                                                     Case Else
                                                                                         Throw New NotImplementedException
                                                                                 End Select


                                                                                 'Copying buffers 
                                                                                 Select Case CurrentOutputSound
                                                                                     Case OutputSounds.OutputSoundA

                                                                                         'Copying the silent buffer if the end of sound A is reached
                                                                                         If PositionA >= OutputSoundA.Length Then
                                                                                             Marshal.Copy(SilentBuffer, 0, output, SilentBuffer.Length)

                                                                                             'Sending message to the controller
                                                                                             SendMessageToController(ISoundPlayerControl.MessagesFromSoundPlayer.EndOfSound)

                                                                                             If StopAtOutputSoundEnd = True Then

                                                                                                 'Returning the callback to port audio
                                                                                                 Return PortAudio.PaStreamCallbackResult.paComplete
                                                                                             Else
                                                                                                 Return PortAudio.PaStreamCallbackResult.paContinue
                                                                                             End If
                                                                                         End If

                                                                                         PlaybackBuffer = OutputSoundA(PositionA).InterleavedSampleArray

                                                                                         'Copying the playback buffer to unmanaged memory
                                                                                         Marshal.Copy(PlaybackBuffer, 0, output, AudioApiSettings.FramesPerBuffer * NumberOfOutputChannels) 'PlaybackBuffer.length?

                                                                                         PositionA += 1

                                                                                     Case OutputSounds.OutputSoundB

                                                                                         'Copying the silent buffer if the end of sound B is reached
                                                                                         If PositionB >= OutputSoundB.Length Then
                                                                                             Marshal.Copy(SilentBuffer, 0, output, SilentBuffer.Length)

                                                                                             'Sending message to the controller
                                                                                             SendMessageToController(ISoundPlayerControl.MessagesFromSoundPlayer.EndOfSound)

                                                                                             If StopAtOutputSoundEnd = True Then

                                                                                                 'Returning the callback to port audio
                                                                                                 Return PortAudio.PaStreamCallbackResult.paComplete
                                                                                             Else
                                                                                                 Return PortAudio.PaStreamCallbackResult.paContinue
                                                                                             End If
                                                                                         End If

                                                                                         PlaybackBuffer = OutputSoundB(PositionB).InterleavedSampleArray

                                                                                         'Copying the playback buffer to unmanaged memory
                                                                                         Marshal.Copy(PlaybackBuffer, 0, output, AudioApiSettings.FramesPerBuffer * NumberOfOutputChannels) 'PlaybackBuffer.length?

                                                                                         PositionB += 1

                                                                                     Case OutputSounds.FadingToA

                                                                                         If PositionA < OutputSoundA.Length And PositionB < OutputSoundB.Length Then

                                                                                             'Mixing sound A and B to the buffer
                                                                                             For j As Integer = 0 To PlaybackBuffer.Length - 1
                                                                                                 PlaybackBuffer(j) = OutputSoundB(PositionB).InterleavedSampleArray(j) * OverlapFadeOutArray(CrossFadeProgress) + OutputSoundA(PositionA).InterleavedSampleArray(j) * OverlapFadeInArray(CrossFadeProgress)
                                                                                                 CrossFadeProgress += 1
                                                                                             Next

                                                                                             'Copying the playback buffer to unmanaged memory
                                                                                             Marshal.Copy(PlaybackBuffer, 0, output, AudioApiSettings.FramesPerBuffer * NumberOfOutputChannels) 'PlaybackBuffer.length?

                                                                                         ElseIf PositionA < OutputSoundA.Length And PositionB >= OutputSoundB.Length Then

                                                                                             'Copying only sound A to the buffer
                                                                                             For j As Integer = 0 To PlaybackBuffer.Length - 1
                                                                                                 PlaybackBuffer(j) = OutputSoundA(PositionA).InterleavedSampleArray(j) * OverlapFadeInArray(CrossFadeProgress)
                                                                                                 CrossFadeProgress += 1
                                                                                             Next

                                                                                             'Copying the playback buffer to unmanaged memory
                                                                                             Marshal.Copy(PlaybackBuffer, 0, output, AudioApiSettings.FramesPerBuffer * NumberOfOutputChannels) 'PlaybackBuffer.length?

                                                                                         ElseIf PositionA >= OutputSoundA.Length And PositionB < OutputSoundB.Length Then

                                                                                             'Copying only sound B to the buffer
                                                                                             For j As Integer = 0 To PlaybackBuffer.Length - 1
                                                                                                 PlaybackBuffer(j) = OutputSoundB(PositionB).InterleavedSampleArray(j) * OverlapFadeOutArray(CrossFadeProgress)
                                                                                                 CrossFadeProgress += 1
                                                                                             Next

                                                                                             'Copying the playback buffer to unmanaged memory
                                                                                             Marshal.Copy(PlaybackBuffer, 0, output, AudioApiSettings.FramesPerBuffer * NumberOfOutputChannels) 'PlaybackBuffer.length?

                                                                                         Else
                                                                                             'End of both sounds: Copying silence
                                                                                             CrossFadeProgress = OverlapFadeLength
                                                                                             Marshal.Copy(SilentBuffer, 0, output, SilentBuffer.Length)

                                                                                             'Sending message to the controller
                                                                                             SendMessageToController(ISoundPlayerControl.MessagesFromSoundPlayer.EndOfSound)

                                                                                             If StopAtOutputSoundEnd = True Then

                                                                                                 'Returning the callback to port audio
                                                                                                 Return PortAudio.PaStreamCallbackResult.paComplete
                                                                                             End If
                                                                                         End If

                                                                                         PositionA += 1
                                                                                         PositionB += 1

                                                                                         'Changing to OutputSounds.OutputSoundA and Resetting the CrossFadeProgress, if fading is completed
                                                                                         If CrossFadeProgress >= OverlapFadeLength - 1 Then
                                                                                             CurrentOutputSound = OutputSounds.OutputSoundA
                                                                                             CrossFadeProgress = 0
                                                                                         End If

                                                                                     Case OutputSounds.FadingToB

                                                                                         If PositionA < OutputSoundA.Length And PositionB < OutputSoundB.Length Then

                                                                                             'Mixing sound A and B to the buffer
                                                                                             For j As Integer = 0 To PlaybackBuffer.Length - 1
                                                                                                 PlaybackBuffer(j) = OutputSoundB(PositionB).InterleavedSampleArray(j) * OverlapFadeInArray(CrossFadeProgress) + OutputSoundA(PositionA).InterleavedSampleArray(j) * OverlapFadeOutArray(CrossFadeProgress)
                                                                                                 CrossFadeProgress += 1
                                                                                             Next

                                                                                             'Copying the playback buffer to unmanaged memory
                                                                                             Marshal.Copy(PlaybackBuffer, 0, output, AudioApiSettings.FramesPerBuffer * NumberOfOutputChannels) 'PlaybackBuffer.length?

                                                                                         ElseIf PositionA < OutputSoundA.Length And PositionB >= OutputSoundB.Length Then

                                                                                             'Copying only sound A to the buffer
                                                                                             For j As Integer = 0 To PlaybackBuffer.Length - 1
                                                                                                 PlaybackBuffer(j) = OutputSoundA(PositionA).InterleavedSampleArray(j) * OverlapFadeOutArray(CrossFadeProgress)
                                                                                                 CrossFadeProgress += 1
                                                                                             Next

                                                                                             'Copying the playback buffer to unmanaged memory
                                                                                             Marshal.Copy(PlaybackBuffer, 0, output, AudioApiSettings.FramesPerBuffer * NumberOfOutputChannels) 'PlaybackBuffer.length?

                                                                                         ElseIf PositionA >= OutputSoundA.Length And PositionB < OutputSoundB.Length Then

                                                                                             'Copying only sound B to the buffer
                                                                                             For j As Integer = 0 To PlaybackBuffer.Length - 1
                                                                                                 PlaybackBuffer(j) = OutputSoundB(PositionB).InterleavedSampleArray(j) * OverlapFadeInArray(CrossFadeProgress)
                                                                                                 CrossFadeProgress += 1
                                                                                             Next

                                                                                             'Copying the playback buffer to unmanaged memory
                                                                                             Marshal.Copy(PlaybackBuffer, 0, output, AudioApiSettings.FramesPerBuffer * NumberOfOutputChannels) 'PlaybackBuffer.length?

                                                                                         Else
                                                                                             'End of both sounds: Copying silence
                                                                                             CrossFadeProgress = OverlapFadeLength
                                                                                             Marshal.Copy(SilentBuffer, 0, output, SilentBuffer.Length)

                                                                                             'Sending message to the controller
                                                                                             SendMessageToController(ISoundPlayerControl.MessagesFromSoundPlayer.EndOfSound)

                                                                                             If StopAtOutputSoundEnd = True Then

                                                                                                 'Returning the callback to port audio
                                                                                                 Return PortAudio.PaStreamCallbackResult.paComplete
                                                                                             End If
                                                                                         End If

                                                                                         PositionA += 1
                                                                                         PositionB += 1

                                                                                         'Changing to OutputSounds.OutputSoundA and Resetting the CrossFadeProgress, if fading is completed
                                                                                         If CrossFadeProgress >= OverlapFadeLength - 1 Then
                                                                                             CurrentOutputSound = OutputSounds.OutputSoundB
                                                                                             CrossFadeProgress = 0
                                                                                         End If

                                                                                 End Select
                                                                             End If

                                                                             Return PortAudio.PaStreamCallbackResult.paContinue

                                                                         Catch ex As Exception

                                                                             SendInfoToAudioLog(CurrentOutputSound.ToString & " " & PositionA & " " & PositionB & " " & CrossFadeProgress & vbCrLf &
                                                                                               ex.ToString, "ExceptionsDuringPlayback")

                                                                             'Returning silence if an exception occurred
                                                                             Marshal.Copy(SilentBuffer, 0, output, AudioApiSettings.FramesPerBuffer * NumberOfOutputChannels)
                                                                             Return PortAudio.PaStreamCallbackResult.paContinue

                                                                         End Try

                                                                     End Function


    'OTHER DECLATATIONS
    Public ReadOnly SoundDirection As SoundDirections
    Public ReadOnly NumberOfOutputChannels As Integer
    Public ReadOnly NumberOfInputChannels As Integer
    Public ReadOnly AudioEncoding As Audio.Formats.WaveFormat.WaveFormatEncodings
    Public ReadOnly AudioBitDepth As Integer
    Private ReadOnly PaSampleFormat As PortAudio.PaSampleFormat

    Public Function GetSampleRate() As Integer
        Return AudioApiSettings.SampleRate
    End Function

    Public PositionA As Integer
    Public PositionB As Integer

    Private _IsInitialized As Boolean = False
    Public ReadOnly Property IsInitialized As Boolean
        Get
            Return _IsInitialized
        End Get
    End Property

    Private OutputSoundA As BufferHolder()
    Private OutputSoundB As BufferHolder()
    Private NewSound As BufferHolder()
    Private SilentSound As BufferHolder()

    Public Class BufferHolder
        Public InterleavedSampleArray As Single()
        Public ChannelDataList As List(Of Single())
        Public ChannelCount As Integer
        Public FrameCount As Integer

        ''' <summary>
        ''' Holds the (0-based) index of the first sample in the current BufferHolder
        ''' </summary>
        Public StartSample As Integer
        ''' <summary>
        ''' Holds the start time (in seconds) of the first sample in the current BufferHolder
        ''' </summary>
        Public StartTime As Single

        Public Sub New(ByVal ChannelCount As Integer, ByVal FrameCount As Integer)
            Me.ChannelCount = ChannelCount
            Me.FrameCount = FrameCount
            Dim NewInterleavedBuffer(ChannelCount * FrameCount - 1) As Single
            InterleavedSampleArray = NewInterleavedBuffer
        End Sub

        Public Sub New(ByVal ChannelCount As Integer, ByVal FrameCount As Integer, ByRef InterleavedSampleArray As Single())
            Me.ChannelCount = ChannelCount
            Me.FrameCount = FrameCount
            Me.InterleavedSampleArray = InterleavedSampleArray
        End Sub

        Public Sub ConvertToChannelData(ByRef DuplexMixer As DuplexMixer)
            Throw New NotImplementedException
        End Sub

    End Class

    Public Property Mixer As DuplexMixer
    Public Class DuplexMixer
        ''' <summary>
        ''' A list of key-value pairs, where the key repressents the hardware output channel and the value repressents the wave file channel from which the output sound should be drawn.
        ''' </summary>
        Public OutputRouting As New SortedList(Of Integer, Integer)
        ''' <summary>
        ''' A list of key-value pairs, where the key repressents the hardware input channel and the value repressents the wave file channel in which the input sound should be stored.
        ''' </summary>
        Public InputRouting As New SortedList(Of Integer, Integer)

        Public ReadOnly AvailableOutputChannels As Integer
        Public ReadOnly AvailableInputChannels As Integer

        ''' <summary>
        ''' Creating a new mixer.
        ''' </summary>
        ''' <param name="AvailableOutputChannels"></param>
        ''' <param name="AvailableInputChannels"></param>
        Public Sub New(ByVal AvailableOutputChannels As Integer, ByVal AvailableInputChannels As Integer)

            Me.AvailableOutputChannels = AvailableOutputChannels
            Me.AvailableInputChannels = AvailableInputChannels

            For c = 1 To AvailableOutputChannels
                OutputRouting.Add(c, 0)
            Next

            For c = 1 To AvailableInputChannels
                InputRouting.Add(c, 0)
            Next

        End Sub

        Public Sub DirectMonoSoundToOutputChannel(ByRef TargetOutputChannel As Integer)
            If OutputRouting.ContainsKey(TargetOutputChannel) Then OutputRouting(TargetOutputChannel) = 1
        End Sub

        Public Sub DirectMonoSoundToOutputChannels(ByRef TargetOutputChannels() As Integer)
            For Each OutputChannel In TargetOutputChannels
                If OutputRouting.ContainsKey(OutputChannel) Then OutputRouting(OutputChannel) = 1
            Next
        End Sub

    End Class

    Public OverlappingSounds As Boolean = False
    Public EqualPowerCrossFade As Boolean = True
    Public OverlappingFadeType As FadeTypes = FadeTypes.Linear
    Public Enum FadeTypes
        Linear
        Smooth
    End Enum

    Private CurrentOutputSound As OutputSounds = OutputSounds.OutputSoundA
    Private Enum OutputSounds
        OutputSoundA
        OutputSoundB
        FadingToB
        FadingToA
    End Enum

    Private Sub SetOverlapDuration(ByVal Duration As Single)
        OverlapFadeLength = NumberOfOutputChannels * AudioApiSettings.SampleRate * Duration
    End Sub

    Public Function GetOverlapDuration() As Single
        Return (_OverlapFadeLength / NumberOfOutputChannels) / AudioApiSettings.SampleRate
    End Function


    Private _OverlapFadeLength As Double
    ''' <summary>
    ''' A value that holds the number of overlapping samples between two sounds. Setting this value automatically creates overlap fade arrays (OverlapFadeInArray and OverlapFadeOutArray). 
    ''' </summary>
    ''' <returns></returns>
    Private Property OverlapFadeLength As Double
        Get
            Return _OverlapFadeLength
        End Get
        Set(value As Double)
            Try

                _OverlapFadeLength = Int(value / (NumberOfOutputChannels * AudioApiSettings.FramesPerBuffer)) * (NumberOfOutputChannels * AudioApiSettings.FramesPerBuffer)

                Dim OverLapFrameCount As Integer = _OverlapFadeLength / NumberOfOutputChannels

                Select Case OverlappingFadeType
                    Case FadeTypes.Linear
                        'Linear fading
                        'fade in array
                        ReDim OverlapFadeInArray(_OverlapFadeLength - 1)
                        For n = 0 To OverLapFrameCount - 1
                            For c = 0 To NumberOfOutputChannels - 1
                                OverlapFadeInArray(n * NumberOfOutputChannels + c) = n / (OverLapFrameCount - 1)
                            Next
                        Next

                        'fade out array
                        ReDim OverlapFadeOutArray(_OverlapFadeLength - 1)
                        For n = 0 To OverLapFrameCount - 1
                            For c = 0 To NumberOfOutputChannels - 1
                                OverlapFadeOutArray(n * NumberOfOutputChannels + c) = 1 - (n / (OverLapFrameCount - 1))
                            Next
                        Next

                    Case FadeTypes.Smooth

                        'Smooth fading
                        'fade in array
                        ReDim OverlapFadeInArray(_OverlapFadeLength - 1)

                        'fade out array
                        ReDim OverlapFadeOutArray(_OverlapFadeLength - 1)

                        Dim FadeProgress As Single = 0
                        Dim currentModFactor As Single
                        Dim StartFactor As Single = 0
                        Dim endFactor As Single = 1
                        For n = 0 To _OverlapFadeLength - 1
                            'fadeProgress goes from 0 to 1 during the fade section
                            FadeProgress = n / (_OverlapFadeLength - 1)

                            'Modifies currentFadeFactor according to a cosine finction, whereby currentModFactor starts on 1 and end at 0
                            currentModFactor = ((Math.Cos(twopi * (FadeProgress / 2)) + 1) / 2)
                            OverlapFadeInArray(n) = StartFactor * currentModFactor + endFactor * (1 - currentModFactor)

                            'Setting the fade out array values to 1-(fade in array values) to create an exact inverse, which allways adds up to 1 during fading.
                            OverlapFadeOutArray(n) = 1 - OverlapFadeInArray(n)
                        Next

                End Select

                'Adjusting to equal power fades
                If EqualPowerCrossFade = True Then
                    For n = 0 To OverlapFadeInArray.Length - 1
                        OverlapFadeInArray(n) = Math.Sqrt(OverlapFadeInArray(n))
                    Next
                    For n = 0 To OverlapFadeOutArray.Length - 1
                        OverlapFadeOutArray(n) = Math.Sqrt(OverlapFadeOutArray(n))
                    Next
                End If

            Catch ex As Exception
                MsgBox(ex.ToString)
            End Try

        End Set
    End Property
    Private OverlapFadeInArray As Single()
    Private OverlapFadeOutArray As Single()


    Private InputBufferHistory As New List(Of Single())

    Property _RecordingSoundFormat As Audio.Formats.WaveFormat
    Property RecordingSoundFormat As Audio.Formats.WaveFormat
        Private Set(value As Audio.Formats.WaveFormat)
            _RecordingSoundFormat = value
        End Set
        Get
            Return _RecordingSoundFormat
        End Get
    End Property

    Public StopAtOutputSoundEnd As Boolean
    'Public EndOfOutputSound_A_IsReached As Boolean = False
    'Public EndOfOutputSound_B_IsReached As Boolean = False

    Private _AudioApiSettings As AudioApiSettings
    Property AudioApiSettings As AudioApiSettings
        Private Set(value As AudioApiSettings)
            _AudioApiSettings = value
        End Set
        Get
            Return _AudioApiSettings
        End Get
    End Property

    Private stream As IntPtr
    Private disposed As Boolean = False

    Private Shared m_messagesEnabled As Boolean = False
    Public Shared Property MessagesEnabled() As Boolean
        Get
            Return m_messagesEnabled
        End Get
        Set
            m_messagesEnabled = Value
        End Set
    End Property

    Private Shared m_loggingEnabled As Boolean = False
    Public Shared Property LoggingEnabled() As Boolean
        Get
            Return m_loggingEnabled
        End Get
        Set
            m_loggingEnabled = Value
        End Set
    End Property

    Private _IsPlaying As Boolean = False
    Public ReadOnly Property IsPlaying As Boolean
        Get
            Return _IsPlaying
        End Get
    End Property

    Private _IsStreamOpen As Boolean = False
    Public ReadOnly Property IsStreamOpen As Boolean
        Get
            Return _IsStreamOpen
        End Get
    End Property

    Private RecordingIsActive As Boolean
    Private PlaybackIsActive As Boolean

    Private _IsClippingInactivated As Boolean = False
    Public ReadOnly Property IsClippingInactivated As Boolean
        Get
            Return _IsClippingInactivated
        End Get
    End Property

    Private CloseStreamAfterPlayCompletion As Boolean = False
    Private ApproachingEndOfBufferAlert_BufferCount As Integer
    Private IsBufferTickActive As Boolean

    Public Enum SoundDirections
        PlaybackOnly
        RecordingOnly
        Duplex
    End Enum

    Public Sub New(ByRef SoundPlayerController As ISoundPlayerControl,
                  Optional SoundDirection As SoundDirections = SoundDirections.PlaybackOnly,
                   Optional ByRef AudioApiSettings As PortAudioVB.AudioApiSettings = Nothing,
                   Optional ByVal AudioEncoding As Formats.WaveFormat.WaveFormatEncodings = Formats.WaveFormat.WaveFormatEncodings.IeeeFloatingPoints,
                   Optional ByVal LoggingEnabled As Boolean = False,
                   Optional ByVal MessagesEnabled As Boolean = False,
                   Optional StopAtOutputSoundEnd As Boolean = False,
                   Optional InactivateClipping As Boolean = False,
                   Optional OverlapDuration As Double = 1,
                   Optional ByVal ApproachingEndOfBufferAlert_BufferCount As Integer = 1,
                   Optional ByVal ActivateBufferTicks As Boolean = False)

        Me.IsBufferTickActive = ActivateBufferTicks
        Me.MyController = SoundPlayerController
        Me.ApproachingEndOfBufferAlert_BufferCount = ApproachingEndOfBufferAlert_BufferCount

        Try
            Me.SoundDirection = SoundDirection
            Select Case SoundDirection
                Case SoundDirections.PlaybackOnly
                    PlaybackIsActive = True
                    RecordingIsActive = False
                Case SoundDirections.RecordingOnly
                    PlaybackIsActive = False
                    RecordingIsActive = True
                Case SoundDirections.Duplex
                    PlaybackIsActive = True
                    RecordingIsActive = True
                Case Else
                    Throw New Exception("Invalid sound direction")
            End Select

            Me.AudioEncoding = AudioEncoding
            Select Case Me.AudioEncoding 'Bit depth is here assumed from encoding...
                Case Formats.WaveFormat.WaveFormatEncodings.PCM
                    AudioBitDepth = 16
                    PaSampleFormat = PortAudio.PaSampleFormat.paInt16
                Case Formats.WaveFormat.WaveFormatEncodings.IeeeFloatingPoints
                    AudioBitDepth = 32
                    PaSampleFormat = PortAudio.PaSampleFormat.paFloat32
                Case Else
                    Throw New Exception("Unsuppported audio encoding.")
            End Select

            'Setting clipping
            Me._IsClippingInactivated = InactivateClipping

            'Overriding any value set in InitializationSuccess
            _IsInitialized = False

            SoundPlayer.LoggingEnabled = LoggingEnabled
            SoundPlayer.MessagesEnabled = MessagesEnabled
            Log("Initializing...")


            'Initializing PA
            If ErrorCheck("Initialize", PortAudio.Pa_Initialize(), True) = True Then
                Me.disposed = True
                ' if Pa_Initialize() returns an error code, 
                ' Pa_Terminate() should NOT be called.
                Throw New Exception("Can't initialize audio")
            End If

            'Setting API settings if not already done
            If AudioApiSettings Is Nothing Then
                'Dim FixedSampleRate As Integer? = Nothing
                Dim newAudioSettingsDialog As New AudioSettingsDialog()
                Dim DialogResult = newAudioSettingsDialog.ShowDialog()
                If DialogResult = DialogResult.OK Then
                    AudioApiSettings = newAudioSettingsDialog.CurrentAudioApiSettings
                Else
                    MsgBox("Did not initialize PaSoundPlayer due to missing audio settings.")
                    Throw New Exception("Did not initialize PaSoundPlayer due to missing audio settings.")
                    Log(ErrorCheck("Terminate", PortAudio.Pa_Terminate(), True))
                    Exit Sub
                End If
            End If

            'Setting Me.audioApiSettings
            Me.AudioApiSettings = AudioApiSettings

            'Storing the number of input and output channels
            If Not Me.AudioApiSettings.SelectedInputDeviceInfo Is Nothing Then
                NumberOfInputChannels = Me.AudioApiSettings.SelectedInputDeviceInfo.Value.maxInputChannels
            End If
            If Not Me.AudioApiSettings.SelectedOutputDeviceInfo Is Nothing Then
                NumberOfOutputChannels = Me.AudioApiSettings.SelectedOutputDeviceInfo.Value.maxOutputChannels
            End If
            If Not Me.AudioApiSettings.SelectedInputAndOutputDeviceInfo Is Nothing Then
                NumberOfInputChannels = Me.AudioApiSettings.SelectedInputAndOutputDeviceInfo.Value.maxInputChannels
                NumberOfOutputChannels = Me.AudioApiSettings.SelectedInputAndOutputDeviceInfo.Value.maxOutputChannels
            End If

            Log("Selected HostAPI:" & vbLf & Me.AudioApiSettings.SelectedApiInfo.ToString())
            If Not Me.AudioApiSettings.SelectedInputDeviceInfo Is Nothing Then Log("Selected input device:" & vbLf & Me.AudioApiSettings.SelectedInputDeviceInfo.ToString())
            If Not Me.AudioApiSettings.SelectedOutputDeviceInfo Is Nothing Then Log("Selected output device:" & vbLf & Me.AudioApiSettings.SelectedOutputDeviceInfo.ToString())
            If Not Me.AudioApiSettings.SelectedInputAndOutputDeviceInfo Is Nothing Then Log("Selected input and output device:" & vbLf & Me.AudioApiSettings.SelectedInputAndOutputDeviceInfo.ToString())

            'Setting OverlapFadeLength (and creating fade arrays)
            SetOverlapDuration(OverlapDuration)

            'Creating a default mixer if none is supplied
            If Mixer Is Nothing Then
                Me.Mixer = New DuplexMixer(NumberOfOutputChannels, NumberOfInputChannels)
                'Me.Mixer.DirectMonoSoundToOutputChannel(1)
                Me.Mixer.DirectMonoSoundToOutputChannels({1, 2})

            Else
                Me.Mixer = Mixer
            End If

            _IsInitialized = True

        Catch e As Exception
            Log(ErrorCheck("Terminate", PortAudio.Pa_Terminate(), True))
            Log(e.ToString())
        End Try
    End Sub


    ''' <summary>
    ''' Swaps the current output sound to a new, using crossfading between ths sounds.
    ''' </summary>
    ''' <param name="NewOutputSound"></param>
    ''' <returns>Returns True if successful, or False if unsuccessful.</returns>
    Public Function SwapOutputSounds(ByRef NewOutputSound As SHT.Audio.Sound) As Boolean

        'Calling [Stop] with fade out to if the new output sound is Nothing
        If NewOutputSound Is Nothing Then
            [Stop](True)
            Return False
        End If

        'Checking that the new sound is at least 1 sample long
        If NewOutputSound.WaveData.ShortestChannelSampleCount = 0 Then
            Log("Error: New sound is contains no sample data (SwapOutputSounds).")
        End If

        'Checks that sound is playing
        If _IsPlaying = False Then
            Log("Error: SwapOutputSounds is only effective during active playback.")
            Return False
        End If

        'Checking that the format is the same format, and returns False if not
        If NewOutputSound.WaveFormat.SampleRate <> AudioApiSettings.SampleRate Or
                        NewOutputSound.WaveFormat.BitDepth <> AudioBitDepth Or
                        NewOutputSound.WaveFormat.Encoding <> AudioEncoding Then
            Log("Error: Different formats in SwapOutputSounds.")
            Return False
        End If


        'Setting NewSound to the NewOutputSound to indicate that the output sound should be swapped by the callback
        'NewSound = CreateBufferHolders(NewOutputSound)

        NewSound = CreateBufferHoldersOnNewThread(NewOutputSound)


        Return True

    End Function


    Public Function CreateBufferHolders(ByRef InputSound As Audio.Sound) As BufferHolder()

        Dim BufferCount As Integer = Int(InputSound.WaveData.LongestChannelSampleCount / AudioApiSettings.FramesPerBuffer) + 1

        Dim Output(BufferCount - 1) As BufferHolder

        'Initializing the BufferHolders
        For b = 0 To Output.Length - 1
            Output(b) = New BufferHolder(NumberOfOutputChannels, AudioApiSettings.FramesPerBuffer)
        Next

        Dim CurrentChannelInterleavedPosition As Integer
        For Each OutputChannel In Mixer.OutputRouting

            If OutputChannel.Value = 0 Then Continue For

            If OutputChannel.Value > InputSound.WaveFormat.Channels Then Continue For

            CurrentChannelInterleavedPosition = OutputChannel.Key - 1

            'Reading samples
            For BufferIndex = 0 To Output.Length - 2
                Dim CurrentWriteSampleIndex As Integer = 0
                For Sample = BufferIndex * AudioApiSettings.FramesPerBuffer To (BufferIndex + 1) * AudioApiSettings.FramesPerBuffer - 1

                    Output(BufferIndex).InterleavedSampleArray(CurrentWriteSampleIndex * NumberOfOutputChannels + CurrentChannelInterleavedPosition) = InputSound.WaveData.SampleData(OutputChannel.Value)(Sample)
                    CurrentWriteSampleIndex += 1
                Next
            Next

            'Reading the last bit
            Dim CurrentWriteSampleIndexB As Integer = 0
            For Sample = AudioApiSettings.FramesPerBuffer * Output.Length - 1 To InputSound.WaveData.SampleData(OutputChannel.Value).Length - 1

                Output(Output.Length - 1).InterleavedSampleArray(CurrentWriteSampleIndexB * NumberOfOutputChannels + CurrentChannelInterleavedPosition) = InputSound.WaveData.SampleData(OutputChannel.Value)(Sample)
                CurrentWriteSampleIndexB += 1
            Next
        Next

        Return Output

    End Function


    Public Function CreateBufferHoldersOnNewThread(ByRef InputSound As Audio.Sound, Optional ByVal BuffersOnMainThread As Integer = 10) As BufferHolder()

        Dim BufferCount As Integer = Int(InputSound.WaveData.LongestChannelSampleCount / AudioApiSettings.FramesPerBuffer) + 1

        Dim Output(BufferCount - 1) As BufferHolder

        'Initializing the BufferHolders
        For b = 0 To Output.Length - 1
            Output(b) = New BufferHolder(NumberOfOutputChannels, AudioApiSettings.FramesPerBuffer)
        Next

        'Creating the BuffersOnMainThread first buffers
        'Limiting the number of main thread buffers if the sound is very short
        If BuffersOnMainThread < Output.Length - 2 Then
            BuffersOnMainThread = Math.Max(0, Output.Length - 1)
        End If


        Dim CurrentChannelInterleavedPosition As Integer
        For Each OutputChannel In Mixer.OutputRouting

            If OutputChannel.Value = 0 Then Continue For

            If OutputChannel.Value > InputSound.WaveFormat.Channels Then Continue For

            CurrentChannelInterleavedPosition = OutputChannel.Key - 1

            'Going through buffer by buffer
            For BufferIndex = 0 To BuffersOnMainThread - 1

                'Setting start sample and time
                Output(BufferIndex).StartSample = BufferIndex * AudioApiSettings.FramesPerBuffer
                Output(BufferIndex).StartTime = BufferIndex * AudioApiSettings.FramesPerBuffer / AudioApiSettings.SampleRate

                'Shuffling samples from the input sound to the interleaved array
                Dim CurrentWriteSampleIndex As Integer = 0
                For Sample = BufferIndex * AudioApiSettings.FramesPerBuffer To (BufferIndex + 1) * AudioApiSettings.FramesPerBuffer - 1

                    Output(BufferIndex).InterleavedSampleArray(CurrentWriteSampleIndex * NumberOfOutputChannels + CurrentChannelInterleavedPosition) = InputSound.WaveData.SampleData(OutputChannel.Value)(Sample)
                    CurrentWriteSampleIndex += 1
                Next
            Next
        Next

        'Fixes the rest of the buffers on a new thread, allowing the new sound to start playing
        Dim ThreadWork As New BufferCreaterOnNewThread(InputSound, Output, BuffersOnMainThread,
                                                               NumberOfOutputChannels, Mixer, AudioApiSettings)

        Return Output

    End Function

    Private Class BufferCreaterOnNewThread
        Implements IDisposable

        Dim InputSound As Audio.Sound
        Dim Output As BufferHolder()
        Dim BuffersOnMainThread As Integer
        Dim NumberOfOutputChannels As Integer
        Dim Mixer As DuplexMixer
        Dim AudioApiSettings As AudioApiSettings

        Public Sub New(ByRef InputSound As Audio.Sound, ByRef Output As BufferHolder(), ByVal BuffersOnMainThread As Integer,
                         ByVal NumberOfOutputChannels As Integer, ByRef Mixer As DuplexMixer, ByRef AudioApiSettings As AudioApiSettings)
            Me.InputSound = InputSound
            Me.Output = Output
            Me.BuffersOnMainThread = BuffersOnMainThread
            Me.NumberOfOutputChannels = NumberOfOutputChannels
            Me.Mixer = Mixer
            Me.AudioApiSettings = AudioApiSettings

            'Starting the new worker thread
            Dim NewThred As New Thread(AddressOf DoWork)
            NewThred.Start()

        End Sub

        Private Sub DoWork()

            Dim CurrentChannelInterleavedPosition As Integer
            For Each OutputChannel In Mixer.OutputRouting

                If OutputChannel.Value = 0 Then Continue For

                If OutputChannel.Value > InputSound.WaveFormat.Channels Then Continue For

                CurrentChannelInterleavedPosition = OutputChannel.Key - 1

                'Going through buffer by buffer
                For BufferIndex = BuffersOnMainThread To Output.Length - 2

                    'Setting start sample and time
                    Output(BufferIndex).StartSample = BufferIndex * AudioApiSettings.FramesPerBuffer
                    Output(BufferIndex).StartTime = BufferIndex * AudioApiSettings.FramesPerBuffer / AudioApiSettings.SampleRate

                    'Shuffling samples from the input sound to the interleaved array
                    Dim CurrentWriteSampleIndex As Integer = 0
                    For Sample = BufferIndex * AudioApiSettings.FramesPerBuffer To (BufferIndex + 1) * AudioApiSettings.FramesPerBuffer - 1

                        Output(BufferIndex).InterleavedSampleArray(CurrentWriteSampleIndex * NumberOfOutputChannels + CurrentChannelInterleavedPosition) = InputSound.WaveData.SampleData(OutputChannel.Value)(Sample)
                        CurrentWriteSampleIndex += 1
                    Next
                Next

                'Reading the last bit
                'Setting start sample and time
                Output(Output.Length - 1).StartSample = (Output.Length - 1) * AudioApiSettings.FramesPerBuffer
                Output(Output.Length - 1).StartTime = (Output.Length - 1) * AudioApiSettings.FramesPerBuffer / AudioApiSettings.SampleRate

                'Shuffling samples from the input sound to the interleaved array
                Dim CurrentWriteSampleIndexB As Integer = 0
                For Sample = AudioApiSettings.FramesPerBuffer * Output.Length - 1 To InputSound.WaveData.SampleData(OutputChannel.Value).Length - 1

                    Output(Output.Length - 1).InterleavedSampleArray(CurrentWriteSampleIndexB * NumberOfOutputChannels + CurrentChannelInterleavedPosition) = InputSound.WaveData.SampleData(OutputChannel.Value)(Sample)
                    CurrentWriteSampleIndexB += 1
                Next
            Next

            'Disposing Me
            Me.Dispose()

        End Sub

#Region "IDisposable Support"
        Private disposedValue As Boolean ' To detect redundant calls

        ' IDisposable
        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not disposedValue Then
                If disposing Then
                    ' TODO: dispose managed state (managed objects).
                End If

                ' TODO: free unmanaged resources (unmanaged objects) and override Finalize() below.
                ' TODO: set large fields to null.
            End If
            disposedValue = True
        End Sub

        ' TODO: override Finalize() only if Dispose(disposing As Boolean) above has code to free unmanaged resources.
        'Protected Overrides Sub Finalize()
        '    ' Do not change this code.  Put cleanup code in Dispose(disposing As Boolean) above.
        '    Dispose(False)
        '    MyBase.Finalize()
        'End Sub

        ' This code added by Visual Basic to correctly implement the disposable pattern.
        Public Sub Dispose() Implements IDisposable.Dispose
            ' Do not change this code.  Put cleanup code in Dispose(disposing As Boolean) above.
            Dispose(True)
            ' TODO: uncomment the following line if Finalize() is overridden above.
            ' GC.SuppressFinalize(Me)
        End Sub
#End Region
    End Class

    ''' <summary>
    ''' Changes the position of the currently playing sound to the start of the buffer containing the indicated time (in seconds). Returns the exact time selected as new start time. 
    ''' </summary>
    Public Function SeekTime(ByVal Time As Single) As Single

        Dim SelectedTime As Single

        Select Case CurrentOutputSound
            Case OutputSounds.OutputSoundA, OutputSounds.FadingToA

                'Locating the buffer containing the indicated start sample
                Dim NewStartPosition As Integer = 0
                For BufferIndex = 0 To OutputSoundA.Length - 1
                    If Time < OutputSoundA(BufferIndex).StartTime Then
                        NewStartPosition = BufferIndex
                        Exit For
                    End If
                Next

                'Limiting NewStartPosition to positive values, and values lower than the length of OutputSoundA, and then sets the new PositionA
                PositionA = Math.Min(Math.Max(0, NewStartPosition), OutputSoundA.Length - 1)

                'Stores the selected start time
                SelectedTime = OutputSoundA(PositionA).StartTime

                'Killing any fade process, and sets CurrentOutputSound to OutputSoundA
                If CurrentOutputSound = OutputSounds.FadingToA Then
                    CurrentOutputSound = OutputSounds.OutputSoundA
                    CrossFadeProgress = 0
                End If

            Case OutputSounds.OutputSoundB, OutputSounds.FadingToB

                'Locating the buffer containing the indicated start sample
                Dim NewStartPosition As Integer = 0
                For BufferIndex = 0 To OutputSoundB.Length - 1
                    If Time < OutputSoundB(BufferIndex).StartTime Then
                        NewStartPosition = BufferIndex
                        Exit For
                    End If
                Next

                'Limiting NewStartPosition to positive values, and values lower than the length of OutputSoundB, and then sets the new PositionB
                PositionB = Math.Min(Math.Max(0, NewStartPosition), OutputSoundB.Length - 1)

                'Stores the selected start time
                SelectedTime = OutputSoundB(PositionB).StartTime

                'Killing any fade process, and sets CurrentOutputSound to OutputSoundB
                If CurrentOutputSound = OutputSounds.FadingToB Then
                    CurrentOutputSound = OutputSounds.OutputSoundB
                    CrossFadeProgress = 0
                End If

            Case Else
                Throw New NotImplementedException
        End Select

        Return SelectedTime

    End Function

    ''' <summary>
    ''' Changes the position of the currently playing sound to the start of the buffer containing the indicated sample. Returns the exact sample selected as new start position. 
    ''' </summary>
    Public Function SeekSample(ByVal StartSample As Integer) As Integer

        Dim SelectedSample As Integer

        Select Case CurrentOutputSound
            Case OutputSounds.OutputSoundA, OutputSounds.FadingToA

                'Locating the buffer containing the indicated start sample
                Dim NewStartPosition As Integer = 0
                For BufferIndex = 0 To OutputSoundA.Length - 1
                    If StartSample < OutputSoundA(BufferIndex).StartSample Then
                        NewStartPosition = BufferIndex
                        Exit For
                    End If
                Next

                'Limiting NewStartPosition to positive values, and values lower than the length of OutputSoundA, and then sets the new PositionA
                PositionA = Math.Min(Math.Max(0, NewStartPosition), OutputSoundA.Length - 1)

                'Stores the selected start time
                SelectedSample = OutputSoundA(PositionA).StartSample

                'Killing any fade process, and sets CurrentOutputSound to OutputSoundA
                If CurrentOutputSound = OutputSounds.FadingToA Then
                    CurrentOutputSound = OutputSounds.OutputSoundA
                    CrossFadeProgress = 0
                End If

            Case OutputSounds.OutputSoundB, OutputSounds.FadingToB

                'Locating the buffer containing the indicated start sample
                Dim NewStartPosition As Integer = 0
                For BufferIndex = 0 To OutputSoundB.Length - 1
                    If StartSample < OutputSoundB(BufferIndex).StartSample Then
                        NewStartPosition = BufferIndex
                        Exit For
                    End If
                Next

                'Limiting NewStartPosition to positive values, and values lower than the length of OutputSoundB, and then sets the new PositionB
                PositionB = Math.Min(Math.Max(0, NewStartPosition), OutputSoundB.Length - 1)

                'Stores the selected start time
                SelectedSample = OutputSoundB(PositionB).StartSample

                'Killing any fade process, and sets CurrentOutputSound to OutputSoundB
                If CurrentOutputSound = OutputSounds.FadingToB Then
                    CurrentOutputSound = OutputSounds.OutputSoundB
                    CrossFadeProgress = 0
                End If

            Case Else
                Throw New NotImplementedException
        End Select

        Return SelectedSample

    End Function


    Public Sub ClearRecordedSound()
        InputBufferHistory.Clear()
    End Sub

    Public Sub OpenStream()

        Log("Opening stream...")
        Me.stream = StreamOpen()
        Log("Stream pointer: " & stream.ToString())

    End Sub




    ''' <summary>
    ''' Starts the sound stream.
    ''' </summary>
    ''' <param name="AppendRecordedSound">If set to True, the new recording will be appended any previously recorded sound. If set to False, a new recording will be started.</param>
    Public Sub Start(Optional ByVal AppendRecordedSound As Boolean = False,
                     Optional ByVal FadeInSound As Boolean = False)

        'Setting both sounds to silent sound
        SilentSound = {New BufferHolder(NumberOfOutputChannels, AudioApiSettings.FramesPerBuffer)}
        OutputSoundA = SilentSound
        OutputSoundB = SilentSound

        If AppendRecordedSound = False Then
            ClearRecordedSound()
        End If

        Log("Starting stream")
        If ErrorCheck("StartStream", PortAudio.Pa_StartStream(stream), True) = False Then
            _IsPlaying = True
        End If

    End Sub


    ''' <summary>
    ''' Stops the output sound.
    ''' </summary>
    ''' <param name="FadeOutSound">Set to true if fading out of the sound shold take place. (The fade out will occur during OverlapFadeLength +1 samples.)</param>
    Public Sub [Stop](Optional ByVal FadeOutSound As Boolean = False)

        'Stops recording directly
        RecordingIsActive = False

        'Calling stop right away if no fade out should be done
        If FadeOutSound = False Then
            StopNow()
        End If

        'Doing fade out by swapping to SilentSound
        NewSound = SilentSound

    End Sub

    Private Sub StopNow()

        Log("Stopping stream...")

        If ErrorCheck("StopStream", PortAudio.Pa_StopStream(stream), True) = False Then
            _IsPlaying = False
        End If

    End Sub


    Public Sub AbortStream() 'Optional ByVal StoreInputSound As Boolean = True)

        'Stops recording directly
        RecordingIsActive = False


        Log("Aborting stream...")

        If ErrorCheck("AbortStream", PortAudio.Pa_AbortStream(stream), True) = False Then
            _IsPlaying = False
        End If

        'If StoreInputSound = True Then
        'Storing recorded sound
        'StoreRecordedSound()
        'End If

    End Sub



    Public Sub CloseStream()

        'Stopping the stream if it is running
        If PortAudio.Pa_IsStreamStopped(Me.stream) < 1 Then
            [Stop]()
        End If

        'Cloing the stream
        If ErrorCheck("CloseStream", PortAudio.Pa_CloseStream(stream), True) = False Then

            _IsStreamOpen = False

            'Resetting the stream
            Me.stream = New IntPtr(0)
        End If

    End Sub


    Private Function StreamOpen() As IntPtr

        'Setting buffer length data, and adjusting the length of the buffer arrays
        'Dim HighestChannelCount As Integer = Math.Max(NumberOfOutputChannels, NumberOfInputChannels)

        'Do recording and playback buffers need to be of equal length?

        'Setting/updating the length of the playback buffer
        Log("Creating a new playback buffer length with the length: " & Me.AudioApiSettings.FramesPerBuffer * NumberOfOutputChannels)
        PlaybackBuffer = New Single((Me.AudioApiSettings.FramesPerBuffer * NumberOfOutputChannels) - 1) {}
        SilentBuffer = New Single((Me.AudioApiSettings.FramesPerBuffer * NumberOfOutputChannels) - 1) {}

        'Setting/updating the length of the recording buffer
        Log("Creating a new recording buffer length with the length: " & Me.AudioApiSettings.FramesPerBuffer * NumberOfInputChannels)
        RecordingBuffer = New Single((Me.AudioApiSettings.FramesPerBuffer * NumberOfInputChannels) - 1) {}

        Dim stream As New IntPtr()
        Dim data As New IntPtr(0)

        Dim inputParams As New PortAudio.PaStreamParameters
        If Me.AudioApiSettings.SelectedInputDevice IsNot Nothing Then
            inputParams.channelCount = NumberOfInputChannels
            inputParams.device = Me.AudioApiSettings.SelectedInputDevice
            inputParams.sampleFormat = PaSampleFormat

            If Me.AudioApiSettings.SelectedInputAndOutputDeviceInfo.HasValue = True Then
                inputParams.suggestedLatency = Me.AudioApiSettings.SelectedInputAndOutputDeviceInfo.Value.defaultLowInputLatency
            Else
                inputParams.suggestedLatency = Me.AudioApiSettings.SelectedInputDeviceInfo.Value.defaultLowInputLatency
            End If
        End If

        Dim outputParams As New PortAudio.PaStreamParameters
        If Me.AudioApiSettings.SelectedOutputDevice IsNot Nothing Then
            outputParams.channelCount = NumberOfOutputChannels
            outputParams.device = Me.AudioApiSettings.SelectedOutputDevice
            outputParams.sampleFormat = PaSampleFormat

            If Me.AudioApiSettings.SelectedInputAndOutputDeviceInfo.HasValue = True Then
                outputParams.suggestedLatency = Me.AudioApiSettings.SelectedInputAndOutputDeviceInfo.Value.defaultLowOutputLatency
            Else
                outputParams.suggestedLatency = Me.AudioApiSettings.SelectedOutputDeviceInfo.Value.defaultLowOutputLatency
            End If
        End If

        Log(inputParams.ToString)
        Log(outputParams.ToString)

        Dim Flag As PortAudio.PaStreamFlags
        If IsClippingInactivated = True Then
            Flag = PortAudio.PaStreamFlags.paClipOff
        Else
            Flag = PortAudio.PaStreamFlags.paNoFlag
        End If

        Select Case SoundDirection
            Case SoundDirections.PlaybackOnly
                ErrorCheck("OpenOutputOnlyStream", PortAudio.Pa_OpenStream(stream, New Nullable(Of PortAudioVB.PortAudio.PaStreamParameters), outputParams,
                                                                       Me.AudioApiSettings.SampleRate, Me.AudioApiSettings.FramesPerBuffer, Flag, Me.paStreamCallback, data), True)

            Case SoundDirections.RecordingOnly
                ErrorCheck("OpenInputOnlyStream", PortAudio.Pa_OpenStream(stream, inputParams, New Nullable(Of PortAudioVB.PortAudio.PaStreamParameters),
                                                                      Me.AudioApiSettings.SampleRate, Me.AudioApiSettings.FramesPerBuffer, Flag, Me.paStreamCallback, data), True)

            Case SoundDirections.Duplex
                ErrorCheck("OpenDuplexStream", PortAudio.Pa_OpenStream(stream, inputParams, outputParams, Me.AudioApiSettings.SampleRate, Me.AudioApiSettings.FramesPerBuffer, Flag,
                                                                   Me.paStreamCallback, data), True)
        End Select

        _IsStreamOpen = True

        Return stream
    End Function


    ''' <summary>
    ''' Gets the recorded sound so far.
    ''' </summary>
    ''' <returns></returns>
    Public Function GetRecordedSound() As Audio.Sound

        Log("Attemting to get recorded sound")

        'Stopping sound if not already done
        If _IsPlaying = True Then [Stop](0.1)

        'Returning nothing if no input sound exists
        If RecordingIsActive = False Then Return Nothing
        If InputBufferHistory Is Nothing Then Return Nothing

        'Creating a new Audio.Sound
        Dim RecordedSound As New Audio.Sound(RecordingSoundFormat)

        If InputBufferHistory.Count = 0 Then Return RecordedSound

        If InputBufferHistory.Count > 0 Then

            'Determining output sound length
            Dim OutputSoundSampleCount As Long = 0
            For Each Buffer In InputBufferHistory
                OutputSoundSampleCount += Buffer.Length / RecordingSoundFormat.Channels
            Next

            For ch = 0 To RecordingSoundFormat.Channels - 1
                Dim NewChannelArray(OutputSoundSampleCount - 1) As Single
                RecordedSound.WaveData.SampleData(ch + 1) = NewChannelArray
            Next

            'Sorting the interleaved samples to 
            Dim CurrentBufferStartSample As Long = 0
            For Each Buffer In InputBufferHistory
                Dim CurrentBufferSampleIndex As Long = 0
                For CurrentDataPoint = 0 To Buffer.Length - 1 Step RecordingSoundFormat.Channels

                    For ch = 0 To RecordingSoundFormat.Channels - 1
                        Try
                            RecordedSound.WaveData.SampleData(ch + 1)(CurrentBufferStartSample + CurrentBufferSampleIndex) = Buffer(CurrentDataPoint + ch)
                        Catch ex As Exception
                            Log(ErrorCheck("Terminate", PortAudio.Pa_Terminate(), True))
                        End Try
                    Next
                    'Increasing sample index
                    CurrentBufferSampleIndex += 1
                Next
                CurrentBufferStartSample += CurrentBufferSampleIndex
            Next

        End If
        Return RecordedSound

    End Function

    ''' <summary>
    ''' Returns the time delay (in seconds) caused by the call-back buffer size.
    ''' </summary>
    ''' <returns></returns>
    Public Function GetCallBackTime() As Double

        Return AudioApiSettings.FramesPerBuffer / AudioApiSettings.SampleRate

    End Function

    Private Sub Log(logString As String)
        If m_loggingEnabled = True Then
            System.Console.WriteLine("PortAudio: " & logString)
        End If
    End Sub

    Private Sub DisplayMessageInBox(Message As String)
        If m_messagesEnabled = True Then
            MsgBox(Message)
        End If
    End Sub

    Public Shared LogToFileEnabled As Boolean = False
    Private Sub LogToFile(Message As String)
        If LogToFileEnabled = True Then
            SendInfoToAudioLog(Message)
        End If
    End Sub

    Private Function ErrorCheck(action As String, errorCode As PortAudio.PaError, Optional ShowErrorInMsgBox As Boolean = False) As Boolean
        If errorCode <> PortAudio.PaError.paNoError Then
            Dim MessageA As String = action & " error: " & PortAudio.Pa_GetErrorText(errorCode)
            Log(MessageA)
            If ShowErrorInMsgBox = True Then DisplayMessageInBox(MessageA)

            LogToFile(MessageA)

            If errorCode = PortAudio.PaError.paUnanticipatedHostError Then
                Dim errorInfo As PortAudio.PaHostErrorInfo = PortAudio.Pa_GetLastHostErrorInfo()
                Dim MessageB As String = "- Host error API type: " & errorInfo.hostApiType
                Dim MessageC As String = "- Host error code: " & errorInfo.errorCode
                Dim MessageD As String = "- Host error text: " & errorInfo.errorText
                Log(MessageB)
                Log(MessageC)
                Log(MessageD)

                LogToFile(MessageB)
                LogToFile(MessageC)
                LogToFile(MessageD)

                If ShowErrorInMsgBox = True Then DisplayMessageInBox(MessageB & vbCrLf & MessageC & vbCrLf & MessageD)
            End If

            Return True
        Else
            Log(action & " OK")
            LogToFile(action & " OK")
            Return False
        End If
    End Function


    Private Sub SendMessageToController(ByVal Message As ISoundPlayerControl.MessagesFromSoundPlayer,
                                        Optional ByVal SendOnNewThread As Boolean = True)

        If SendOnNewThread = False Then
            'Sending message on same thread (Should not be used when messages are sent from within the callback!)
            MyController.MessageFromPlayer(Message)

        Else
            'Sending message on a new thread, allowing the main thread to continue execution
            Dim NewthreadMessageSender As New MessageSenderOnNewThread(Message, MyController)

        End If

        'Dim NewThread As New Thread(AddressOf SendMessage)

        'NewThread.Join()
        'Dim 
        'ThreadPool.QueueUserWorkItem(,)
        'MyController.Handle

    End Sub

    ''' <summary>
    ''' A class used to send one ISoundPlayerControl.MessagesFromSoundPlayer message on a new thread.
    ''' </summary>
    Private Class MessageSenderOnNewThread
        Implements IDisposable

        Private Message As ISoundPlayerControl.MessagesFromSoundPlayer
        Private Controller As ISoundPlayerControl

        ''' <summary>
        ''' Sends the supplied message to the indicated Controller directly on initiation and then desposes the sending object.
        ''' </summary>
        ''' <param name="Message"></param>
        ''' <param name="Controller"></param>
        Public Sub New(ByRef Message As ISoundPlayerControl.MessagesFromSoundPlayer, ByRef Controller As ISoundPlayerControl)
            Me.Message = Message
            Me.Controller = Controller

            'Sending message on a new thread
            Dim NewThred As New Thread(AddressOf SendMessage)
            NewThred.Start()

        End Sub

        Private Sub SendMessage()

            'Sending the message
            Controller.MessageFromPlayer(Message)

            'Disposing Me
            Me.Dispose()

        End Sub

#Region "IDisposable Support"
        Private disposedValue As Boolean ' To detect redundant calls

        ' IDisposable
        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not disposedValue Then
                If disposing Then
                    ' TODO: dispose managed state (managed objects).
                End If

                ' TODO: free unmanaged resources (unmanaged objects) and override Finalize() below.
                ' TODO: set large fields to null.
            End If
            disposedValue = True
        End Sub

        ' TODO: override Finalize() only if Dispose(disposing As Boolean) above has code to free unmanaged resources.
        'Protected Overrides Sub Finalize()
        '    ' Do not change this code.  Put cleanup code in Dispose(disposing As Boolean) above.
        '    Dispose(False)
        '    MyBase.Finalize()
        'End Sub

        ' This code added by Visual Basic to correctly implement the disposable pattern.
        Public Sub Dispose() Implements IDisposable.Dispose
            ' Do not change this code.  Put cleanup code in Dispose(disposing As Boolean) above.
            Dispose(True)
            ' TODO: uncomment the following line if Finalize() is overridden above.
            ' GC.SuppressFinalize(Me)
        End Sub
#End Region

    End Class


#Region "IDisposable Support"
    Private disposedValue As Boolean ' To detect redundant calls

    ' IDisposable
    Protected Overridable Sub Dispose(disposing As Boolean)
        If Not disposedValue Then
            If disposing Then
                ' TODO: dispose managed state (managed objects).
            End If

            ' TODO: free unmanaged resources (unmanaged objects) and override Finalize() below.
            ' TODO: set large fields to null.
            Log("Terminating...")
            ErrorCheck("Terminate", PortAudio.Pa_Terminate(), True)

        End If
        disposedValue = True
    End Sub

    ' TODO: override Finalize() only if Dispose(disposing As Boolean) above has code to free unmanaged resources.
    Protected Overrides Sub Finalize()
        '    ' Do not change this code.  Put cleanup code in Dispose(disposing As Boolean) above.
        Dispose(False)
        MyBase.Finalize()
    End Sub

    ' This code added by Visual Basic to correctly implement the disposable pattern.
    Public Sub Dispose() Implements IDisposable.Dispose
        ' Do not change this code.  Put cleanup code in Dispose(disposing As Boolean) above.
        Dispose(True)
        ' TODO: uncomment the following line if Finalize() is overridden above.
        GC.SuppressFinalize(Me)
    End Sub

#End Region

End Class
