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

Imports System.IO
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Xml
Imports System.Globalization.CultureInfo

''' <summary>
''' A class used to hold audio data.
''' </summary>
<Serializable>
Public Class Sound

    Private _fileName As String
    Public Property FileName As String
        Get
            Return _fileName
        End Get
        Set(value As String)
            'Removes file extensions
            _fileName = Path.GetFileNameWithoutExtension(value)
        End Set
    End Property

    ''' <summary>
    ''' If the Sound object was read from file, SourcePath should contain the full path of the source audio file.
    ''' </summary>
    Public SourcePath As String = ""

    ''' <summary>
    ''' Holds the wave sample data of the current sound.
    ''' </summary>
    ''' <returns></returns>
    Public Property WaveData As LocalWaveData

    ''' <summary>
    ''' Gets the wave format of the current sound.
    ''' </summary>
    ''' <returns></returns>
    Public ReadOnly Property WaveFormat As Formats.WaveFormat

    Private _SMA As SpeechMaterialAnnotation

    ''' <summary>
    ''' Holds SMA type data (Speech Material Annotation)
    ''' </summary>
    ''' <returns></returns>
    Public Property SMA As SpeechMaterialAnnotation
        Get
            Return _SMA
        End Get
        Set(value As SpeechMaterialAnnotation)
            _SMA = value
            _SMA.ParentSound = Me
        End Set
    End Property

    Private _Box As Object
    ''' <summary>
    ''' A flexible property that can be used to store any type of data.
    ''' </summary>
    ''' <returns></returns>
    Public Property Box As Object
        Get
            Return _Box
        End Get
        Set(value As Object)
            'Setting the current box type
            CurrentBoxType = value.GetType

            'Storing the value
            _Box = value
        End Set
    End Property

    ''' <summary>
    ''' Holds the type of the currently stored Box data. 
    ''' </summary>
    ''' <returns></returns>
    Public Property CurrentBoxType As Type

    ''' <summary>
    ''' With the exception of the SMA node, this list contains the raw xml of all iXML sub-nodes read from an audiofile. iXML tags/keywords as keys (xml Name), and data as values (xml InnerXml).
    ''' </summary>
    ''' <returns></returns>
    Public Property iXmlNodes As List(Of Tuple(Of String, String))

    ''' <summary>
    ''' A list that holds wave file chunks that were never parsed, but instead just kept as byte arrays. Thses should never be modified, lest wave file writing will break!
    ''' </summary>
    ''' <returns></returns>
    Private Property UnparsedWaveChunks As New List(Of Byte())

    ''' <summary>
    ''' Creates a new instance of the Sound class.
    ''' </summary>
    ''' <param name="Waveformat"></param>
    ''' <param name="FileName"></param>
    Public Sub New(ByVal Waveformat As Formats.WaveFormat, Optional FileName As String = "")

        Me.FileName = FileName
        Me.WaveFormat = Waveformat
        WaveData = New LocalWaveData(Waveformat.Channels)
        SMA = New SpeechMaterialAnnotation()

        'Adding Waveformat.Channels channels and one sentence in each channel
        For n = 1 To Waveformat.Channels
            SMA.AddChannelData(New Sound.SpeechMaterialAnnotation.SmaChannelData(SMA))
            SMA.ChannelData(n).Add(New Sound.SpeechMaterialAnnotation.SmaSentenceData(SMA))
        Next

    End Sub

    ''' <summary>
    ''' Creates a new Sound which is a deep copy of the original, by using serialization.
    ''' </summary>
    ''' <returns></returns>
    Public Function CreateCopy() As Sound

        'Creating an output object
        Dim newSound As Sound

        'Serializing to memorystream
        Dim serializedMe As New MemoryStream
        Dim serializer As New BinaryFormatter
        serializer.Serialize(serializedMe, Me)

        'Deserializing to new object
        serializedMe.Position = 0
        newSound = CType(serializer.Deserialize(serializedMe), Sound)
        serializedMe.Close()

        'Returning the new object
        Return newSound
    End Function

    ''' <summary>
    ''' Creates a new sound with a copy (not a reference) of the original sample data. The WaveFormat in the new sound, however, is a reference to the wave format in the original sound.
    ''' </summary>
    ''' <returns></returns>
    Public Function CreateSoundDataCopy() As Sound

        'Creating a new sound
        Dim NewSound As New Audio.Sound(WaveFormat)

        'Copying sound data
        For c = 1 To WaveFormat.Channels
            Dim NewChannelArray(WaveData.SampleData(c).Length - 1) As Single
            For s = 0 To NewChannelArray.Length - 1
                NewChannelArray(s) = WaveData.SampleData(c)(s)
            Next
            NewSound.WaveData.SampleData(c) = NewChannelArray
        Next

        'Returning the new sound
        Return NewSound

    End Function


    ''' <summary>
    ''' Copies the sample data of one channel in the original sound to a new sound.
    ''' </summary>
    ''' <param name="Channel"></param>
    ''' <returns></returns>
    Public Function CopyChannelToMonoSound(ByVal Channel As Integer) As Sound

        Dim OutputSound As New Sound(New Formats.WaveFormat(WaveFormat.SampleRate, WaveFormat.BitDepth, 1,, WaveFormat.Encoding), FileName)

        Dim NewChannelArray(WaveData.SampleData(Channel).Length) As Single
        For s = 0 To WaveData.SampleData(Channel).Length - 1
            NewChannelArray(s) = WaveData.SampleData(Channel)(s)
        Next
        OutputSound.WaveData.SampleData(1) = NewChannelArray

        Return OutputSound

    End Function

    ''' <summary>
    ''' Creates a new sound converted to the specified format. The SMA object neither copied or referenced.
    ''' </summary>
    ''' <returns></returns>
    Public Function BitDepthConversion(ByRef NewWaveFormat As Formats.WaveFormat) As Sound

        'Setting the conversion factor 
        Dim SampleConversionFactor As Double = 1
        Select Case WaveFormat.BitDepth
            Case 16
                Select Case NewWaveFormat.BitDepth
                    Case 16
                            'Same, does nothing
                    Case 32
                        SampleConversionFactor = 1 / WaveFormat.PositiveFullScale
                    Case Else
                        Throw New NotImplementedException
                End Select

            Case 32
                Select Case NewWaveFormat.BitDepth
                    Case 16
                        SampleConversionFactor = 1 * NewWaveFormat.PositiveFullScale
                    Case 32
                        'Same, does nothing
                    Case Else
                        Throw New NotImplementedException
                End Select
            Case Else
                Throw New NotImplementedException
        End Select

        'Creating an output sound
        Dim OutputSound As New Sound(NewWaveFormat)

        For c = 1 To OutputSound.WaveFormat.Channels

            'Copying and converting the sample data
            Dim NewChannelArray(WaveData.SampleData(c).Length - 1) As Single
            OutputSound.WaveData.SampleData(c) = NewChannelArray
            For n = 0 To OutputSound.WaveData.SampleData(c).Length - 1
                OutputSound.WaveData.SampleData(c)(n) = WaveData.SampleData(1)(n) * SampleConversionFactor
            Next
        Next

        Return OutputSound

    End Function


    ''' <summary>
    ''' Creates a zero-padded copy of the current sound.
    ''' <param name="PreSoundPaddingLength">The length in samples of the silence to be added prior to the existing sound.</param>
    ''' <param name="PostSoundPaddingLength">The length in samples of the silence to be added after the existing sound.</param> 
    ''' <param name="CreateNewSound">If set to false, nothing will be returned, but instead the original sound will be zero padded.</param>
    ''' </summary>
    ''' <returns>Returns a new sound, which is a zero-padded copy of the current sound.</returns>
    Public Function ZeroPad(Optional ByVal PreSoundPaddingLength As Integer? = Nothing, Optional ByVal PostSoundPaddingLength As Integer? = Nothing, Optional ByVal CreateNewSound As Boolean = False) As Sound

        'Determining the length of the pre sound panning
        If PreSoundPaddingLength Is Nothing Then PreSoundPaddingLength = 0

        'Determining the length of the post sound panning
        If PostSoundPaddingLength Is Nothing Then PostSoundPaddingLength = 0

        Dim OutputSound As Sound = Nothing
        If CreateNewSound = True Then
            'Creates a copy
            OutputSound = Me.CreateCopy
        End If

        For c = 1 To WaveFormat.Channels
            'Create new sound channel arrays
            Dim NewChannelArray(PreSoundPaddingLength + WaveData.SampleData(c).Length + PostSoundPaddingLength - 1) As Single

            'Copying the sound from the current instance to the appropriate place in the new channel array
            For s = 0 To WaveData.SampleData(c).Length - 1
                NewChannelArray(s + PreSoundPaddingLength) = WaveData.SampleData(c)(s)
            Next

            If CreateNewSound = True Then
                'Adding the new channel array
                OutputSound.WaveData.SampleData(c) = NewChannelArray
            Else
                'Replacing the sound array in the original sound
                WaveData.SampleData(c) = NewChannelArray
            End If

        Next

        If CreateNewSound = True Then

            'Returning the new sound
            Return OutputSound
        Else
            'Returning the current instance of Sound.
            Return Me
        End If

    End Function


    ''' <summary>
    ''' Creates a zero-padded copy of the current sound.
    ''' <param name="PreSoundPadding">The length in seconds of the silence to be added prior to the existing sound.</param>
    ''' <param name="PostSoundPadding">The length in seconds of the silence to be added after the existing sound.</param> 
    ''' <param name="CreateNewSound">If set to false, nothing will be returned, but instead the original sound will be zero padded.</param>
    ''' </summary>
    ''' <returns>Returns a new sound, which is a zero-padded copy of the current sound.</returns>
    Public Function ZeroPad(Optional ByVal PreSoundPadding As Double? = Nothing, Optional ByVal PostSoundPadding As Double? = Nothing, Optional ByVal CreateNewSound As Boolean = False) As Sound

        'Determining the length of the pre sound panning
        Dim PreSoundPaddingLength As Integer = 0
        If PreSoundPadding IsNot Nothing Then
            PreSoundPaddingLength = PreSoundPadding * WaveFormat.SampleRate
        End If

        'Determining the length of the post sound panning
        Dim PostSoundPaddingLength As Integer = 0
        If PostSoundPadding IsNot Nothing Then
            PostSoundPaddingLength = PostSoundPadding * WaveFormat.SampleRate
        End If

        'Returns the new sound, by calling ZeroPad with sample parameters
        Return ZeroPad(PreSoundPaddingLength, PostSoundPaddingLength, CreateNewSound)

    End Function

    ''' <summary>
    ''' Creates copy of the current sound which is zero-padded to a certain duration (in seconds).
    ''' <param name="TotalDuration">The duration in seconds to which the sound should be zero padded. If the sound is longer than this duration, no padding is done, but instead a copy of the original sound will be returned.</param>
    ''' <param name="CreateNewSound">If set to false, nothing will be returned, but instead the original sound will be zero padded.</param>
    ''' </summary>
    ''' <returns>Returns a new sound, which is a zero-padded copy of the current sound.</returns>
    Public Function ZeroPad(ByVal TotalDuration As Double, Optional ByVal CreateNewSound As Boolean = False) As Sound

        Dim TotalLength As Integer = TotalDuration * WaveFormat.SampleRate
        Dim PaddingLength As Integer = TotalLength - WaveData.ShortestChannelSampleCount

        If PaddingLength <= 0 Then
            If CreateNewSound = True Then
                Return Me.CreateCopy
            Else
                Return Nothing
            End If
        Else
            Return ZeroPad(0, PaddingLength, CreateNewSound)
        End If

    End Function

    ''' <summary>
    ''' Creates copy of the current sound which is zero-padded to a certain length (in samples).
    ''' <param name="TotalLength">The length in samples to which the sound should be zero padded. If the shortest channel of the sound is longer than this length, no padding is done, but instead a copy of the original sound will be returned.</param>
    ''' <param name="CreateNewSound">If set to false, nothing will be returned, but instead the original sound will be zero padded.</param>
    ''' </summary>
    ''' <returns>Returns a new sound, which is a zero-padded copy of the current sound.</returns>
    Public Function ZeroPad(ByVal TotalLength As Integer, Optional ByVal CreateNewSound As Boolean = False) As Sound

        Dim PaddingLength As Integer = TotalLength - WaveData.ShortestChannelSampleCount

        If PaddingLength <= 0 Then
            If CreateNewSound = True Then
                Return Me.CreateCopy
            Else
                Return Nothing
            End If
        Else
            Return ZeroPad(0, PaddingLength, CreateNewSound)
        End If

    End Function

    <Serializable>
    Class LocalWaveData

        Private _sampleData As List(Of Single())

        Public ReadOnly Property LongestChannelSampleCount As Integer
            Get
                Return DetermineLongestChannelLength()
            End Get
        End Property

        Public ReadOnly Property ShortestChannelSampleCount As Integer
            Get
                Return DetermineShortestChannelLength()
            End Get
        End Property

        ''' <summary>
        ''' Gets or sets or the sample data of the specified channel (Channel indices are 1-based).
        ''' </summary>
        ''' <param name="channel"></param>
        ''' <returns></returns>
        Public Property SampleData(Optional ByVal channel As Integer = 1) As Single()
            Get
                Return _sampleData(channel - 1)
            End Get
            Set(value As Single())
                _sampleData(channel - 1) = value
            End Set
        End Property

        Private Function DetermineLongestChannelLength()

            'Determining max length
            Dim maxLength As Integer = 0
            For c = 0 To _sampleData.Count - 1
                If _sampleData(c).Length > maxLength Then maxLength = _sampleData(c).Length
            Next
            Return maxLength

        End Function

        Private Function DetermineShortestChannelLength()

            'Determining min length
            Dim minLength As Integer = Integer.MaxValue
            If _sampleData.Count = 0 Then
                minLength = 0
            Else
                For c = 0 To _sampleData.Count - 1
                    If _sampleData(c).Length < minLength Then minLength = _sampleData(c).Length
                Next
            End If
            Return minLength

        End Function

        ''' <summary>
        '''Enforcing equal channel length, by extending all channels to the length of the longest
        ''' </summary>
        Public Sub EnforceEqualChannelLength()

            'Determining max length
            Dim maxLength As Integer = LongestChannelSampleCount

            'Setting all channel arrays to the max length, using ReDim Preserve
            For c = 0 To _sampleData.Count - 1
                If _sampleData(c).Length < maxLength Then
                    ReDim Preserve _sampleData(c)(maxLength - 1)
                End If
            Next

        End Sub

        ''' <summary>
        ''' Creates a new instance of LocalWaveData
        ''' </summary>
        ''' <param name="Channels">The number of audio channels to initialize.</param>
        Public Sub New(ByVal Channels As Integer)

            _sampleData = New List(Of Single())
            For n = 0 To Channels - 1
                Dim ChannelSoundData As Single() = {}
                _sampleData.Add(ChannelSoundData)
            Next

        End Sub

    End Class


    ''' <summary>
    ''' A class used to store data related to the segmentation of speech material sound files. All data can be contained and saved in a wave file.
    ''' </summary>
    <Serializable>
    Class SpeechMaterialAnnotation

        Public Property ParentSound As Audio.Sound

        Public Const CurrentVersion As String = "1.0"
        ''' <summary>
        ''' Holds the SMA version of the file that the data was loaded from, or CurrentVersion if the data was not loaded from file.
        ''' </summary>
        Public ReadFromVersion As String = CurrentVersion ' Using CurrentVersion as default
        
        Public ReadOnly Property ChannelCount
            Get
                Return _ChannelData.Count
            End Get
        End Property

        Private _ChannelData As New List(Of SmaChannelData)
        Property ChannelData(ByVal Channel As Integer) As SmaChannelData
            Get
                Return _ChannelData(Channel - 1)
            End Get
            Set(value As SmaChannelData)
                _ChannelData(Channel - 1) = value
            End Set
        End Property

        Public Sub AddChannelData()
            _ChannelData.Add(New SmaChannelData(Me))
        End Sub

        Public Sub AddChannelData(ByRef NewSmaChannelData As SmaChannelData)
            _ChannelData.Add(NewSmaChannelData)
        End Sub

        ''' <summary>
        ''' Creates a new instance of SpeechMaterialAnnotation
        ''' </summary>
        Public Sub New(Optional ByVal DefaultFrequencyWeighting As FrequencyWeightings = FrequencyWeightings.Z,
                       Optional ByVal DefaultTimeWeighting As Double = 0)
            SetFrequencyWeighting(DefaultFrequencyWeighting, False)
            SetTimeWeighting(DefaultTimeWeighting, False)
        End Sub

        Private _FrequencyWeighting As FrequencyWeightings = FrequencyWeightings.Z
        Public Function GetFrequencyWeighting() As FrequencyWeightings
            Return _FrequencyWeighting
        End Function

        Public Sub SetFrequencyWeighting(ByVal FrequencyWeighting As FrequencyWeightings, ByVal EnforceOnAllDescendents As Boolean)

            'Setting only SMA top level frequency weighting
            _FrequencyWeighting = FrequencyWeighting

            If EnforceOnAllDescendents = True Then

                'Enforcing the same frequency weighting on all descendant channels, sentences, words, and phones
                For c = 0 To _ChannelData.Count - 1
                    _ChannelData(c).SetFrequencyWeighting(FrequencyWeighting, False)
                    For s = 0 To _ChannelData(c).Count - 1
                        _ChannelData(c)(s).SetFrequencyWeighting(FrequencyWeighting, False)
                        For w = 0 To _ChannelData(c)(s).Count - 1
                            _ChannelData(c)(s)(w).SetFrequencyWeighting(FrequencyWeighting, False)
                            For p = 0 To _ChannelData(c)(s)(w).PhoneData.Count - 1
                                _ChannelData(c)(s)(w).PhoneData(p).FrequencyWeighting = FrequencyWeighting
                            Next
                        Next
                    Next
                Next
            End If
        End Sub

        Private _TimeWeighting As Double = 0
        Public Function GetTimeWeighting() As Double
            Return _TimeWeighting
        End Function

        Public Sub SetTimeWeighting(ByVal TimeWeighting As Double, ByVal EnforceOnAllDescendents As Boolean)

            'Setting only SMA top level Time weighting
            _TimeWeighting = TimeWeighting

            If EnforceOnAllDescendents = True Then

                'Enforcing the same Time weighting on all descendant channels, sentences, words, and phones
                For c = 0 To _ChannelData.Count - 1
                    _ChannelData(c).SetTimeWeighting(TimeWeighting, False)
                    For s = 0 To _ChannelData(c).Count - 1
                        _ChannelData(c)(s).SetTimeWeighting(TimeWeighting, False)
                        For w = 0 To _ChannelData(c)(s).Count - 1
                            _ChannelData(c)(s)(w).SetTimeWeighting(TimeWeighting, False)
                            For p = 0 To _ChannelData(c)(s)(w).PhoneData.Count - 1
                                _ChannelData(c)(s)(w).PhoneData(p).TimeWeighting = TimeWeighting
                            Next
                        Next
                    Next
                Next
            End If
        End Sub

        Public Shadows Function ToString(ByVal IncludeHeadings As Boolean) As String

            Dim OutputList As New List(Of String)

            If IncludeHeadings = True Then

                OutputList.Add("SMA")
                OutputList.Add("SMA_VERSION:" & vbTab & Audio.Sound.SpeechMaterialAnnotation.CurrentVersion) 'I.e. SMA version number

                For channel As Integer = 1 To ChannelCount

                    'Writing channel data
                    OutputList.Add("CHANNEL")
                    OutputList.Add("ORTHOGRAPHIC_FORM")
                    OutputList.Add("PHONETIC_FORM")

                    OutputList.Add("START_SAMPLE")
                    OutputList.Add("LENGTH")

                    If ChannelData(channel).UnWeightedLevel IsNot Nothing Then
                        OutputList.Add("UNWEIGHTED_LEVEL")
                    End If

                    If ChannelData(channel).UnWeightedPeakLevel IsNot Nothing Then
                        OutputList.Add("UNWEIGHTED_PEAKLEVEL")
                    End If

                    If ChannelData(channel).WeightedLevel IsNot Nothing Then
                        OutputList.Add("WEIGHTED_LEVEL")
                    End If
                    OutputList.Add("FREQUENCY_WEIGHTING")
                    OutputList.Add("TIME_WEIGHTING")

                    For sentence As Integer = 0 To ChannelData(channel).Count - 1

                        'Writing sentence data
                        OutputList.Add("SENTENCE")
                        OutputList.Add("ORTHOGRAPHIC_FORM")
                        OutputList.Add("PHONETIC_FORM")
                        OutputList.Add("START_SAMPLE")
                        OutputList.Add("LENGTH")
                        OutputList.Add("UNWEIGHTED_LEVEL")
                        OutputList.Add("UNWEIGHTED_PEAKLEVEL")
                        OutputList.Add("WEIGHTED_LEVEL")
                        OutputList.Add("FREQUENCY_WEIGHTING")
                        OutputList.Add("TIME_WEIGHTING")
                        OutputList.Add("INITIAL_PEAK")
                        OutputList.Add("START_TIME")

                        For word = 0 To ChannelData(channel)(sentence).Count - 1

                            'writing word data
                            OutputList.Add("WORD")
                            OutputList.Add("ORTHOGRAPHIC_FORM")
                            OutputList.Add("PHONETIC_FORM")
                            OutputList.Add("START_SAMPLE")
                            OutputList.Add("LENGTH")
                            OutputList.Add("UNWEIGHTED_LEVEL")
                            OutputList.Add("UNWEIGHTED_PEAKLEVEL")
                            OutputList.Add("WEIGHTED_LEVEL")
                            OutputList.Add("FREQUENCY_WEIGHTING")
                            OutputList.Add("TIME_WEIGHTING")
                            OutputList.Add("START_TIME")

                            For phone = 0 To ChannelData(channel)(sentence)(word).PhoneData.Count - 1

                                'writing phone data
                                OutputList.Add("PHONE")
                                OutputList.Add("ORTHOGRAPHIC_FORM")
                                OutputList.Add("PHONETIC_FORM")
                                OutputList.Add("START_SAMPLE")
                                OutputList.Add("LENGTH")
                                OutputList.Add("UNWEIGHTED_LEVEL")
                                OutputList.Add("UNWEIGHTED_PEAKLEVEL")
                                OutputList.Add("WEIGHTED_LEVEL")
                                OutputList.Add("FREQUENCY_WEIGHTING")
                                OutputList.Add("TIME_WEIGHTING")

                                'End of phone
                            Next
                            'End of word
                        Next
                        'End of sentence
                    Next
                    'End of channel
                Next
            End If

            'Start writing data
            Dim DefaultNotMeasuredValue As String = "Not measured"
            If IncludeHeadings = True Then
                OutputList.Add(vbCrLf)
            Else
            End If

            For channel As Integer = 1 To ChannelCount

                'Writing channel data

                OutputList.Add(channel)

                OutputList.Add(ChannelData(channel).OrthographicForm.ToString)
                OutputList.Add(ChannelData(channel).PhoneticForm.ToString)

                OutputList.Add(ChannelData(channel).StartSample)
                OutputList.Add(ChannelData(channel).Length)

                If ChannelData(channel).UnWeightedLevel IsNot Nothing Then
                    OutputList.Add(ChannelData(channel).UnWeightedLevel.Value.ToString(InvariantCulture))
                Else
                    OutputList.Add(DefaultNotMeasuredValue)
                End If

                If ChannelData(channel).UnWeightedPeakLevel IsNot Nothing Then
                    OutputList.Add(ChannelData(channel).UnWeightedPeakLevel.Value.ToString(InvariantCulture))
                Else
                    OutputList.Add(DefaultNotMeasuredValue)
                End If

                If ChannelData(channel).WeightedLevel IsNot Nothing Then
                    OutputList.Add(ChannelData(channel).WeightedLevel.Value.ToString(InvariantCulture))
                Else
                    OutputList.Add(DefaultNotMeasuredValue)
                End If
                OutputList.Add(ChannelData(channel).GetFrequencyWeighting.ToString)
                OutputList.Add(ChannelData(channel).GetTimeWeighting.ToString(InvariantCulture))

                For sentence As Integer = 0 To ChannelData(channel).Count - 1

                    'Writing sentence data
                    OutputList.Add(sentence)
                    OutputList.Add(ChannelData(channel)(sentence).OrthographicForm)
                    OutputList.Add(ChannelData(channel)(sentence).PhoneticForm)
                    OutputList.Add(ChannelData(channel)(sentence).StartSample)
                    OutputList.Add(ChannelData(channel)(sentence).Length)
                    If ChannelData(channel)(sentence).UnWeightedLevel IsNot Nothing Then
                        OutputList.Add(ChannelData(channel)(sentence).UnWeightedLevel.Value.ToString(InvariantCulture))
                    Else
                        OutputList.Add(DefaultNotMeasuredValue)
                    End If

                    If ChannelData(channel)(sentence).UnWeightedPeakLevel IsNot Nothing Then
                        OutputList.Add(ChannelData(channel)(sentence).UnWeightedPeakLevel.Value.ToString(InvariantCulture))
                    Else
                        OutputList.Add(DefaultNotMeasuredValue)
                    End If

                    If ChannelData(channel)(sentence).WeightedLevel IsNot Nothing Then
                        OutputList.Add(ChannelData(channel)(sentence).WeightedLevel.Value.ToString(InvariantCulture))
                    Else
                        OutputList.Add(DefaultNotMeasuredValue)
                    End If
                    OutputList.Add(ChannelData(channel)(sentence).GetFrequencyWeighting.ToString)
                    OutputList.Add(ChannelData(channel)(sentence).GetTimeWeighting.ToString(InvariantCulture))

                    OutputList.Add(ChannelData(channel)(sentence).InitialPeak.ToString(InvariantCulture))
                    OutputList.Add(ChannelData(channel)(sentence).StartTime.ToString(InvariantCulture))

                    For word = 0 To ChannelData(channel)(sentence).Count - 1

                        'writing word data
                        OutputList.Add(word)
                        OutputList.Add(ChannelData(channel)(sentence)(word).OrthographicForm)
                        OutputList.Add(ChannelData(channel)(sentence)(word).PhoneticForm)
                        OutputList.Add(ChannelData(channel)(sentence)(word).StartSample)
                        OutputList.Add(ChannelData(channel)(sentence)(word).Length)

                        If ChannelData(channel)(sentence)(word).UnWeightedLevel IsNot Nothing Then
                            OutputList.Add(ChannelData(channel)(sentence)(word).UnWeightedLevel.Value.ToString(InvariantCulture))
                        Else
                            OutputList.Add(DefaultNotMeasuredValue)
                        End If

                        If ChannelData(channel)(sentence)(word).UnWeightedPeakLevel IsNot Nothing Then
                            OutputList.Add(ChannelData(channel)(sentence)(word).UnWeightedPeakLevel.Value.ToString(InvariantCulture))
                        Else
                            OutputList.Add(DefaultNotMeasuredValue)
                        End If

                        If ChannelData(channel)(sentence)(word).WeightedLevel IsNot Nothing Then
                            OutputList.Add(ChannelData(channel)(sentence)(word).WeightedLevel.Value.ToString(InvariantCulture))
                        Else
                            OutputList.Add(DefaultNotMeasuredValue)
                        End If
                        OutputList.Add(ChannelData(channel)(sentence)(word).GetFrequencyWeighting.ToString)
                        OutputList.Add(ChannelData(channel)(sentence)(word).GetTimeWeighting.ToString(InvariantCulture))

                        OutputList.Add(ChannelData(channel)(sentence)(word).StartTime.ToString(InvariantCulture))

                        For phone = 0 To ChannelData(channel)(sentence)(word).PhoneData.Count - 1

                            'writing phone data
                            OutputList.Add(phone)
                            OutputList.Add(ChannelData(channel)(sentence)(word).PhoneData(phone).OrthographicForm)
                            OutputList.Add(ChannelData(channel)(sentence)(word).PhoneData(phone).PhoneticForm)
                            OutputList.Add(ChannelData(channel)(sentence)(word).PhoneData(phone).StartSample)
                            OutputList.Add(ChannelData(channel)(sentence)(word).PhoneData(phone).Length)

                            If ChannelData(channel)(sentence)(word).PhoneData(phone).UnWeightedLevel IsNot Nothing Then
                                OutputList.Add(ChannelData(channel)(sentence)(word).PhoneData(phone).UnWeightedLevel.Value.ToString(InvariantCulture))
                            Else
                                OutputList.Add(DefaultNotMeasuredValue)
                            End If

                            If ChannelData(channel)(sentence)(word).PhoneData(phone).UnWeightedPeakLevel IsNot Nothing Then
                                OutputList.Add(ChannelData(channel)(sentence)(word).PhoneData(phone).UnWeightedPeakLevel.Value.ToString(InvariantCulture))
                            Else
                                OutputList.Add(DefaultNotMeasuredValue)
                            End If

                            If ChannelData(channel)(sentence)(word).PhoneData(phone).WeightedLevel IsNot Nothing Then
                                OutputList.Add(ChannelData(channel)(sentence)(word).PhoneData(phone).WeightedLevel.Value.ToString(InvariantCulture))
                            Else
                                OutputList.Add(DefaultNotMeasuredValue)
                            End If
                            OutputList.Add(ChannelData(channel)(sentence)(word).PhoneData(phone).FrequencyWeighting.ToString)
                            OutputList.Add(ChannelData(channel)(sentence)(word).PhoneData(phone).TimeWeighting.ToString(InvariantCulture))

                            'End of phone
                        Next
                        'End of word
                    Next
                    'End of sentence
                Next
                'End of channel
            Next

            Return String.Join(vbTab, OutputList)

        End Function

        Public Function CreateCopy() As SpeechMaterialAnnotation

            'Creating an output object
            Dim newSmaData As SpeechMaterialAnnotation

            'Serializing to memorystream
            Dim serializedMe As New MemoryStream
            Dim serializer As New BinaryFormatter
            serializer.Serialize(serializedMe, Me)

            'Deserializing to new object
            serializedMe.Position = 0
            newSmaData = CType(serializer.Deserialize(serializedMe), SpeechMaterialAnnotation)
            serializedMe.Close()

            'Returning the new object
            Return newSmaData
        End Function


        ''' <summary>
        ''' Converts all instances of a specified phone to a new phone
        ''' </summary>
        ''' <param name="CurrentPhone"></param>
        ''' <param name="NewPhone"></param>
        Public Sub ConvertPhone(ByVal CurrentPhone As String, ByVal NewPhone As String)
            For Each Channel In Me._ChannelData
                For Each Sentence In Channel
                    For Each Word In Sentence

                        Word.PhoneticForm = Word.PhoneticForm.Replace(CurrentPhone, NewPhone)

                        For Each Phone In Word.PhoneData
                            Phone.PhoneticForm = Phone.PhoneticForm.Replace(CurrentPhone, NewPhone)
                        Next
                    Next
                Next
            Next
        End Sub


        ''' <summary>
        ''' Measures sound levels for each channel, sentence, word and phone of the current SMA object.
        ''' </summary>
        ''' <returns>Returns True if all measurements were successful, and False if one or more measurements failed.</returns>
        Public Function MeasureSoundLevels(Optional ByVal LogMeasurementResults As Boolean = False, Optional ByVal LogFolder As String = "") As Boolean

            If ParentSound Is Nothing Then
                Throw New Exception("The parent sound if the current instance of SpeechMaterialAnnotation cannot be Nothing!")
            End If

            Dim SuccesfullMeasurementsCount As Integer = 0
            Dim AttemptedMeasurementCount As Integer = 0

            'Measuring each channel 
            For c As Integer = 1 To ChannelCount

                'Measuring on channel level 

                'Skips to next if the parent sound does not have enough channels
                If ParentSound.WaveFormat.Channels < c Then Continue For

                'Skips to next if the current channel of the parent sound does not contain any sound
                If ParentSound.WaveData.SampleData(c).Length = 0 Then Continue For

                'Measuring UnWeightedLevel
                ChannelData(c).UnWeightedLevel = Nothing
                ChannelData(c).UnWeightedLevel = DSP.MeasureSectionLevel(ParentSound, c, 0, Nothing, SoundDataUnit.dB)
                AttemptedMeasurementCount += 1
                If ChannelData(c).UnWeightedLevel IsNot Nothing Then SuccesfullMeasurementsCount += 1

                'Meaures UnWeightedPeakLevel
                ChannelData(c).UnWeightedPeakLevel = Nothing
                ChannelData(c).UnWeightedPeakLevel = DSP.MeasureSectionLevel(ParentSound, c, 0, , SoundDataUnit.dB, SoundMeasurementType.AbsolutePeakAmplitude)
                AttemptedMeasurementCount += 1
                If ChannelData(c).UnWeightedPeakLevel IsNot Nothing Then SuccesfullMeasurementsCount += 1

                'Measures weighted level
                ChannelData(c).WeightedLevel = Nothing
                If ChannelData(c).GetTimeWeighting <> 0 Then
                    ChannelData(c).WeightedLevel = Audio.DSP.GetLevelOfLoudestWindow(ParentSound, c,
                                                                                 ChannelData(c).GetTimeWeighting * ParentSound.WaveFormat.SampleRate,
                                                                                  0, Nothing, , ChannelData(c).GetFrequencyWeighting, True)
                Else
                    ChannelData(c).WeightedLevel = DSP.MeasureSectionLevel(ParentSound, c, 0, Nothing, SoundDataUnit.dB, SoundMeasurementType.RMS, ChannelData(c).GetFrequencyWeighting)
                End If
                AttemptedMeasurementCount += 1
                If ChannelData(c).WeightedLevel IsNot Nothing Then SuccesfullMeasurementsCount += 1


                'Measuring each sentence 
                For s = 0 To ChannelData(c).Count - 1

                    'Measuring on sentence level

                    'Measuring UnWeightedLevel
                    ChannelData(c)(s).UnWeightedLevel = Nothing
                    ChannelData(c)(s).UnWeightedLevel = DSP.MeasureSectionLevel(ParentSound, c, ChannelData(c)(s).StartSample, ChannelData(c)(s).Length, SoundDataUnit.dB)
                    AttemptedMeasurementCount += 1
                    If ChannelData(c)(s).UnWeightedLevel IsNot Nothing Then SuccesfullMeasurementsCount += 1

                    'Meaures UnWeightedPeakLevel
                    ChannelData(c)(s).UnWeightedPeakLevel = Nothing
                    ChannelData(c)(s).UnWeightedPeakLevel = DSP.MeasureSectionLevel(ParentSound, c, ChannelData(c)(s).StartSample, ChannelData(c)(s).Length, SoundDataUnit.dB, SoundMeasurementType.AbsolutePeakAmplitude)
                    AttemptedMeasurementCount += 1
                    If ChannelData(c)(s).UnWeightedPeakLevel IsNot Nothing Then SuccesfullMeasurementsCount += 1

                    'Measures weighted level
                    ChannelData(c)(s).WeightedLevel = Nothing
                    If ChannelData(c)(s).GetTimeWeighting <> 0 Then
                        ChannelData(c)(s).WeightedLevel = Audio.DSP.GetLevelOfLoudestWindow(ParentSound, c,
                                                                                 ChannelData(c)(s).GetTimeWeighting * ParentSound.WaveFormat.SampleRate,
                                                                                  ChannelData(c)(s).StartSample, ChannelData(c)(s).Length, , ChannelData(c)(s).GetFrequencyWeighting, True)
                    Else
                        ChannelData(c)(s).WeightedLevel = DSP.MeasureSectionLevel(ParentSound, c, ChannelData(c)(s).StartSample, ChannelData(c)(s).Length, SoundDataUnit.dB, SoundMeasurementType.RMS, ChannelData(c)(s).GetFrequencyWeighting)
                    End If
                    AttemptedMeasurementCount += 1
                    If ChannelData(c)(s).WeightedLevel IsNot Nothing Then SuccesfullMeasurementsCount += 1


                    'Measuring each word
                    For w = 0 To ChannelData(c)(s).Count - 1

                        'Measuring on word level

                        'Measuring UnWeightedLevel
                        ChannelData(c)(s)(w).UnWeightedLevel = Nothing
                        ChannelData(c)(s)(w).UnWeightedLevel = DSP.MeasureSectionLevel(ParentSound, c, ChannelData(c)(s)(w).StartSample, ChannelData(c)(s)(w).Length, SoundDataUnit.dB)
                        AttemptedMeasurementCount += 1
                        If ChannelData(c)(s)(w).UnWeightedLevel IsNot Nothing Then SuccesfullMeasurementsCount += 1

                        'Meaures UnWeightedPeakLevel
                        ChannelData(c)(s)(w).UnWeightedPeakLevel = Nothing
                        ChannelData(c)(s)(w).UnWeightedPeakLevel = DSP.MeasureSectionLevel(ParentSound, c, ChannelData(c)(s)(w).StartSample, ChannelData(c)(s)(w).Length, SoundDataUnit.dB, SoundMeasurementType.AbsolutePeakAmplitude)
                        AttemptedMeasurementCount += 1
                        If ChannelData(c)(s)(w).UnWeightedPeakLevel IsNot Nothing Then SuccesfullMeasurementsCount += 1

                        'Measures weighted level
                        ChannelData(c)(s)(w).WeightedLevel = Nothing
                        If ChannelData(c)(s)(w).GetTimeWeighting <> 0 Then
                            ChannelData(c)(s)(w).WeightedLevel = Audio.DSP.GetLevelOfLoudestWindow(ParentSound, c,
                                                                                 ChannelData(c)(s)(w).GetTimeWeighting * ParentSound.WaveFormat.SampleRate,
                                                                                  ChannelData(c)(s)(w).StartSample, ChannelData(c)(s)(w).Length, , ChannelData(c)(s)(w).GetFrequencyWeighting, True)
                        Else
                            ChannelData(c)(s)(w).WeightedLevel = DSP.MeasureSectionLevel(ParentSound, c, ChannelData(c)(s)(w).StartSample, ChannelData(c)(s)(w).Length, SoundDataUnit.dB, SoundMeasurementType.RMS, ChannelData(c)(s)(w).GetFrequencyWeighting)
                        End If
                        AttemptedMeasurementCount += 1
                        If ChannelData(c)(s)(w).WeightedLevel IsNot Nothing Then SuccesfullMeasurementsCount += 1


                        'Measuring each phone
                        For p = 0 To ChannelData(c)(s)(w).PhoneData.Count - 1

                            'Measuring on phone level
                            If ChannelData(c)(s)(w).PhoneData(p).PhoneticForm = WordEndString Then
                                ChannelData(c)(s)(w).PhoneData(p).UnWeightedLevel = Nothing
                                ChannelData(c)(s)(w).PhoneData(p).UnWeightedPeakLevel = Nothing
                                ChannelData(c)(s)(w).PhoneData(p).WeightedLevel = Nothing
                                Continue For
                            End If

                            'Measuring UnWeightedLevel
                            ChannelData(c)(s)(w).PhoneData(p).UnWeightedLevel = Nothing
                            ChannelData(c)(s)(w).PhoneData(p).UnWeightedLevel = DSP.MeasureSectionLevel(ParentSound, c, ChannelData(c)(s)(w).PhoneData(p).StartSample, ChannelData(c)(s)(w).PhoneData(p).Length, SoundDataUnit.dB)
                            AttemptedMeasurementCount += 1
                            If ChannelData(c)(s)(w).PhoneData(p).UnWeightedLevel IsNot Nothing Then SuccesfullMeasurementsCount += 1

                            'Meaures UnWeightedPeakLevel
                            ChannelData(c)(s)(w).PhoneData(p).UnWeightedPeakLevel = Nothing
                            ChannelData(c)(s)(w).PhoneData(p).UnWeightedPeakLevel = DSP.MeasureSectionLevel(ParentSound, c, ChannelData(c)(s)(w).PhoneData(p).StartSample, ChannelData(c)(s)(w).PhoneData(p).Length, SoundDataUnit.dB, SoundMeasurementType.AbsolutePeakAmplitude)
                            AttemptedMeasurementCount += 1
                            If ChannelData(c)(s)(w).PhoneData(p).UnWeightedPeakLevel IsNot Nothing Then SuccesfullMeasurementsCount += 1

                            'Measures weighted level
                            ChannelData(c)(s)(w).PhoneData(p).WeightedLevel = Nothing
                            If ChannelData(c)(s)(w).PhoneData(p).TimeWeighting <> 0 Then
                                ChannelData(c)(s)(w).PhoneData(p).WeightedLevel = Audio.DSP.GetLevelOfLoudestWindow(ParentSound, c,
                                                                                 ChannelData(c)(s)(w).PhoneData(p).TimeWeighting * ParentSound.WaveFormat.SampleRate,
                                                                                  ChannelData(c)(s)(w).PhoneData(p).StartSample, ChannelData(c)(s)(w).PhoneData(p).Length, , ChannelData(c)(s)(w).PhoneData(p).FrequencyWeighting, True)
                            Else
                                ChannelData(c)(s)(w).PhoneData(p).WeightedLevel = DSP.MeasureSectionLevel(ParentSound, c, ChannelData(c)(s)(w).PhoneData(p).StartSample, ChannelData(c)(s)(w).PhoneData(p).Length, SoundDataUnit.dB, SoundMeasurementType.RMS, ChannelData(c)(s)(w).PhoneData(p).FrequencyWeighting)
                            End If
                            AttemptedMeasurementCount += 1
                            If ChannelData(c)(s)(w).PhoneData(p).WeightedLevel IsNot Nothing Then SuccesfullMeasurementsCount += 1

                        Next
                    Next
                Next
            Next


            'Logging results
            If LogMeasurementResults = True Then
                SendInfoToAudioLog(vbCrLf &
                                   "FileName" & vbTab & ParentSound.FileName & vbTab &
                                   "FailedMeasurementCount: " & vbTab & AttemptedMeasurementCount - SuccesfullMeasurementsCount & vbTab &
                                   ToString(True), "SoundMeasurementLog.txt", LogFolder)
            End If

            'Checking if all attempted measurements were succesful
            If AttemptedMeasurementCount = SuccesfullMeasurementsCount Then
                Return True
            Else
                Return False
            End If

        End Function


        ''' <summary>
        ''' Resets the sound levels for each channel, sentence, word and phone of the current SMA object.
        ''' </summary>
        Public Sub ResetSoundLevels()

            'Resetting each channel 
            For c As Integer = 1 To ChannelCount

                ChannelData(c).UnWeightedLevel = Nothing
                ChannelData(c).UnWeightedPeakLevel = Nothing
                ChannelData(c).WeightedLevel = Nothing

                'Resetting each sentence 
                For s = 0 To ChannelData(c).Count - 1

                    ChannelData(c)(s).UnWeightedLevel = Nothing
                    ChannelData(c)(s).UnWeightedPeakLevel = Nothing
                    ChannelData(c)(s).WeightedLevel = Nothing

                    'Resetting each word
                    For w = 0 To ChannelData(c)(s).Count - 1

                        ChannelData(c)(s)(w).UnWeightedLevel = Nothing
                        ChannelData(c)(s)(w).UnWeightedPeakLevel = Nothing
                        ChannelData(c)(s)(w).WeightedLevel = Nothing

                        'Resetting each phone
                        For p = 0 To ChannelData(c)(s)(w).PhoneData.Count - 1

                            ChannelData(c)(s)(w).PhoneData(p).UnWeightedLevel = Nothing
                            ChannelData(c)(s)(w).PhoneData(p).UnWeightedPeakLevel = Nothing
                            ChannelData(c)(s)(w).PhoneData(p).WeightedLevel = Nothing
                        Next
                    Next
                Next
            Next

        End Sub


        <Serializable>
        Partial Public Class SmaChannelData
            Inherits List(Of SmaSentenceData)

            Private Property ParentSMA As SpeechMaterialAnnotation

            Public Property OrthographicForm As String = ""
            Public Property PhoneticForm As String = ""

            Public Property StartSample As Integer = 0

            Private _Length As Integer = 0
            Public Property Length As Integer
                Get
                    'Updates the length based on the audio channel
                    UpdateLength()
                    Return _Length
                End Get
                Set(value As Integer)
                    _Length = value
                End Set
            End Property

            ''' <summary>
            ''' Updates the length of the audio channel
            ''' </summary>
            ''' <returns></returns>
            Private Function UpdateLength() As Boolean
                If ParentSMA.ParentSound IsNot Nothing Then
                    Length = ParentSMA.ParentSound.WaveData.SampleData(GetParentAudioChannel).Length
                    Return True
                Else
                    Return False
                End If
            End Function

            Private Function GetParentAudioChannel() As Integer
                If ParentSMA IsNot Nothing Then
                    Dim ChannelIndex As Integer = 0
                    For c = 1 To ParentSMA.ChannelCount
                        If ParentSMA.ChannelData(c) Is Me Then ChannelIndex = c
                    Next
                    Return ChannelIndex
                Else
                    Throw New Exception("No parent SMA exists. Cannot retrieve the current SMA channel parent audio channel.")
                End If
            End Function

            Public Property UnWeightedLevel As Double? = Nothing
            Public Property UnWeightedPeakLevel As Double? = Nothing

            Public Property WeightedLevel As Double? = Nothing

            Private _FrequencyWeighting As FrequencyWeightings = FrequencyWeightings.Z
            Public Function GetFrequencyWeighting() As FrequencyWeightings
                Return _FrequencyWeighting
            End Function

            Public Sub SetFrequencyWeighting(ByVal FrequencyWeighting As FrequencyWeightings, ByVal EnforceOnAllDescendents As Boolean)

                'Setting only SMA top level frequency weighting
                _FrequencyWeighting = FrequencyWeighting

                If EnforceOnAllDescendents = True Then

                    'Enforcing the same frequency weighting on all descendant sentences, words, and phones
                    For s = 0 To Me.Count - 1
                        Me(s).SetFrequencyWeighting(FrequencyWeighting, False)
                        For w = 0 To Me(s).Count - 1
                            Me(s)(w).SetFrequencyWeighting(FrequencyWeighting, False)
                            For p = 0 To Me(s)(w).PhoneData.Count - 1
                                Me(s)(w).PhoneData(p).FrequencyWeighting = FrequencyWeighting
                            Next
                        Next
                    Next
                End If
            End Sub

            Private _TimeWeighting As Double = 0 'A time weighting of 0 indicates "no time weighting", thus the average RMS level is indicated.
            Public Function GetTimeWeighting() As Double
                Return _TimeWeighting
            End Function

            Public Sub SetTimeWeighting(ByVal TimeWeighting As Double, ByVal EnforceOnAllDescendents As Boolean)

                'Setting only SMA top level Time weighting
                _TimeWeighting = TimeWeighting

                If EnforceOnAllDescendents = True Then

                    'Enforcing the same Time weighting on all descendant sentences, words, and phones
                    For s = 0 To Me.Count - 1
                        Me(s).SetTimeWeighting(TimeWeighting, False)
                        For w = 0 To Me(s).Count - 1
                            Me(s)(w).SetTimeWeighting(TimeWeighting, False)
                            For p = 0 To Me(s)(w).PhoneData.Count - 1
                                Me(s)(w).PhoneData(p).TimeWeighting = TimeWeighting
                            Next
                        Next
                    Next
                End If
            End Sub

            Public Sub New(ByRef ParentSMA As SpeechMaterialAnnotation)
                Me.ParentSMA = ParentSMA
                Me.SetFrequencyWeighting(ParentSMA.GetFrequencyWeighting, True)
                Me.SetTimeWeighting(ParentSMA.GetTimeWeighting, True)
            End Sub

        End Class

        <Serializable>
        Partial Public Class SmaSentenceData
            Inherits List(Of SmaWordData)

            Private Property ParentSMA As SpeechMaterialAnnotation

            Public Property OrthographicForm As String = ""
            Public Property PhoneticForm As String = ""

            Public Property StartSample As Integer = -1
            Public Property Length As Integer = 0
            Public Property UnWeightedLevel As Double? = Nothing
            Public Property UnWeightedPeakLevel As Double? = Nothing

            Public Property WeightedLevel As Double? = Nothing

            Private _FrequencyWeighting As FrequencyWeightings = FrequencyWeightings.Z
            Public Function GetFrequencyWeighting() As FrequencyWeightings
                Return _FrequencyWeighting
            End Function

            Public Sub SetFrequencyWeighting(ByVal FrequencyWeighting As FrequencyWeightings, ByVal EnforceOnAllDescendents As Boolean)

                'Setting only SMA top level frequency weighting
                _FrequencyWeighting = FrequencyWeighting

                If EnforceOnAllDescendents = True Then

                    'Enforcing the same frequency weighting on all descendant words, and phones
                    For w = 0 To Me.Count - 1
                        Me(w).SetFrequencyWeighting(FrequencyWeighting, False)
                        For p = 0 To Me(w).PhoneData.Count - 1
                            Me(w).PhoneData(p).FrequencyWeighting = FrequencyWeighting
                        Next
                    Next
                End If
            End Sub

            Private _TimeWeighting As Double = 0 'A time weighting of 0 indicates "no time weighting", thus the average RMS level is indicated.
            Public Function GetTimeWeighting() As Double
                Return _TimeWeighting
            End Function

            Public Sub SetTimeWeighting(ByVal TimeWeighting As Double, ByVal EnforceOnAllDescendents As Boolean)

                'Setting only SMA top level Time weighting
                _TimeWeighting = TimeWeighting

                If EnforceOnAllDescendents = True Then

                    'Enforcing the same Time weighting on all descendant words, and phones
                    For w = 0 To Me.Count - 1
                        Me(w).SetTimeWeighting(TimeWeighting, False)
                        For p = 0 To Me(w).PhoneData.Count - 1
                            Me(w).PhoneData(p).TimeWeighting = TimeWeighting
                        Next
                    Next
                End If
            End Sub

            ''' <summary>
            ''' The initial peak amplitude value is the absulote value of the highest detected sample value within the word parts of a segmented audio recording, before any gain has been applied to the audio segment. 
            ''' Its default value is -1. Before any gain is applied to such a segment the InitialPeak should be measured using the method SetInitialPeakAmplitude. And once it has been measured (i.e. has a value other than -1), it should not be measured again. 
            ''' </summary>
            ''' <returns></returns>
            Public Property InitialPeak As Double = -1
            Public Property StartTime As Double

            Public Sub New(ByRef ParentSMA As SpeechMaterialAnnotation)
                Me.ParentSMA = ParentSMA
                Me.SetFrequencyWeighting(ParentSMA.GetFrequencyWeighting, True)
                Me.SetTimeWeighting(ParentSMA.GetTimeWeighting, True)
            End Sub

        End Class


        ''' <summary>
        '''Is used to store data for segmentation and location of recorded words and phones. 
        '''Can also perform sound measurements on the indicated words and phones, as soon as both start sample and length is assigned
        ''' </summary>
        <Serializable>
        Public Class SmaWordData

            Private Property ParentSMA As SpeechMaterialAnnotation

            Public Property OrthographicForm As String = ""
            Public Property PhoneticForm As String = ""

            Public Property StartSample As Integer = -1
            Public Property Length As Integer = 0

            'Sound level properties. vbNull if never set
            Public Property UnWeightedLevel As Double? = Nothing
            Public Property UnWeightedPeakLevel As Double? = Nothing

            Public Property WeightedLevel As Double? = Nothing

            Private _FrequencyWeighting As FrequencyWeightings = FrequencyWeightings.Z
            Public Function GetFrequencyWeighting() As FrequencyWeightings
                Return _FrequencyWeighting
            End Function

            Public Sub SetFrequencyWeighting(ByVal FrequencyWeighting As FrequencyWeightings, ByVal EnforceOnAllDescendents As Boolean)

                'Setting only SMA top level frequency weighting
                _FrequencyWeighting = FrequencyWeighting

                If EnforceOnAllDescendents = True Then

                    'Enforcing the same frequency weighting on all descendant phones
                    For p = 0 To PhoneData.Count - 1
                        PhoneData(p).FrequencyWeighting = FrequencyWeighting
                    Next
                End If
            End Sub

            Private _TimeWeighting As Double = 0 'A time weighting of 0 indicates "no time weighting", thus the average RMS level is indicated.
            Public Function GetTimeWeighting() As Double
                Return _TimeWeighting
            End Function

            Public Sub SetTimeWeighting(ByVal TimeWeighting As Double, ByVal EnforceOnAllDescendents As Boolean)

                'Setting only SMA top level Time weighting
                _TimeWeighting = TimeWeighting

                If EnforceOnAllDescendents = True Then

                    'Enforcing the same Time weighting on all descendant phones
                    For p = 0 To PhoneData.Count - 1
                        PhoneData(p).TimeWeighting = TimeWeighting
                    Next
                End If
            End Sub

            'Word start time  
            Public Property StartTime As Double

            'Phone data
            Public Property PhoneData As New List(Of SmaPhoneData)

            Public Sub New(ByRef ParentSMA As SpeechMaterialAnnotation)
                Me.ParentSMA = ParentSMA
                Me.SetFrequencyWeighting(ParentSMA.GetFrequencyWeighting, True)
                Me.SetTimeWeighting(ParentSMA.GetTimeWeighting, True)
            End Sub

        End Class

        ''' <summary>
        ''' Is used to store data for segmentation and location of recorded phones. 
        ''' Can also perform sound measurements On the indicated sound.
        ''' </summary>
        <Serializable>
        Public Class SmaPhoneData

            Private Property ParentSMA As SpeechMaterialAnnotation

            Public Property OrthographicForm As String = ""
            Public Property PhoneticForm As String = ""

            Public Property StartSample As Integer = -1
            Public Property Length As Integer = 0

            'Sound level properties. vbNull if never set
            Public Property UnWeightedLevel As Double? = Nothing
            Public Property UnWeightedPeakLevel As Double? = Nothing

            Public Property FrequencyWeighting As FrequencyWeightings = FrequencyWeightings.Z
            Public Property TimeWeighting As Double = 0 'A time weighting of 0 indicates "no time weighting", thus the average RMS level is indicated.
            Public Property WeightedLevel As Double? = Nothing

            Public Sub New(ByRef ParentSMA As SpeechMaterialAnnotation)
                Me.ParentSMA = ParentSMA
                Me.FrequencyWeighting = ParentSMA.GetFrequencyWeighting
                Me.TimeWeighting = ParentSMA.GetTimeWeighting
            End Sub

        End Class


        Public Enum MeasurementSections
            CarrierPhrases
            TestWords
            CarriersAndTestWords
        End Enum

    End Class

#Region "IO"


    ''' <summary>
    ''' Reads a sound (.wav or .ptwf) from file and stores it in a new Sounds object.
    ''' </summary>
    ''' <param name="filePath">The file path to the file to read. If left empty a open file dialogue box will appear.</param>
    ''' <param name="startReadTime"></param>
    ''' <param name="stopReadTime"></param>
    ''' <param name="inputTimeFormat"></param>
    ''' <param name="StoreSourcePath">If set to True, the full path of the audio file from which the Sound object was created is stored in Sound.SourcePath.</param>
    ''' <returns>Returns a new Sound containing the sound data from the input sound file.</returns>
    Public Shared Function LoadWaveFile(ByVal filePath As String,
                                        Optional ByVal startReadTime As Decimal = 0,
                                        Optional ByVal stopReadTime As Decimal = 0,
                                        Optional ByVal inputTimeFormat As TimeUnits = TimeUnits.seconds,
                                        Optional ByVal StoreSourcePath As Boolean = False) As Audio.Sound

        Try

            Dim fileName As String = ""

            'Finds out the filename
            If Not filePath = "" Then fileName = Path.GetFileNameWithoutExtension(filePath)

            'Creates a variable to hold data chunk size
            Dim dataSize As UInteger = 0

            Dim fileStreamRead As FileStream = New FileStream(filePath, FileMode.Open)
            Dim reader As BinaryReader = New BinaryReader(fileStreamRead, Text.Encoding.UTF8)

            'Starts reading

            Dim chunkID As String = reader.ReadChars(4)
            Dim fileSize As UInteger = reader.ReadUInt32
            Dim riffType As String = reader.ReadChars(4)
            'Abort if riffType is not WAVE
            If Not riffType = "WAVE" Then
                Throw New Exception("The file is not a wave-file!")
            End If

            Dim fmtID As String
            Dim fmtSize As UInteger
            Dim fmtCode As UShort
            Dim channels As UShort
            Dim sampleRate As UInteger
            Dim fmtAvgBPS As UInteger
            Dim fmtBlockAlign As UShort
            Dim bitDepth As UShort

            Dim sound As Audio.Sound = Nothing
            Dim FormatChunkIsRead As Boolean = False 'THis variable is used to ensure that the format chunk is read before the ptwf and the data chunks.

            'Chunks to ignore
            Dim dataChunkFound As Boolean
            While dataChunkFound = False

                Dim IDOfNextChunk As String = reader.ReadChars(4)
                Dim sizeOfNextChunk As UInteger = reader.ReadUInt32
                Select Case IDOfNextChunk

                    Case "fmt "

                        Dim fmtChunkStartPosition As Integer = reader.BaseStream.Position

                        ' Reading the format chunk (not all data is stored)
                        fmtID = IDOfNextChunk ' reader.ReadChars(4)
                        fmtSize = sizeOfNextChunk ' reader.ReadUInt32
                        fmtCode = reader.ReadUInt16
                        channels = reader.ReadUInt16
                        sampleRate = reader.ReadUInt32
                        fmtAvgBPS = reader.ReadUInt32
                        fmtBlockAlign = reader.ReadUInt16
                        bitDepth = reader.ReadUInt16

                        sound = New Audio.Sound(New Audio.Formats.WaveFormat(sampleRate, bitDepth, channels,, fmtCode))

                        'Stores the source file path
                        If StoreSourcePath = True Then
                            sound.SourcePath = filePath
                        End If

                        'Checks to see if the whole of subchunk1 has been read
                        While reader.BaseStream.Position < fmtChunkStartPosition + fmtSize
                            reader.ReadByte()
                        End While

                        'Noting that the format chunk is read
                        FormatChunkIsRead = True


                    Case "iXML"

                        Dim iXMLDataStartReadPosition As Integer = reader.BaseStream.Position

                        'Copying iXML data to a new stream
                        Dim iXMLStream As New MemoryStream
                        For s = 0 To sizeOfNextChunk - 1
                            iXMLStream.WriteByte(fileStreamRead.ReadByte)
                        Next
                        iXMLStream.Position = 0

                        'Parsing the iXML data
                        Dim iXMLdata = ParseiXMLString(iXMLStream)

                        'Storing the data
                        If iXMLdata.Item1 IsNot Nothing Then
                            sound.SMA = iXMLdata.Item1
                        End If
                        If iXMLdata.Item2 IsNot Nothing Then
                            sound.iXmlNodes = iXMLdata.Item2
                        End If

                        'Checks if a padding byte needs to be read
                        Dim currentBaseStreamPosition As Integer = reader.BaseStream.Position
                        If Not currentBaseStreamPosition Mod 2 = 0 Then
                            reader.ReadByte()
                        End If

                    Case "data"

                        'Aborting if the format chink has not yet been read
                        If FormatChunkIsRead = False Then
                            AudioError("The wave file has an unsupported internal structure.")
                            Return Nothing
                        End If

                        dataChunkFound = True
                        dataSize = sizeOfNextChunk

                    Case Else
                        Dim SizeOfUnknownChunk As UInteger = sizeOfNextChunk

                        'Reads to the end of the chunk but does not save the data
                        'Stores the unknown chunk so that it can be retained upon save
                        sound.UnparsedWaveChunks.Add(reader.ReadBytes(SizeOfUnknownChunk))

                        'Reads any padding bytes
                        If SizeOfUnknownChunk Mod 2 = 1 Then
                            reader.ReadByte()
                        End If

                End Select

            End While



            Dim startReadDataPoint As Integer
            Dim stopReadDataPoint As Integer

            Select Case inputTimeFormat
                Case TimeUnits.seconds
                    startReadDataPoint = startReadTime * sound.WaveFormat.SampleRate * sound.WaveFormat.Channels
                    stopReadDataPoint = stopReadTime * sound.WaveFormat.SampleRate * sound.WaveFormat.Channels

                Case TimeUnits.samples
                    startReadDataPoint = startReadTime * sound.WaveFormat.Channels
                    stopReadDataPoint = stopReadTime * sound.WaveFormat.Channels

            End Select

            Dim soundIndexOfDataPoints As Integer = dataSize / (sound.WaveFormat.BitDepth / 8)

            If stopReadTime = 0 Then
                stopReadDataPoint = soundIndexOfDataPoints - 1
            End If

            If stopReadDataPoint > soundIndexOfDataPoints Then
                stopReadDataPoint = soundIndexOfDataPoints - 1
            End If

            Dim numberOfDataPointsToRead As Integer = stopReadDataPoint + 1 - startReadDataPoint
            Dim soundDataArray(numberOfDataPointsToRead - 1) As Double

            If numberOfDataPointsToRead > 0 Then
                Select Case sound.WaveFormat.Encoding
                    Case = Audio.Formats.WaveFormat.WaveFormatEncodings.PCM
                        Select Case sound.WaveFormat.BitDepth
                            Case 16
                                For n = 0 To startReadDataPoint - 1
                                    reader.ReadInt16()
                                Next
                                For n = startReadDataPoint To stopReadDataPoint '- 1?
                                    soundDataArray(n - startReadDataPoint) = reader.ReadInt16()
                                    'MsgBox("Reading" & n - startReadDataPoint & " " & soundDataArray(n - startReadDataPoint))
                                Next

                            Case 32
                                For n = 0 To startReadDataPoint - 1
                                    reader.ReadInt32()
                                Next
                                For n = startReadDataPoint To stopReadDataPoint '- 1?
                                    soundDataArray(n - startReadDataPoint) = reader.ReadInt32()
                                    'MsgBox("Reading" & n - startReadDataPoint & " " & soundDataArray(n - startReadDataPoint))
                                Next

                            Case Else
                                Throw New NotImplementedException("Reading of " & sound.WaveFormat.BitDepth & " bits PCM format is not yet supported.")
                        End Select
                    Case = Audio.Formats.WaveFormat.WaveFormatEncodings.IeeeFloatingPoints
                        Select Case sound.WaveFormat.BitDepth
                            Case 32
                                For n = 0 To startReadDataPoint - 1
                                    reader.ReadSingle()
                                Next
                                For n = startReadDataPoint To stopReadDataPoint '- 1?
                                    soundDataArray(n - startReadDataPoint) = reader.ReadSingle()
                                    'MsgBox("Reading" & n - startReadDataPoint & " " & soundDataArray(n - startReadDataPoint))
                                Next
                            Case Else
                                Throw New NotImplementedException("Reading of " & sound.WaveFormat.BitDepth & " bits IEEE floating points format is not yet supported.")
                        End Select
                End Select

            Else
                If numberOfDataPointsToRead < 0 Then Throw New Exception("The number of data points to read was below zero.")
            End If

            fileStreamRead.Close()

            'Dim  As Integer = sound.waveFormat.channels
            If Not numberOfDataPointsToRead Mod channels = 0 Then Throw New Exception("ReadWaveFile detected unequal number of samples between the channels.")
            Dim numberofDataPointsIneachChannelarray = (numberOfDataPointsToRead / channels)

            For c = 1 To channels
                Dim channelData((numberofDataPointsIneachChannelarray) - 1) As Single

                If numberOfDataPointsToRead > channels Then
                    Dim counter As Integer = 0
                    For n = c - 1 To soundDataArray.Length - 1 Step channels
                        channelData(counter) = soundDataArray(n)
                        'MsgBox("Sorting channel " & c & counter & " " & channelData(counter))
                        counter += 1
                    Next
                Else
                    If numberOfDataPointsToRead < 0 Then Throw New Exception("The number of data points to read was below zero.")
                End If

                sound.WaveData.SampleData(c) = channelData

            Next

            'Adding the input file name
            sound.FileName = fileName

            Return sound

        Catch ex As Exception
            AudioError(ex.ToString)
            Return Nothing
        End Try

    End Function

    ''' <summary>
    ''' Saves the current instance of Sound to a wave file.
    ''' </summary>
    ''' <param name="filePath">The filepath where the file should be saved. If left empty a windows forms save file dialogue box will appaear,
    ''' in which the user may enter file name and storing location.</param>
    ''' <param name="startSample">This parameter enables saving of only a part of the file. StartSample indicates the first sample to be saved.</param>
    ''' <param name="length">This parameter enables saving of only a part of the file. Length indicates the length in samples of the file to be saved.</param>
    ''' <returns>Returns true if save succeded, and False if save failed.</returns>
    Public Function WriteWaveFile(Optional ByRef filePath As String = "",
                                   Optional ByVal startSample As Integer = Nothing, Optional ByVal length As Integer? = Nothing,
                                   Optional CreatePath As Boolean = True) As Boolean

        'Ensures that the extension is .wav
        Dim FileExtension As String = Path.GetExtension(filePath)
        If FileExtension <> ".wav" Then
            filePath = filePath.TrimEnd(FileExtension)
            filePath &= ".wav"
        End If

        'Writes the wave data to a memory stream
        Dim WaveStream = WriteWaveFileStream(startSample, length)

        If WaveStream IsNot Nothing Then
            WaveStream.Position = 0
            Try

                'Creating the path if it doesn't exist
                If CreatePath = True Then
                    Dim TempDirectory As String = IO.Path.GetDirectoryName(filePath)
                    If Not IO.Directory.Exists(TempDirectory) Then IO.Directory.CreateDirectory(TempDirectory)
                End If

                'Saves the file
                Dim theFile As FileStream = File.Create(filePath)
                WaveStream.WriteTo(theFile)
                theFile.Close()

                Return True
            Catch ex As Exception
                AudioError("Error saving to wave file:" & vbCrLf & ex.ToString)
                Return False
            End Try
        Else
            AudioError("Error saving to wave file.")
            Return False
        End If

    End Function


    ''' <summary>
    ''' Writes the current Sound to a MemoryStream structures as a RIFF WAVE file.
    ''' </summary>
    ''' <param name="startSample"></param>
    ''' <param name="length"></param>
    ''' <param name="checkForDistorsion"></param>
    ''' <returns></returns>
    Public Function WriteWaveFileStream(Optional ByVal startSample As Integer = Nothing, Optional ByVal length As Integer? = Nothing,
                                     Optional checkForDistorsion As Boolean = True) As MemoryStream

        Try

            'Checking that bit depth and encoding is supported before starting to write
            Select Case WaveFormat.Encoding
                Case Audio.Formats.WaveFormat.WaveFormatEncodings.PCM
                    Select Case WaveFormat.BitDepth
                        Case 8, 16
                        Case Else
                            Throw New NotImplementedException("Writing of " & WaveFormat.BitDepth & " bits PCM format is not yet supported.")
                    End Select
                Case Audio.Formats.WaveFormat.WaveFormatEncodings.IeeeFloatingPoints
                    Select Case WaveFormat.BitDepth
                        Case 32
                        Case Else
                            Throw New NotImplementedException("Writing of " & WaveFormat.BitDepth & " bits IEEE floating point format is not yet supported.")
                    End Select
                Case Else
                    Throw New NotImplementedException("Writing of the format " & WaveFormat.Encoding.ToString & " is not yet supported.")
            End Select

            'Enforcing equal channel length
            WaveData.EnforceEqualChannelLength()

            'And checking the values of startSample and length
            CheckAndCorrectSectionLength(WaveData.ShortestChannelSampleCount, startSample, length)

            'Writing the sound stream
            Dim WaveStream As New MemoryStream
            Dim writer As BinaryWriter = New BinaryWriter(WaveStream, Text.Encoding.UTF8)

            ' Write the header
            writer.Write(WaveFormat.MainChunkID.ToCharArray)
            Dim fileSize As UInteger = 0
            writer.Write(fileSize)
            writer.Write(WaveFormat.RiffType.ToCharArray)

            ' Write the format chunk
            writer.Write(WaveFormat.FmtID.ToCharArray)
            writer.Write(WaveFormat.FmtSize)
            writer.Write(WaveFormat.FmtCode)
            writer.Write(WaveFormat.Channels)
            writer.Write(WaveFormat.SampleRate)
            writer.Write(WaveFormat.FmtAvgBPS)
            writer.Write(WaveFormat.FmtBlockAlign)
            writer.Write(WaveFormat.BitDepth)


            'Writing iXML chunk
            If iXmlNodes IsNot Nothing Or SMA IsNot Nothing Then

                'Noting start position
                Dim iXMLChunkStartByte As Integer = writer.BaseStream.Position

                'Writes the chunk head
                writer.Write("iXML".ToCharArray)
                Dim TempSize As UInteger = 0
                writer.Write(TempSize)

                'Writes the iXML to the base stream
                If WriteiXmlToStream(WaveStream) = True Then

                    Dim iXMLChunkEndByte As Integer = writer.BaseStream.Position

                    'Writing chunk-final zero-padding
                    Dim currentBaseStreamPosition As Integer = writer.BaseStream.Position
                    If Not currentBaseStreamPosition Mod 2 = 0 Then
                        Dim zeroPad As Byte = 0
                        writer.Write(zeroPad)
                    End If

                    Dim iXMLChunkEndByte_InklPadding As Integer = writer.BaseStream.Position

                    'Seeking position for iXML chunk size, calculating iXML chunk size, and then writes it to the stream
                    writer.Seek(iXMLChunkStartByte + 4, SeekOrigin.Begin)
                    Dim iXmlChunkSize As UInteger = 0
                    iXmlChunkSize = iXMLChunkEndByte - iXMLChunkStartByte - 8  'According to the RIFF specification size should not include any zero-padding bytes. But since this does not open in some wave readers, the zero-pad bytes are included in the chunk size here.
                    writer.Write(iXmlChunkSize)

                    'Going back to the current position, to continue writing the next chunk
                    writer.Seek(iXMLChunkEndByte_InklPadding, SeekOrigin.Begin)

                Else
                    'If for some reason writing of iXML failed, the stream rolls back so that any data already written to the stream by WriteiXmlToStream can be overwritten
                    writer.Seek(iXMLChunkStartByte, SeekOrigin.Begin)

                End If
            End If

            'Adds any unparsed chunks that were read from wave file (they need to be completely unmodified for write to work!)
            If UnparsedWaveChunks IsNot Nothing Then
                For n = 0 To UnparsedWaveChunks.Count - 1

                    writer.Write(UnparsedWaveChunks(n))

                    'Writing chunk-final zero-padding
                    Dim currentBaseStreamPosition As Integer = writer.BaseStream.Position
                    If Not currentBaseStreamPosition Mod 2 = 0 Then
                        Dim zeroPad As Byte = 0
                        writer.Write(zeroPad)
                    End If

                Next
            End If

            ' Write the data chunk. Converts the data from Single to Short, and checks the sound data for distorsion
            writer.Write(WaveFormat.DataID.ToCharArray)

            'Writes data size
            If startSample = Nothing Then startSample = 0
            If length Is Nothing Then length = WaveData.ShortestChannelSampleCount
            If length > WaveData.ShortestChannelSampleCount Then length = WaveData.ShortestChannelSampleCount
            If startSample > WaveData.ShortestChannelSampleCount - 1 Then startSample = WaveData.ShortestChannelSampleCount - 1

            Dim numberOfDataPoints As Integer = length * WaveFormat.Channels
            Dim dataSize As UInteger = numberOfDataPoints * (WaveFormat.BitDepth / 8)
            writer.Write(dataSize)

            'Time measurement code
            'Dim startTime As DateTime = DateTime.Now

            'Prepares the data section, by creating an array with all channels interleaved
            Dim interleavedSoundArray() As Single = {}
            Try
                ReDim interleavedSoundArray(numberOfDataPoints - 1)
            Catch ex As Exception
                MsgBox(ex.ToString)
            End Try

            For c = 1 To WaveFormat.Channels

                '   If length <waveData.SampleCount Then

                'Dim channelCount As Integer = waveFormat.channels
                'Dim channelArray() As Single = waveData.sampleData(c)
                Dim counter As Integer = 0
                For n = startSample To startSample + length - 1
                    interleavedSoundArray(c - 1 + counter) = WaveData.SampleData(c)(n)
                    counter += WaveFormat.Channels
                Next

                'Else
                '    Dim counter As Integer = 0
                '    For Each dataPoint In waveData.sampleData(c)
                '    interleavedSoundArray(c - 1 + counter) = dataPoint
                '    counter += waveFormat.channels
                'Next
                'End If
            Next

            Dim distorsion As Boolean = False
            Dim distorsionSampleCount As Integer = 0

            If checkForDistorsion = False Then
                For Each dataPoint In interleavedSoundArray
                    writer.Write(dataPoint)
                Next
            Else

                Select Case WaveFormat.Encoding
                    Case Audio.Formats.WaveFormat.WaveFormatEncodings.PCM
                        Select Case WaveFormat.BitDepth
                            Case 8
                                Dim dataPointSByteValue As Byte
                                For Each dataPoint In interleavedSoundArray
                                    If dataPoint < Byte.MinValue Then
                                        dataPoint = Byte.MinValue
                                        distorsion = True
                                        distorsionSampleCount += 1
                                    End If

                                    If dataPoint > Byte.MaxValue Then
                                        dataPoint = Byte.MaxValue
                                        distorsion = True
                                        distorsionSampleCount += 1
                                    End If

                                    dataPointSByteValue = dataPoint
                                    writer.Write(dataPointSByteValue)
                                Next

                            Case 16

                                Dim dataPointShortValue As Short
                                For Each dataPoint In interleavedSoundArray

                                    If dataPoint < Short.MinValue Then
                                        dataPoint = Short.MinValue
                                        distorsion = True
                                        distorsionSampleCount += 1
                                    End If

                                    If dataPoint > Short.MaxValue Then
                                        dataPoint = Short.MaxValue
                                        distorsion = True
                                        distorsionSampleCount += 1
                                    End If

                                    dataPointShortValue = dataPoint
                                    writer.Write(dataPointShortValue)
                                Next
                        End Select

                    Case Audio.Formats.WaveFormat.WaveFormatEncodings.IeeeFloatingPoints
                        Select Case WaveFormat.BitDepth
                            Case 32

                                Dim dataPointValue As Single = 0
                                For Each dataPoint In interleavedSoundArray

                                    If dataPoint < Single.MinValue Then
                                        dataPoint = Single.MinValue
                                        distorsion = True
                                        distorsionSampleCount += 1
                                    End If

                                    If dataPoint > Single.MaxValue Then
                                        dataPoint = Single.MaxValue
                                        distorsion = True
                                        distorsionSampleCount += 1
                                    End If

                                    dataPointValue = dataPoint
                                    writer.Write(dataPointValue)
                                Next
                        End Select
                End Select

                'Case PCM 16 bit 'This alternative piece of code throws an exception every time distorsion occurs. The code is faster than checking each sample
                'if distorsion occurs only for a relatively small number of samples. But much slower if much distorsion occurs!
                '   Dim dataPointShortValue As Short
                '  For Each dataPoint In interleavedSoundArray 
                '
                'Try
                'dataPointShortValue = dataPoint
                'Catch ex As Exception
                'Try
                'If dataPoint < Short.MinValue Then
                'dataPointShortValue = Short.MinValue
                'distorsion = True
                'distorsionSampleCount += 1
                'End If
                '
                'If dataPoint > Short.MaxValue Then
                'dataPointShortValue = Short.MaxValue
                'distorsion = True
                'distorsionSampleCount += 1
                'End If
                '
                'Catch ex2 As Exception
                'Errors("Error writing sound stream. " & ex2.ToString)
                'End Try
                'End Try
                '
                'writer.Write(dataPointShortValue)
                'Next

            End If

            Dim dataChunkZeroPaddingCount As Integer = 0
            If numberOfDataPoints Mod 2 = 1 Then
                'MsgBox("writing one pad byte")
                dataChunkZeroPaddingCount += 1
                Dim zeroPad As Byte = 0
                writer.Write(zeroPad)
            End If

            writer.Seek(4, SeekOrigin.Begin)
            fileSize = writer.BaseStream.Length - dataChunkZeroPaddingCount - 8
            writer.Write(fileSize)

            Dim warnForDistorsion As Boolean = True
            If warnForDistorsion = True Then
                If distorsion = True Then
                    AudioError("Distorsion occurred for " & distorsionSampleCount & " samples in WriteSoundStream!" & vbTab & "Sound file name: " & FileName)
                End If
            End If


            'Storing the soundStream 
            Return WaveStream


        Catch ex As Exception
            AudioError(ex.ToString)
            Return Nothing
        End Try

    End Function

    ''' <summary>
    ''' Writes iXML data to the indicated MemoryStream.
    ''' </summary>
    ''' <param name="TargetStream"></param>
    ''' <returns>Returns True if write was succesful, or false if writing failed.</returns>
    Private Function WriteiXmlToStream(ByRef TargetStream As MemoryStream) As Boolean

        Try

            'Creating XmlWriterSettings
            Dim settings As XmlWriterSettings = New XmlWriterSettings()
            settings.Indent = True
            settings.Encoding = Text.Encoding.UTF8

            'Writing xml data to the stream
            Using writer As XmlWriter = XmlWriter.Create(TargetStream, settings)

                ' Begin writing.
                writer.WriteStartDocument()
                writer.WriteStartElement("BWFXML") ' Root.
                writer.WriteElementString("IXML_VERSION", "2.10")

                'Writing any xml tags and data stored in the Sound.iXmlNodes object
                If iXmlNodes IsNot Nothing Then
                    For Each CurrentNode In iXmlNodes
                        writer.WriteStartElement(CurrentNode.Item1)
                        writer.WriteRaw(CurrentNode.Item2)
                        writer.WriteEndElement()
                    Next
                End If


                'Writing the SMA chunk
                '(N.B. The SMA decimal separator should always be point (.), not comma)

                writer.WriteStartElement("SMA")
                writer.WriteElementString("SMA_VERSION", Audio.Sound.SpeechMaterialAnnotation.CurrentVersion) 'I.e. SMA version number

                For channel As Integer = 1 To SMA.ChannelCount

                    writer.WriteStartElement("CHANNEL")

                    If SMA.ChannelData(channel).OrthographicForm <> "" Then
                        writer.WriteElementString("ORTHOGRAPHIC_FORM", SMA.ChannelData(channel).OrthographicForm)
                    End If
                    If SMA.ChannelData(channel).PhoneticForm <> "" Then
                        writer.WriteElementString("PHONETIC_FORM", SMA.ChannelData(channel).PhoneticForm)
                    End If

                    writer.WriteElementString("START_SAMPLE", SMA.ChannelData(channel).StartSample)
                    writer.WriteElementString("LENGTH", SMA.ChannelData(channel).Length)

                    If SMA.ChannelData(channel).UnWeightedLevel IsNot Nothing Then
                        writer.WriteElementString("UNWEIGHTED_LEVEL", SMA.ChannelData(channel).UnWeightedLevel.Value.ToString(InvariantCulture))
                    End If

                    If SMA.ChannelData(channel).UnWeightedPeakLevel IsNot Nothing Then
                        writer.WriteElementString("UNWEIGHTED_PEAKLEVEL", SMA.ChannelData(channel).UnWeightedPeakLevel.Value.ToString(InvariantCulture))
                    End If

                    If SMA.ChannelData(channel).WeightedLevel IsNot Nothing Then
                        writer.WriteElementString("WEIGHTED_LEVEL", SMA.ChannelData(channel).WeightedLevel.Value.ToString(InvariantCulture))
                    End If
                    writer.WriteElementString("FREQUENCY_WEIGHTING", SMA.ChannelData(channel).GetFrequencyWeighting.ToString)
                    writer.WriteElementString("TIME_WEIGHTING", SMA.ChannelData(channel).GetTimeWeighting.ToString(InvariantCulture))

                    For sentence As Integer = 0 To SMA.ChannelData(channel).Count - 1

                        writer.WriteStartElement("SENTENCE")

                        'Writing sentence data
                        If SMA.ChannelData(channel)(sentence).OrthographicForm <> "" Then
                            writer.WriteElementString("ORTHOGRAPHIC_FORM", SMA.ChannelData(channel)(sentence).OrthographicForm)
                        End If
                        If SMA.ChannelData(channel)(sentence).PhoneticForm <> "" Then
                            writer.WriteElementString("PHONETIC_FORM", SMA.ChannelData(channel)(sentence).PhoneticForm)
                        End If

                        writer.WriteElementString("START_SAMPLE", SMA.ChannelData(channel)(sentence).StartSample)
                        writer.WriteElementString("LENGTH", SMA.ChannelData(channel)(sentence).Length)
                        If SMA.ChannelData(channel)(sentence).UnWeightedLevel IsNot Nothing Then
                            writer.WriteElementString("UNWEIGHTED_LEVEL", SMA.ChannelData(channel)(sentence).UnWeightedLevel.Value.ToString(InvariantCulture))
                        End If

                        If SMA.ChannelData(channel)(sentence).UnWeightedPeakLevel IsNot Nothing Then
                            writer.WriteElementString("UNWEIGHTED_PEAKLEVEL", SMA.ChannelData(channel)(sentence).UnWeightedPeakLevel.Value.ToString(InvariantCulture))
                        End If

                        If SMA.ChannelData(channel)(sentence).WeightedLevel IsNot Nothing Then
                            writer.WriteElementString("WEIGHTED_LEVEL", SMA.ChannelData(channel)(sentence).WeightedLevel.Value.ToString(InvariantCulture))
                        End If
                        writer.WriteElementString("FREQUENCY_WEIGHTING", SMA.ChannelData(channel)(sentence).GetFrequencyWeighting.ToString)
                        writer.WriteElementString("TIME_WEIGHTING", SMA.ChannelData(channel)(sentence).GetTimeWeighting.ToString(InvariantCulture))

                        writer.WriteElementString("INITIAL_PEAK", SMA.ChannelData(channel)(sentence).InitialPeak.ToString(InvariantCulture))
                        writer.WriteElementString("START_TIME", SMA.ChannelData(channel)(sentence).StartTime.ToString(InvariantCulture))

                        For word = 0 To SMA.ChannelData(channel)(sentence).Count - 1

                            writer.WriteStartElement("WORD")

                            'writing word data
                            If SMA.ChannelData(channel)(sentence)(word).OrthographicForm <> "" Then
                                writer.WriteElementString("ORTHOGRAPHIC_FORM", SMA.ChannelData(channel)(sentence)(word).OrthographicForm)
                            End If
                            If SMA.ChannelData(channel)(sentence)(word).PhoneticForm <> "" Then
                                writer.WriteElementString("PHONETIC_FORM", SMA.ChannelData(channel)(sentence)(word).PhoneticForm)
                            End If

                            writer.WriteElementString("START_SAMPLE", SMA.ChannelData(channel)(sentence)(word).StartSample)
                            writer.WriteElementString("LENGTH", SMA.ChannelData(channel)(sentence)(word).Length)

                            If SMA.ChannelData(channel)(sentence)(word).UnWeightedLevel IsNot Nothing Then
                                writer.WriteElementString("UNWEIGHTED_LEVEL", SMA.ChannelData(channel)(sentence)(word).UnWeightedLevel.Value.ToString(InvariantCulture))
                            End If

                            If SMA.ChannelData(channel)(sentence)(word).UnWeightedPeakLevel IsNot Nothing Then
                                writer.WriteElementString("UNWEIGHTED_PEAKLEVEL", SMA.ChannelData(channel)(sentence)(word).UnWeightedPeakLevel.Value.ToString(InvariantCulture))
                            End If

                            If SMA.ChannelData(channel)(sentence)(word).WeightedLevel IsNot Nothing Then
                                writer.WriteElementString("WEIGHTED_LEVEL", SMA.ChannelData(channel)(sentence)(word).WeightedLevel.Value.ToString(InvariantCulture))
                            End If
                            writer.WriteElementString("FREQUENCY_WEIGHTING", SMA.ChannelData(channel)(sentence)(word).GetFrequencyWeighting.ToString)
                            writer.WriteElementString("TIME_WEIGHTING", SMA.ChannelData(channel)(sentence)(word).GetTimeWeighting.ToString(InvariantCulture))

                            writer.WriteElementString("START_TIME", SMA.ChannelData(channel)(sentence)(word).StartTime.ToString(InvariantCulture))

                            'writing phone data
                            For phone = 0 To SMA.ChannelData(channel)(sentence)(word).PhoneData.Count - 1

                                writer.WriteStartElement("PHONE")

                                If SMA.ChannelData(channel)(sentence)(word).PhoneData(phone).OrthographicForm <> "" Then
                                    writer.WriteElementString("ORTHOGRAPHIC_FORM", SMA.ChannelData(channel)(sentence)(word).PhoneData(phone).OrthographicForm)
                                End If
                                If SMA.ChannelData(channel)(sentence)(word).PhoneData(phone).PhoneticForm <> "" Then
                                    writer.WriteElementString("PHONETIC_FORM", SMA.ChannelData(channel)(sentence)(word).PhoneData(phone).PhoneticForm)
                                End If

                                writer.WriteElementString("START_SAMPLE", SMA.ChannelData(channel)(sentence)(word).PhoneData(phone).StartSample)
                                writer.WriteElementString("LENGTH", SMA.ChannelData(channel)(sentence)(word).PhoneData(phone).Length)

                                If SMA.ChannelData(channel)(sentence)(word).PhoneData(phone).UnWeightedLevel IsNot Nothing Then
                                    writer.WriteElementString("UNWEIGHTED_LEVEL", SMA.ChannelData(channel)(sentence)(word).PhoneData(phone).UnWeightedLevel.Value.ToString(InvariantCulture))
                                End If

                                If SMA.ChannelData(channel)(sentence)(word).PhoneData(phone).UnWeightedPeakLevel IsNot Nothing Then
                                    writer.WriteElementString("UNWEIGHTED_PEAKLEVEL", SMA.ChannelData(channel)(sentence)(word).PhoneData(phone).UnWeightedPeakLevel.Value.ToString(InvariantCulture))
                                End If

                                If SMA.ChannelData(channel)(sentence)(word).PhoneData(phone).WeightedLevel IsNot Nothing Then
                                    writer.WriteElementString("WEIGHTED_LEVEL", SMA.ChannelData(channel)(sentence)(word).PhoneData(phone).WeightedLevel.Value.ToString(InvariantCulture))
                                End If
                                writer.WriteElementString("FREQUENCY_WEIGHTING", SMA.ChannelData(channel)(sentence)(word).PhoneData(phone).FrequencyWeighting.ToString)
                                writer.WriteElementString("TIME_WEIGHTING", SMA.ChannelData(channel)(sentence)(word).PhoneData(phone).TimeWeighting.ToString(InvariantCulture))

                                'End of phone
                                writer.WriteEndElement()

                            Next

                            'End of word
                            writer.WriteEndElement()

                        Next

                        'End of sentence
                        writer.WriteEndElement()

                    Next

                    'End of channel
                    writer.WriteEndElement()
                Next

                ' End document.
                writer.WriteEndElement()
                writer.WriteEndElement()
                writer.WriteEndDocument()

                writer.Close()

            End Using

            Return True

        Catch ex As Exception

            MsgBox("Failed to write iXML data.")
            Return False
        End Try

    End Function

    Public Shared Function ParseiXMLString(ByVal fileStream As MemoryStream) As Tuple(Of Audio.Sound.SpeechMaterialAnnotation, List(Of Tuple(Of String, String)))

        Dim NewSMA As New Audio.Sound.SpeechMaterialAnnotation()


        Try


            'Storing the current culture decimal separator, in order for parsing of Double values to work (independently of current culture). (N.B. The SMA decimal separator should always be point (.), not comma)
            Dim CDS = System.Threading.Thread.CurrentThread.CurrentCulture.NumberFormat.NumberDecimalSeparator

            'Writing the SMA chunk

            'Creating XmlWriterSettings
            Dim settings As XmlReaderSettings = New XmlReaderSettings

            Dim BWFXML_detected As Boolean = False
            Dim iXML_Verison As String = ""

            Dim unUsediXMLNodes As New List(Of Tuple(Of String, String))

            ' Create XmlWriter.
            Using reader As XmlReader = XmlReader.Create(fileStream, settings)

                While reader.Read

                    ' Check for start elements.
                    If reader.IsStartElement() Then

                        If reader.Name = "BWFXML" Then
                            BWFXML_detected = True

                        ElseIf reader.Name = "IXML_VERSION" Then
                            If reader.Read() Then iXML_Verison = reader.Value.Trim()

                            'Parsing SMA head data

                        ElseIf reader.Name = "SMA" Then

                            'Declaring variables used when parsing the SMA subtree
                            Dim SMA_Version As String = ""

                            'Indices that keep track of the current channel, sentence, word and phone (all increase by 1 when detected)
                            Dim CurrentChannel As Integer = 0
                            Dim CurrentSentence As Integer = -1
                            Dim CurrentWord As Integer = -1
                            Dim CurrentPhone As Integer = -1

                            'Reading the SMA subtree
                            Dim smaReader = reader.ReadSubtree()
                            While smaReader.Read

                                ' Check for start elements.
                                If smaReader.IsStartElement() Then

                                    If smaReader.Name = "SMA" Then
                                        'Just ignores the head node

                                    ElseIf smaReader.Name = "SMA_VERSION" Then
                                        If smaReader.Read() Then NewSMA.ReadFromVersion = smaReader.Value.Trim()

                                    ElseIf smaReader.Name = "CHANNEL" Then

                                        'New channel
                                        CurrentChannel += 1
                                        NewSMA.AddChannelData()

                                        'Reading the SMA channel subtree
                                        Dim smaChannelReader = smaReader.ReadSubtree()
                                        While smaChannelReader.Read

                                            ' Check for start elements.
                                            If smaChannelReader.IsStartElement() Then

                                                If smaChannelReader.Name = "CHANNEL" Then
                                                    'Just ignores the head node

                                                ElseIf smaChannelReader.Name = "ORTHOGRAPHIC_FORM" Then
                                                    If smaChannelReader.Read() Then
                                                        NewSMA.ChannelData(CurrentChannel).OrthographicForm = smaChannelReader.Value.Trim()
                                                    End If

                                                ElseIf smaChannelReader.Name = "PHONETIC_FORM" Then
                                                    If smaChannelReader.Read() Then
                                                        NewSMA.ChannelData(CurrentChannel).PhoneticForm = smaChannelReader.Value.Trim()
                                                    End If

                                                ElseIf smaChannelReader.Name = "START_SAMPLE" Then
                                                    Dim value As Integer
                                                    If smaChannelReader.Read() Then
                                                        If Integer.TryParse(smaChannelReader.Value.Trim(), value) = True Then
                                                            NewSMA.ChannelData(CurrentChannel).StartSample = value
                                                        End If
                                                    End If

                                                ElseIf smaChannelReader.Name = "LENGTH" Then
                                                    Dim value As Integer
                                                    If smaChannelReader.Read() Then
                                                        If Integer.TryParse(smaChannelReader.Value.Trim(), value) = True Then
                                                            NewSMA.ChannelData(CurrentChannel).Length = value
                                                        End If
                                                    End If

                                                ElseIf smaChannelReader.Name = "UNWEIGHTED_LEVEL" Then
                                                    Dim value As Double
                                                    If smaChannelReader.Read() Then
                                                        If Double.TryParse(smaChannelReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                            NewSMA.ChannelData(CurrentChannel).UnWeightedLevel = value
                                                        End If
                                                    End If

                                                ElseIf smaChannelReader.Name = "UNWEIGHTED_PEAKLEVEL" Then
                                                    Dim value As Double
                                                    If smaChannelReader.Read() Then
                                                        If Double.TryParse(smaChannelReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                            NewSMA.ChannelData(CurrentChannel).UnWeightedPeakLevel = value
                                                        End If
                                                    End If

                                                ElseIf smaChannelReader.Name = "WEIGHTED_LEVEL" Then
                                                    Dim value As Double
                                                    If smaChannelReader.Read() Then
                                                        If Double.TryParse(smaChannelReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                            NewSMA.ChannelData(CurrentChannel).WeightedLevel = value
                                                        End If
                                                    End If

                                                ElseIf smaChannelReader.Name = "FREQUENCY_WEIGHTING" Then
                                                    Dim value As FrequencyWeightings
                                                    If smaChannelReader.Read() Then
                                                        If [Enum].TryParse(smaChannelReader.Value.Trim(), True, value) = True Then
                                                            NewSMA.ChannelData(CurrentChannel).SetFrequencyWeighting(value, False)
                                                        End If
                                                    End If

                                                ElseIf smaChannelReader.Name = "TIME_WEIGHTING" Then
                                                    Dim value As Double
                                                    If smaChannelReader.Read() Then
                                                        If Double.TryParse(smaChannelReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                            NewSMA.ChannelData(CurrentChannel).SetTimeWeighting(value, False)
                                                        End If
                                                    End If

                                                ElseIf smaChannelReader.Name = "SENTENCE" Then

                                                    'New sentence
                                                    CurrentSentence += 1
                                                    NewSMA.ChannelData(CurrentChannel).Add(New Sound.SpeechMaterialAnnotation.SmaSentenceData(NewSMA))

                                                    'Reading the SMA sentence subtree
                                                    Dim smaSentenceReader = smaChannelReader.ReadSubtree()
                                                    While smaSentenceReader.Read

                                                        ' Check for start elements.
                                                        If smaSentenceReader.IsStartElement() Then

                                                            If smaSentenceReader.Name = "SENTENCE" Then
                                                                'Just ignores the head node

                                                            ElseIf smaSentenceReader.Name = "ORTHOGRAPHIC_FORM" Then
                                                                If smaSentenceReader.Read() Then
                                                                    NewSMA.ChannelData(CurrentChannel)(CurrentSentence).OrthographicForm = smaSentenceReader.Value.Trim()
                                                                End If

                                                            ElseIf smaSentenceReader.Name = "PHONETIC_FORM" Then
                                                                If smaSentenceReader.Read() Then
                                                                    NewSMA.ChannelData(CurrentChannel)(CurrentSentence).PhoneticForm = smaSentenceReader.Value.Trim()
                                                                End If

                                                            ElseIf smaSentenceReader.Name = "START_SAMPLE" Then
                                                                Dim value As Integer
                                                                If smaSentenceReader.Read() Then
                                                                    If Integer.TryParse(smaSentenceReader.Value.Trim(), value) = True Then
                                                                        NewSMA.ChannelData(CurrentChannel)(CurrentSentence).StartSample = value
                                                                    End If
                                                                End If

                                                            ElseIf smaSentenceReader.Name = "LENGTH" Then
                                                                Dim value As Integer
                                                                If smaSentenceReader.Read() Then
                                                                    If Integer.TryParse(smaSentenceReader.Value.Trim(), value) = True Then
                                                                        NewSMA.ChannelData(CurrentChannel)(CurrentSentence).Length = value
                                                                    End If
                                                                End If

                                                            ElseIf smaSentenceReader.Name = "UNWEIGHTED_LEVEL" Then
                                                                Dim value As Double
                                                                If smaSentenceReader.Read() Then
                                                                    If Double.TryParse(smaSentenceReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                                        NewSMA.ChannelData(CurrentChannel)(CurrentSentence).UnWeightedLevel = value
                                                                    End If
                                                                End If

                                                            ElseIf smaSentenceReader.Name = "UNWEIGHTED_PEAKLEVEL" Then
                                                                Dim value As Double
                                                                If smaSentenceReader.Read() Then
                                                                    If Double.TryParse(smaSentenceReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                                        NewSMA.ChannelData(CurrentChannel)(CurrentSentence).UnWeightedPeakLevel = value
                                                                    End If
                                                                End If

                                                            ElseIf smaSentenceReader.Name = "WEIGHTED_LEVEL" Then
                                                                Dim value As Double
                                                                If smaSentenceReader.Read() Then
                                                                    If Double.TryParse(smaSentenceReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                                        NewSMA.ChannelData(CurrentChannel)(CurrentSentence).WeightedLevel = value
                                                                    End If
                                                                End If

                                                            ElseIf smaSentenceReader.Name = "FREQUENCY_WEIGHTING" Then
                                                                Dim value As FrequencyWeightings
                                                                If smaSentenceReader.Read() Then
                                                                    If [Enum].TryParse(smaSentenceReader.Value.Trim(), True, value) = True Then
                                                                        NewSMA.ChannelData(CurrentChannel)(CurrentSentence).SetFrequencyWeighting(value, False)
                                                                    End If
                                                                End If

                                                            ElseIf smaSentenceReader.Name = "TIME_WEIGHTING" Then
                                                                Dim value As Double
                                                                If smaSentenceReader.Read() Then
                                                                    If Double.TryParse(smaSentenceReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                                        NewSMA.ChannelData(CurrentChannel)(CurrentSentence).SetTimeWeighting(value, False)
                                                                    End If
                                                                End If

                                                            ElseIf smaSentenceReader.Name = "INITIAL_PEAK" Then
                                                                Dim value As Double
                                                                If smaSentenceReader.Read() Then
                                                                    If Double.TryParse(smaSentenceReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                                        NewSMA.ChannelData(CurrentChannel)(CurrentSentence).InitialPeak = value
                                                                    End If
                                                                End If

                                                            ElseIf smaSentenceReader.Name = "START_TIME" Then
                                                                Dim value As Double
                                                                If smaSentenceReader.Read() Then
                                                                    If Double.TryParse(smaSentenceReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                                        NewSMA.ChannelData(CurrentChannel)(CurrentSentence).StartTime = value
                                                                    End If
                                                                End If

                                                            ElseIf smaSentenceReader.Name = "WORD" Then

                                                                ' A new word
                                                                CurrentWord += 1
                                                                NewSMA.ChannelData(CurrentChannel)(CurrentSentence).Add(New Sound.SpeechMaterialAnnotation.SmaWordData(NewSMA))

                                                                'Reading the SMA word subtree
                                                                Dim smaWordReader = smaSentenceReader.ReadSubtree()
                                                                While smaWordReader.Read

                                                                    ' Check for start elements.
                                                                    If smaWordReader.IsStartElement() Then

                                                                        If smaWordReader.Name = "WORD" Then
                                                                            'Just ignores the head node

                                                                        ElseIf smaWordReader.Name = "ORTHOGRAPHIC_FORM" Then
                                                                            If smaWordReader.Read() Then
                                                                                NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).OrthographicForm = smaWordReader.Value.Trim()
                                                                            End If

                                                                        ElseIf smaWordReader.Name = "PHONETIC_FORM" Then
                                                                            If smaWordReader.Read() Then
                                                                                NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).PhoneticForm = smaWordReader.Value.Trim()
                                                                            End If

                                                                        ElseIf smaWordReader.Name = "START_SAMPLE" Then
                                                                            Dim value As Integer
                                                                            If smaWordReader.Read() Then
                                                                                If Integer.TryParse(smaWordReader.Value.Trim(), value) = True Then
                                                                                    NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).StartSample = value
                                                                                End If
                                                                            End If

                                                                        ElseIf smaWordReader.Name = "LENGTH" Then
                                                                            Dim value As Integer
                                                                            If smaWordReader.Read() Then
                                                                                If Integer.TryParse(smaWordReader.Value.Trim(), value) = True Then
                                                                                    NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).Length = value
                                                                                End If
                                                                            End If

                                                                        ElseIf smaWordReader.Name = "UNWEIGHTED_LEVEL" Then
                                                                            Dim value As Double
                                                                            If smaWordReader.Read() Then
                                                                                If Double.TryParse(smaWordReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                                                    NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).UnWeightedLevel = value
                                                                                End If
                                                                            End If

                                                                        ElseIf smaWordReader.Name = "UNWEIGHTED_PEAKLEVEL" Then
                                                                            Dim value As Double
                                                                            If smaWordReader.Read() Then
                                                                                If Double.TryParse(smaWordReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                                                    NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).UnWeightedPeakLevel = value
                                                                                End If
                                                                            End If

                                                                        ElseIf smaWordReader.Name = "WEIGHTED_LEVEL" Then
                                                                            Dim value As Double
                                                                            If smaWordReader.Read() Then
                                                                                If Double.TryParse(smaWordReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                                                    NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).WeightedLevel = value
                                                                                End If
                                                                            End If

                                                                        ElseIf smaWordReader.Name = "FREQUENCY_WEIGHTING" Then
                                                                            Dim value As FrequencyWeightings
                                                                            If smaWordReader.Read() Then
                                                                                If [Enum].TryParse(smaWordReader.Value.Trim(), True, value) = True Then
                                                                                    NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).SetFrequencyWeighting(value, False)
                                                                                End If
                                                                            End If

                                                                        ElseIf smaWordReader.Name = "TIME_WEIGHTING" Then
                                                                            Dim value As Double
                                                                            If smaWordReader.Read() Then
                                                                                If Double.TryParse(smaWordReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                                                    NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).SetTimeWeighting(value, False)
                                                                                End If
                                                                            End If

                                                                        ElseIf smaWordReader.Name = "START_TIME" Then
                                                                            Dim value As Double
                                                                            If smaWordReader.Read() Then
                                                                                If Double.TryParse(smaWordReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                                                    NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).StartTime = value
                                                                                End If
                                                                            End If

                                                                        ElseIf smaWordReader.Name = "PHONE" Then

                                                                            'A new phone
                                                                            CurrentPhone += 1
                                                                            NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).PhoneData.Add(New Sound.SpeechMaterialAnnotation.SmaPhoneData(NewSMA))

                                                                            'Reading the SMA phone subtree
                                                                            Dim smaPhoneReader = smaWordReader.ReadSubtree()
                                                                            While smaPhoneReader.Read

                                                                                ' Check for start elements.
                                                                                If smaPhoneReader.IsStartElement() Then

                                                                                    If smaPhoneReader.Name = "PHONE" Then
                                                                                        'Just ignores the head node

                                                                                    ElseIf smaPhoneReader.Name = "ORTHOGRAPHIC_FORM" Then
                                                                                        If smaPhoneReader.Read() Then
                                                                                            NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).PhoneData(CurrentPhone).OrthographicForm = smaPhoneReader.Value.Trim()
                                                                                        End If

                                                                                    ElseIf smaPhoneReader.Name = "PHONETIC_FORM" Then
                                                                                        If smaPhoneReader.Read() Then
                                                                                            NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).PhoneData(CurrentPhone).PhoneticForm = smaPhoneReader.Value.Trim()
                                                                                        End If

                                                                                    ElseIf smaPhoneReader.Name = "START_SAMPLE" Then
                                                                                        Dim value As Integer
                                                                                        If smaPhoneReader.Read() Then
                                                                                            If Integer.TryParse(smaPhoneReader.Value.Trim(), value) = True Then
                                                                                                NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).PhoneData(CurrentPhone).StartSample = value
                                                                                            End If
                                                                                        End If

                                                                                    ElseIf smaPhoneReader.Name = "LENGTH" Then
                                                                                        Dim value As Integer
                                                                                        If smaPhoneReader.Read() Then
                                                                                            If Integer.TryParse(smaPhoneReader.Value.Trim(), value) = True Then
                                                                                                NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).PhoneData(CurrentPhone).Length = value
                                                                                            End If
                                                                                        End If

                                                                                    ElseIf smaPhoneReader.Name = "UNWEIGHTED_LEVEL" Then
                                                                                        Dim value As Double
                                                                                        If smaPhoneReader.Read() Then
                                                                                            If Double.TryParse(smaPhoneReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                                                                NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).PhoneData(CurrentPhone).UnWeightedLevel = value
                                                                                            End If
                                                                                        End If

                                                                                    ElseIf smaPhoneReader.Name = "UNWEIGHTED_PEAKLEVEL" Then
                                                                                        Dim value As Double
                                                                                        If smaPhoneReader.Read() Then
                                                                                            If Double.TryParse(smaPhoneReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                                                                NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).PhoneData(CurrentPhone).UnWeightedPeakLevel = value
                                                                                            End If
                                                                                        End If

                                                                                    ElseIf smaPhoneReader.Name = "WEIGHTED_LEVEL" Then
                                                                                        Dim value As Double
                                                                                        If smaPhoneReader.Read() Then
                                                                                            If Double.TryParse(smaPhoneReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                                                                NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).PhoneData(CurrentPhone).WeightedLevel = value
                                                                                            End If
                                                                                        End If

                                                                                    ElseIf smaPhoneReader.Name = "FREQUENCY_WEIGHTING" Then
                                                                                        Dim value As FrequencyWeightings
                                                                                        If smaPhoneReader.Read() Then
                                                                                            If [Enum].TryParse(smaPhoneReader.Value.Trim(), True, value) = True Then
                                                                                                NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).PhoneData(CurrentPhone).FrequencyWeighting = value
                                                                                            End If
                                                                                        End If

                                                                                    ElseIf smaPhoneReader.Name = "TIME_WEIGHTING" Then
                                                                                        Dim value As Double
                                                                                        If smaPhoneReader.Read() Then
                                                                                            If Double.TryParse(smaPhoneReader.Value.Trim().Replace(",", CDS).Replace(".", CDS), value) = True Then
                                                                                                NewSMA.ChannelData(CurrentChannel)(CurrentSentence)(CurrentWord).PhoneData(CurrentPhone).TimeWeighting = value
                                                                                            End If
                                                                                        End If

                                                                                    Else
                                                                                        MsgBox("Unknown SMA phone node ignored " & "(" & smaPhoneReader.Name & ")!")
                                                                                    End If
                                                                                End If
                                                                            End While
                                                                        Else
                                                                            MsgBox("Unknown SMA word node ignored " & "(" & smaWordReader.Name & ")!")
                                                                        End If
                                                                    End If
                                                                End While
                                                            Else
                                                                MsgBox("Unknown SMA sentence node ignored " & "(" & smaSentenceReader.Name & ")!")
                                                            End If
                                                        End If
                                                    End While
                                                Else
                                                    MsgBox("Unknown SMA channel node ignored " & "(" & smaChannelReader.Name & ")!")
                                                End If
                                            End If
                                        End While
                                    Else
                                        MsgBox("Unknown SMA head node ignored " & "(" & smaReader.Name & ")!")
                                    End If
                                End If
                            End While
                        Else

                            'Adding any unparsed nodes to the unUsediXMLNodes list 
                            unUsediXMLNodes.Add(New Tuple(Of String, String)(reader.Name, reader.ReadInnerXml))

                        End If
                    End If
                End While

            End Using


            Return New Tuple(Of Audio.Sound.SpeechMaterialAnnotation, List(Of Tuple(Of String, String)))(NewSMA, unUsediXMLNodes)

        Catch ex As Exception
            MsgBox(ex.ToString)
            Return Nothing
        End Try

    End Function


#End Region

End Class

