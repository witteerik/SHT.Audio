using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace TestProjectCS
{
    public partial class Form1 : Form, SHT.Audio.DuplexPlayer.ISoundPlayerControl
    {
        private SHT.Audio.Sound TestSound;
        private SHT.Audio.DuplexPlayer.SoundPlayer Player;

        private Random rnd = new Random();

        //int[] speakerChannels = new int[] { 1, 2, 3, 4, 5, 6, 7, 8, 13, 14, 15, 16 };
        int[] speakerChannels = new int[] { 1, 2 };
        int buffersPlayed = 0;


        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {

            try
            {
                // Loading a test sound from file. 
                TestSound = SHT.Audio.Sound.LoadWaveFile("[Enter the path to the sound file here]");

                // Referencing this as myPlayerControl, and a null instance of AudioApiSettings to send as arguments when initializing the sound player
                SHT.Audio.DuplexPlayer.ISoundPlayerControl myPlayerControl = this;
                SHT.Audio.DuplexPlayer.PortAudioVB.AudioApiSettings AudioApiSettings = null;

                // Initializing the sound player (with a cross-fade time between subsequently played sounds of 0.05 seconds)
                Player = new SHT.Audio.DuplexPlayer.SoundPlayer(ref myPlayerControl,
                    SHT.Audio.DuplexPlayer.SoundPlayer.SoundDirections.PlaybackOnly,
                    ref AudioApiSettings, TestSound.WaveFormat.Encoding, false, false, false, false, 0.1);

                // Opening the sound stream
                Player.OpenStream();

                // Starting the sound stream (The sound stream can be kept open even if the sound has reached the end. The reason is that the sound player detects the end of sound, and internally replaces the sound with a silent sound.)
                Player.Start();

            }
            catch (Exception ex)
            {
                MessageBox.Show("An exception occurred. Have you entered the sound file path?\n\n" + ex.Message);
            }

        }

        void SHT.Audio.DuplexPlayer.ISoundPlayerControl.MessageFromPlayer(ref SHT.Audio.DuplexPlayer.ISoundPlayerControl.MessagesFromSoundPlayer Message)
        {

            //This could contain the C# equivalent of the following VB code

            //    Select Case Message
            //    Case SHT.Audio.DuplexPlayer.ISoundPlayerControl.MessagesFromSoundPlayer.ApproachingEndOfBufferAlert

            //    Case SHT.Audio.DuplexPlayer.ISoundPlayerControl.MessagesFromSoundPlayer.EndOfSound

            //    Case SHT.Audio.DuplexPlayer.ISoundPlayerControl.MessagesFromSoundPlayer.NewBufferTick
            //        'Threading.Thread.CurrentThread.Join()

            //        'Dim t As New Threading.WaitCallback(Sub() IncreaseBufferCount())
            //        'Threading.ThreadPool.QueueUserWorkItem(t)

            //        IncreaseBufferCount()
            //    Case Else
            //        'Just ignores the message
            //End Select

        }

        private void IncreaseBufferCount()
        {
            buffersPlayed += 1;
        }


        private void PlayRandomButton_Click(object sender, EventArgs e)
        {
            PlaySoundInRandomChannel();
        }

        private void PlaySoundInRandomChannel()
        {

            // Creating a new mixer
            var NewMixer = new SHT.Audio.DuplexPlayer.SoundPlayer.DuplexMixer(Player.NumberOfOutputChannels, Player.NumberOfInputChannels);
            NewMixer.OutputRouting.Clear();

            // Setting the mixer to rout channel 1 in the test sound to a random speaker
            int RandomSpeakerChannel = speakerChannels[rnd.Next(0, speakerChannels.Length)];
            NewMixer.OutputRouting.Add(RandomSpeakerChannel, 1);

            // Replacing the existing mixer with the new mixer
            Player.Mixer = NewMixer;

            // Sends the test sound to the player to start playing
            Player.SwapOutputSounds(ref TestSound);
        }


        private void StopSoundButton_Click(object sender, EventArgs e)
        {
            //Fading out the sound
            Player.Stop(true);
        }

        private void Form1_FormClosing(object sender, FormClosingEventArgs e)
        {

            //Displosing the sound player
            Player.Stop();
            Player.CloseStream();
            Player.Dispose();

        }
    }
}
