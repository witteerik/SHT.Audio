namespace TestProjectCS
{
    partial class Form1
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.StopSoundButton = new System.Windows.Forms.Button();
            this.PlayRandomButton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // StopSoundButton
            // 
            this.StopSoundButton.Location = new System.Drawing.Point(34, 66);
            this.StopSoundButton.Name = "StopSoundButton";
            this.StopSoundButton.Size = new System.Drawing.Size(196, 23);
            this.StopSoundButton.TabIndex = 3;
            this.StopSoundButton.Text = "Stop sound";
            this.StopSoundButton.UseVisualStyleBackColor = true;
            this.StopSoundButton.Click += new System.EventHandler(this.StopSoundButton_Click);
            // 
            // PlayRandomButton
            // 
            this.PlayRandomButton.Location = new System.Drawing.Point(34, 27);
            this.PlayRandomButton.Name = "PlayRandomButton";
            this.PlayRandomButton.Size = new System.Drawing.Size(196, 23);
            this.PlayRandomButton.TabIndex = 2;
            this.PlayRandomButton.Text = "Play sound in random channel";
            this.PlayRandomButton.UseVisualStyleBackColor = true;
            this.PlayRandomButton.Click += new System.EventHandler(this.PlayRandomButton_Click);
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(427, 120);
            this.Controls.Add(this.StopSoundButton);
            this.Controls.Add(this.PlayRandomButton);
            this.Name = "Form1";
            this.Text = "Form1";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.Form1_FormClosing);
            this.Load += new System.EventHandler(this.Form1_Load);
            this.ResumeLayout(false);

        }

        #endregion

        internal System.Windows.Forms.Button StopSoundButton;
        internal System.Windows.Forms.Button PlayRandomButton;
    }
}

