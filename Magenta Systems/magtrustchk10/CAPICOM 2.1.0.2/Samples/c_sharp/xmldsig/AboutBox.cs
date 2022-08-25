using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using Sample;

namespace Sample.UI
{
	internal class AboutBox : System.Windows.Forms.Form
	{
        private System.Windows.Forms.PictureBox IconBox;
        private System.Windows.Forms.Label TitleLabel;
        private System.Windows.Forms.Label CopyrightLabel;
        private System.Windows.Forms.Button OkButton;
		private System.ComponentModel.Container components = null;

		public AboutBox() {
			InitializeComponent();
		}

		protected override void Dispose( bool disposing ) {
			if( disposing ) {
				if (components != null) {
					components.Dispose();
				}
			}
			base.Dispose( disposing );
		}

		private void InitializeComponent() {
            this.Icon = new Icon(GetType().Module.Assembly.GetManifestResourceStream("Traffic.ico"));

            System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(AboutBox));
            this.IconBox = new System.Windows.Forms.PictureBox();
            this.TitleLabel = new System.Windows.Forms.Label();
            this.CopyrightLabel = new System.Windows.Forms.Label();
            this.OkButton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // IconBox
            // 
            this.IconBox.Image = new Bitmap(GetType().Module.Assembly.GetManifestResourceStream("traffic_light.gif"));
            this.IconBox.Location = new System.Drawing.Point(8, 8);
            this.IconBox.Name = "IconBox";
            this.IconBox.Size = new System.Drawing.Size(48, 48);
            this.IconBox.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
            this.IconBox.TabIndex = 0;
            this.IconBox.TabStop = false;
            // 
            // TitleLabel
            // 
            this.TitleLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 15.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
            this.TitleLabel.Location = new System.Drawing.Point(56, 16);
            this.TitleLabel.Name = "TitleLabel";
            this.TitleLabel.Size = new System.Drawing.Size(328, 32);
            this.TitleLabel.TabIndex = 1;
            this.TitleLabel.Text = Messages.DemoTitle;
            // 
            // CopyrightLabel
            // 
            this.CopyrightLabel.Location = new System.Drawing.Point(56, 40);
            this.CopyrightLabel.Name = "CopyrightLabel";
            this.CopyrightLabel.Size = new System.Drawing.Size(328, 50);
            this.CopyrightLabel.TabIndex = 2;
            this.CopyrightLabel.Text = Messages.CopyrightMessage;
            // 
            // OkButton
            // 
            this.OkButton.Location = new System.Drawing.Point(304, 72);
            this.OkButton.Name = "OkButton";
            this.OkButton.TabIndex = 3;
            this.OkButton.Text = "Ok";
            this.OkButton.Click += new System.EventHandler(this.OkButton_Click);
            // 
            // AboutBox
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(392, 109);
            this.Controls.AddRange(new System.Windows.Forms.Control[] {
                                                      this.OkButton,
                                                      this.CopyrightLabel,
                                                      this.TitleLabel,
                                                      this.IconBox});
            this.AcceptButton = this.OkButton;
            this.CancelButton = this.OkButton;
            this.MaximizeBox = false;
            this.MaximumSize = new System.Drawing.Size(400, 136);
            this.MinimizeBox = false;
            this.MinimumSize = new System.Drawing.Size(400, 136);
            this.SizeGripStyle = SizeGripStyle.Hide;
            this.Name = "AboutBox";
            this.Text = "About";
            this.ResumeLayout(false);
        }

        private void OkButton_Click(object sender, System.EventArgs e) {
            this.Close();
        }

	}
}
