//******************************************************************************
//
// THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
// EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
//
// Copyright (C) 1999- 2002.  Microsoft Corporation.  All rights reserved.
//
//******************************************************************************
//
// MainWindow.cs
//
// This is a C# sample that illustrates how to use features introduced in
// CAPICOM's bridging support to interop between CAPI and System.Security.xml.
//
// Note: for simplicity, this code does not handle exceptions.
//
//******************************************************************************

using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using System.Xml;
using Sample;

namespace Sample.UI
{
    internal class MainWindow : Form {
        private Container components;

        private MenuItem helpMenu;
        private MenuItem aboutMenu;
        private MenuItem closeMenu;

        private MenuItem fileMenu;
        private MenuItem openFileMenu;

        private StatusBar statusBar;

        private TextBox textBoxSign;
        private TextBox textBoxVerify;

        private Label labelSign;
        private Label labelVerify;

        private Button buttonSign;
        private Button buttonVerify;

        private ComboBox keyInfoBox;
        private GroupBox signGroupBox;

        private MainMenu mainMenu;

        public MainWindow () {
            InitializeComponent();
        }

        private void InitializeComponent() {
            this.SuspendLayout();

            components = new Container();
            textBoxSign = new TextBox();
            textBoxVerify = new TextBox();
            labelSign = new Label();
            labelVerify = new Label();
            buttonSign = new Button();
            buttonVerify = new Button();            
            keyInfoBox = new ComboBox();
            signGroupBox = new GroupBox();
            mainMenu = new MainMenu();

            this.Menu = this.mainMenu;
            this.Icon = new Icon(GetType().Module.Assembly.GetManifestResourceStream("Traffic.ico"));
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.Text = Messages.DemoTitle;
            this.ClientSize = new System.Drawing.Size(440, 520);
            this.MaximizeBox = false;
            this.FormBorderStyle = FormBorderStyle.FixedDialog;

            labelSign.Location = new System.Drawing.Point(15, 5);
            labelSign.Text = "XML fragment to sign:";
            labelSign.Size = new System.Drawing.Size(250, 20);
            labelVerify.TabStop = false;
            labelSign.Font = new System.Drawing.Font("Microsoft Sans Serif", 11F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));

            labelVerify.Location = new System.Drawing.Point(15, 245);
            labelVerify.Text = "Signed XML fragment:";
            labelVerify.TabStop = false;
            labelVerify.Size = new System.Drawing.Size(250, 20);
            labelVerify.Font = new System.Drawing.Font("Microsoft Sans Serif", 11F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));

            textBoxSign.Anchor = (AnchorStyles.Bottom | AnchorStyles.Top | AnchorStyles.Left | AnchorStyles.Right);
            textBoxSign.Location = new System.Drawing.Point(15, 35);
            textBoxSign.ReadOnly = false;
            textBoxSign.ScrollBars = ScrollBars.Vertical;
            textBoxSign.Multiline = true;
            textBoxSign.Size = new System.Drawing.Size(300, 200);
            textBoxSign.TabIndex = 1;
            textBoxSign.Text = "<text>this is some text</text>";
            textBoxSign.Font = new System.Drawing.Font("Arial", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));

            textBoxVerify.Anchor = (AnchorStyles.Bottom | AnchorStyles.Top | AnchorStyles.Left | AnchorStyles.Right);
            textBoxVerify.Location = new System.Drawing.Point(15, 275);
            textBoxVerify.Text = "";
            textBoxVerify.ReadOnly = false;
            textBoxVerify.ScrollBars = ScrollBars.Vertical;
            textBoxVerify.Multiline = true;
            textBoxVerify.TabIndex = 4;
            textBoxVerify.Size = new System.Drawing.Size(300, 200);
            textBoxVerify.Font = new System.Drawing.Font("Arial", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));

            buttonSign.Anchor = (AnchorStyles.Bottom | AnchorStyles.Right);
            buttonSign.Location = new System.Drawing.Point(15, 55);
            buttonSign.Size = new System.Drawing.Size(75, 25);
            buttonSign.TabIndex = 2;
            buttonSign.Font = new System.Drawing.Font("Arial", 10.5F, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
            buttonSign.Text = "&Sign";
            buttonSign.Click += new System.EventHandler(buttonSign_Click);

            buttonVerify.Anchor = (AnchorStyles.Bottom | AnchorStyles.Right);
            buttonVerify.Location = new System.Drawing.Point(348, 370);
            buttonVerify.Size = new System.Drawing.Size(75, 25);
            buttonVerify.TabIndex = 5;
            buttonVerify.Font = new System.Drawing.Font("Arial", 10.5F, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
            buttonVerify.Text = "&Verify";
            buttonVerify.Click += new System.EventHandler(buttonVerify_Click);

            keyInfoBox.Anchor = ((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left) | System.Windows.Forms.AnchorStyles.Right);
            keyInfoBox.DropDownWidth = 100;
            keyInfoBox.Items.AddRange(new object[] {"RSAKeyValue", "X509Data"});
            keyInfoBox.Location = new System.Drawing.Point(10, 20);
            keyInfoBox.Size = new System.Drawing.Size(85, 24);
            keyInfoBox.TabIndex = 1;
            keyInfoBox.Font = new System.Drawing.Font("Arial", 8.5F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
            keyInfoBox.SelectedIndex = 0;
            keyInfoBox.Font = new System.Drawing.Font("Arial", 8.5F, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
            
            signGroupBox.Anchor = ((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left) | System.Windows.Forms.AnchorStyles.Right);
            signGroupBox.Location = new System.Drawing.Point(330, 85);
            signGroupBox.Size = new System.Drawing.Size(105, 90);
            signGroupBox.TabIndex = 2;

            statusBar = new StatusBar();
            statusBar.Text = Messages.CopyrightMessage;
            statusBar.Dock = DockStyle.Bottom;
            statusBar.Font = new System.Drawing.Font("Arial", 8.5F, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));

            fileMenu = new MenuItem();
            fileMenu = mainMenu.MenuItems.Add("&File");

            helpMenu = new MenuItem();
            helpMenu = mainMenu.MenuItems.Add("&Help");

            aboutMenu = new MenuItem("&About", new EventHandler(this.AboutBoxHandler), Shortcut.F1);
            closeMenu = new MenuItem("&Close", new EventHandler(this.EscapeHandler), Shortcut.AltF4);
            helpMenu.MenuItems.Add(aboutMenu);

            openFileMenu = new MenuItem("&Open File", new EventHandler(this.OpenFileHandler), Shortcut.CtrlO);
            fileMenu.MenuItems.Add(openFileMenu);
            fileMenu.MenuItems.Add(closeMenu);

            this.Controls.Add(textBoxSign);
            this.Controls.Add(textBoxVerify);
            this.Controls.Add(labelSign);
            this.Controls.Add(labelVerify);
            this.Controls.Add(buttonVerify);
            signGroupBox.Controls.Add(buttonSign);
            signGroupBox.Controls.Add(keyInfoBox);
            this.Controls.Add(signGroupBox);
            this.Controls.Add(statusBar);

            this.ResumeLayout(false);
        }

        private void AboutBoxHandler(object sender, System.EventArgs e) {
            AboutBox aboutBox = new AboutBox();
            aboutBox.ShowDialog();
        }

        private void buttonSign_Click(object sender, System.EventArgs e) {
            switch (keyInfoBox.SelectedIndex) {
                case 0:
                    textBoxVerify.Text = SignVerify.SignXML(textBoxSign.Text);
                    break;
                case 1:
                    textBoxVerify.Text = SignVerify.SignXMLX509Data(textBoxSign.Text);
                    break;
            }                
        }

        private void buttonVerify_Click(object sender, System.EventArgs e) {
            bool bVer = SignVerify.VerifyXML(textBoxVerify.Text);
            if (bVer) 
                MessageBox.Show(Messages.SuccessMessage, Messages.DemoTitle, MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            else 
                MessageBox.Show(Messages.FailureMessage, Messages.DemoTitle, MessageBoxButtons.OK, MessageBoxIcon.Stop);
        }

        private void EscapeHandler(object sender, System.EventArgs e) {
            this.Close();
        }

        private void OpenFileHandler(object sender, System.EventArgs e) {
            OpenFileDialog openFileDialog = new OpenFileDialog();
            openFileDialog.CheckPathExists = true;
            openFileDialog.CheckFileExists = true;
            openFileDialog.InitialDirectory = "c:\\" ;
            openFileDialog.Filter = "xml files (*.xml)|*.xml|All files (*.*)|*.*" ;
            DialogResult dialogResult = openFileDialog.ShowDialog(this);
            if (dialogResult == DialogResult.OK) {
                try {
                    // See whether the file is a valid XML document
                    XmlDocument xmlDoc = new XmlDocument();
                    xmlDoc.PreserveWhitespace = true;
                    xmlDoc.Load(openFileDialog.FileName);
                    textBoxSign.Text = xmlDoc.OuterXml;                    
                } catch (Exception exc) {
                    MessageBox.Show(exc.ToString(), Messages.ExceptionTitle, MessageBoxButtons.OK, MessageBoxIcon.Stop);
                }
            }
        }
    }
}