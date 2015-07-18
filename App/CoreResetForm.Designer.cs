namespace CoreReset
{
    partial class CoreResetForm
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(CoreResetForm));
            this.MainPanel = new System.Windows.Forms.Panel();
            this.MainSplitContainer = new System.Windows.Forms.SplitContainer();
            this.CodeTextBox = new System.Windows.Forms.TextBox();
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.panel1 = new System.Windows.Forms.Panel();
            this.checkBoxShowOriginalVarNames = new System.Windows.Forms.CheckBox();
            this.ASTreeView = new System.Windows.Forms.TreeView();
            this.label3 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.textBoxAnalysisResult = new System.Windows.Forms.TextBox();
            this.buttonAbout = new System.Windows.Forms.Button();
            this.buttonAnalyse = new System.Windows.Forms.Button();
            this.buttonOpenCodeFile = new System.Windows.Forms.Button();
            this.MainPanel.SuspendLayout();
            this.MainSplitContainer.Panel1.SuspendLayout();
            this.MainSplitContainer.Panel2.SuspendLayout();
            this.MainSplitContainer.SuspendLayout();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // MainPanel
            // 
            this.MainPanel.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.MainPanel.BackColor = System.Drawing.SystemColors.ActiveCaptionText;
            this.MainPanel.Controls.Add(this.MainSplitContainer);
            this.MainPanel.Location = new System.Drawing.Point(16, 86);
            this.MainPanel.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.MainPanel.Name = "MainPanel";
            this.MainPanel.Size = new System.Drawing.Size(1208, 647);
            this.MainPanel.TabIndex = 0;
            // 
            // MainSplitContainer
            // 
            this.MainSplitContainer.Dock = System.Windows.Forms.DockStyle.Fill;
            this.MainSplitContainer.Location = new System.Drawing.Point(0, 0);
            this.MainSplitContainer.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.MainSplitContainer.Name = "MainSplitContainer";
            // 
            // MainSplitContainer.Panel1
            // 
            this.MainSplitContainer.Panel1.Controls.Add(this.CodeTextBox);
            // 
            // MainSplitContainer.Panel2
            // 
            this.MainSplitContainer.Panel2.Controls.Add(this.splitContainer1);
            this.MainSplitContainer.Size = new System.Drawing.Size(1208, 647);
            this.MainSplitContainer.SplitterDistance = 568;
            this.MainSplitContainer.SplitterWidth = 5;
            this.MainSplitContainer.TabIndex = 0;
            // 
            // CodeTextBox
            // 
            this.CodeTextBox.AcceptsReturn = true;
            this.CodeTextBox.AcceptsTab = true;
            this.CodeTextBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.CodeTextBox.Font = new System.Drawing.Font("Courier New", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(177)));
            this.CodeTextBox.Location = new System.Drawing.Point(0, 0);
            this.CodeTextBox.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.CodeTextBox.Multiline = true;
            this.CodeTextBox.Name = "CodeTextBox";
            this.CodeTextBox.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.CodeTextBox.Size = new System.Drawing.Size(568, 647);
            this.CodeTextBox.TabIndex = 0;
            this.CodeTextBox.WordWrap = false;
            // 
            // splitContainer1
            // 
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer1.Location = new System.Drawing.Point(0, 0);
            this.splitContainer1.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.splitContainer1.Name = "splitContainer1";
            this.splitContainer1.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.panel1);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.BackColor = System.Drawing.SystemColors.ControlLight;
            this.splitContainer1.Panel2.Controls.Add(this.label4);
            this.splitContainer1.Panel2.Controls.Add(this.textBoxAnalysisResult);
            this.splitContainer1.Size = new System.Drawing.Size(635, 647);
            this.splitContainer1.SplitterDistance = 217;
            this.splitContainer1.SplitterWidth = 5;
            this.splitContainer1.TabIndex = 0;
            // 
            // panel1
            // 
            this.panel1.BackColor = System.Drawing.SystemColors.ControlLight;
            this.panel1.Controls.Add(this.checkBoxShowOriginalVarNames);
            this.panel1.Controls.Add(this.ASTreeView);
            this.panel1.Controls.Add(this.label3);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(635, 217);
            this.panel1.TabIndex = 1;
            // 
            // checkBoxShowOriginalVarNames
            // 
            this.checkBoxShowOriginalVarNames.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.checkBoxShowOriginalVarNames.AutoSize = true;
            this.checkBoxShowOriginalVarNames.Location = new System.Drawing.Point(418, 5);
            this.checkBoxShowOriginalVarNames.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.checkBoxShowOriginalVarNames.Name = "checkBoxShowOriginalVarNames";
            this.checkBoxShowOriginalVarNames.Size = new System.Drawing.Size(214, 21);
            this.checkBoxShowOriginalVarNames.TabIndex = 8;
            this.checkBoxShowOriginalVarNames.Text = "Show original variable names";
            this.checkBoxShowOriginalVarNames.UseVisualStyleBackColor = true;
            this.checkBoxShowOriginalVarNames.CheckedChanged += new System.EventHandler(this.checkBoxShowOriginalVarNames_CheckedChanged);
            // 
            // ASTreeView
            // 
            this.ASTreeView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.ASTreeView.Location = new System.Drawing.Point(0, 27);
            this.ASTreeView.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.ASTreeView.Name = "ASTreeView";
            this.ASTreeView.Size = new System.Drawing.Size(633, 190);
            this.ASTreeView.TabIndex = 0;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(4, 7);
            this.label3.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(144, 17);
            this.label3.TabIndex = 7;
            this.label3.Text = "Abstract Syntax Tree:";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(4, 7);
            this.label4.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(59, 17);
            this.label4.TabIndex = 8;
            this.label4.Text = "Results:";
            // 
            // textBoxAnalysisResult
            // 
            this.textBoxAnalysisResult.AcceptsReturn = true;
            this.textBoxAnalysisResult.AcceptsTab = true;
            this.textBoxAnalysisResult.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.textBoxAnalysisResult.BackColor = System.Drawing.Color.Snow;
            this.textBoxAnalysisResult.ForeColor = System.Drawing.Color.Blue;
            this.textBoxAnalysisResult.Location = new System.Drawing.Point(0, 27);
            this.textBoxAnalysisResult.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.textBoxAnalysisResult.MaxLength = 10000000;
            this.textBoxAnalysisResult.Multiline = true;
            this.textBoxAnalysisResult.Name = "textBoxAnalysisResult";
            this.textBoxAnalysisResult.ReadOnly = true;
            this.textBoxAnalysisResult.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.textBoxAnalysisResult.Size = new System.Drawing.Size(633, 397);
            this.textBoxAnalysisResult.TabIndex = 0;
            this.textBoxAnalysisResult.WordWrap = false;
            // 
            // buttonAbout
            // 
            this.buttonAbout.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonAbout.Location = new System.Drawing.Point(1124, 12);
            this.buttonAbout.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.buttonAbout.Name = "buttonAbout";
            this.buttonAbout.Size = new System.Drawing.Size(100, 28);
            this.buttonAbout.TabIndex = 8;
            this.buttonAbout.Text = "About";
            this.buttonAbout.UseVisualStyleBackColor = true;
            this.buttonAbout.Click += new System.EventHandler(this.buttonAbout_Click);
            // 
            // buttonAnalyse
            // 
            this.buttonAnalyse.Image = ((System.Drawing.Image)(resources.GetObject("buttonAnalyse.Image")));
            this.buttonAnalyse.ImageAlign = System.Drawing.ContentAlignment.TopCenter;
            this.buttonAnalyse.Location = new System.Drawing.Point(96, 15);
            this.buttonAnalyse.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.buttonAnalyse.Name = "buttonAnalyse";
            this.buttonAnalyse.Size = new System.Drawing.Size(72, 64);
            this.buttonAnalyse.TabIndex = 2;
            this.buttonAnalyse.UseVisualStyleBackColor = true;
            this.buttonAnalyse.Click += new System.EventHandler(this.buttonAnalyse_Click);
            // 
            // buttonOpenCodeFile
            // 
            this.buttonOpenCodeFile.Image = ((System.Drawing.Image)(resources.GetObject("buttonOpenCodeFile.Image")));
            this.buttonOpenCodeFile.ImageAlign = System.Drawing.ContentAlignment.TopCenter;
            this.buttonOpenCodeFile.Location = new System.Drawing.Point(16, 15);
            this.buttonOpenCodeFile.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.buttonOpenCodeFile.Name = "buttonOpenCodeFile";
            this.buttonOpenCodeFile.Size = new System.Drawing.Size(72, 64);
            this.buttonOpenCodeFile.TabIndex = 1;
            this.buttonOpenCodeFile.UseVisualStyleBackColor = true;
            this.buttonOpenCodeFile.Click += new System.EventHandler(this.buttonOpenCodeFile_Click);
            // 
            // CoreResetForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1028, 748);
            this.Controls.Add(this.buttonAbout);
            this.Controls.Add(this.buttonAnalyse);
            this.Controls.Add(this.buttonOpenCodeFile);
            this.Controls.Add(this.MainPanel);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.MinimumSize = new System.Drawing.Size(1021, 722);
            this.Name = "CoreResetForm";
            this.Text = "Core Reset Basic - a front-end application for execution of parsing & analysis al" +
                "gorithms";
            this.MainPanel.ResumeLayout(false);
            this.MainSplitContainer.Panel1.ResumeLayout(false);
            this.MainSplitContainer.Panel1.PerformLayout();
            this.MainSplitContainer.Panel2.ResumeLayout(false);
            this.MainSplitContainer.ResumeLayout(false);
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel2.ResumeLayout(false);
            this.splitContainer1.Panel2.PerformLayout();
            this.splitContainer1.ResumeLayout(false);
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Panel MainPanel;
        private System.Windows.Forms.SplitContainer MainSplitContainer;
        private System.Windows.Forms.TextBox CodeTextBox;
        private System.Windows.Forms.Button buttonOpenCodeFile;
        private System.Windows.Forms.Button buttonAnalyse;
        private System.Windows.Forms.SplitContainer splitContainer1;
        private System.Windows.Forms.TextBox textBoxAnalysisResult;
        private System.Windows.Forms.TreeView ASTreeView;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Button buttonAbout;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.CheckBox checkBoxShowOriginalVarNames;
    }
}

