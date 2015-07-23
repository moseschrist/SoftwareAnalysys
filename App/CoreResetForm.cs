using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using Microsoft.FSharp.Core;
using System.Threading;

namespace CoreReset
{
    /// <summary>
    /// This is the main application form.
    /// </summary>
    public partial class CoreResetForm : Form
    {
        /// <summary>
        /// A command tree node is a TreeNode placeholder which can hold
        /// properties associated with a core language command.
        /// </summary>
        public class CmdTreeNode : TreeNode
        {
            public CmdTreeNode(string text)
                : base(text)
            {
            }
         }

        private ToolTip m_analyseToolTip = null;
        private ToolTip m_generalToolTip = null;
        private BJKCore.Cmd m_lastAST = null;
        private bool m_showOriginalVarNamesInAST = false;

        public CoreResetForm()
        {
            InitializeComponent();
            m_analyseToolTip = new ToolTip();
            m_analyseToolTip.SetToolTip(buttonAnalyse, "Analyse program");
            m_generalToolTip = new ToolTip();
            m_generalToolTip.SetToolTip(buttonOpenCodeFile, "Open Core Language program file");
        }

        private void buttonOpenCodeFile_Click(object sender, EventArgs e)
        {
            OpenFileDialog dlg = new OpenFileDialog();
            dlg.Multiselect = false;
            dlg.Filter = "Core language program file (*.core)|*.core";
            if (dlg.ShowDialog() == DialogResult.OK)
            {
                CodeTextBox.Text = File.ReadAllText(dlg.FileName); 
            }
        }

        private void setResultText(string s)
        {
            textBoxAnalysisResult.Lines = s.Split('\n'); 
        }


        private void addASTNode(CmdTreeNode parent, BJKCore.Cmd c)
        {
            CmdTreeNode child = null;
            if (c.IsChoice)
            {
                child = new CmdTreeNode("Choose {...} or {...}");
                BJKCore.Cmd.Choice choice = c as BJKCore.Cmd.Choice;
                addASTNode(child, choice.Item1);
                addASTNode(child, choice.Item2);
            }
            else if (c.IsAssume)
            {
                BJKCore.Cmd.Assume assume = c as BJKCore.Cmd.Assume;
                string boolExpr = boolExprString(assume.Item);
                child = new CmdTreeNode("Assume (" + boolExpr + ")");
            }
            else if (c.IsAssumeNot)
            {
                BJKCore.Cmd.AssumeNot assumeNot = c as BJKCore.Cmd.AssumeNot;
                string coBoolExp = coBoolExprString(assumeNot.Item);
                child = new CmdTreeNode("Assume (" + coBoolExp + ")");
            }
            else if (c.IsWhile)
            {
                BJKCore.Cmd.While whle = c as BJKCore.Cmd.While;
                string boolExpr = boolExprString(whle.Item1);
                child = new CmdTreeNode("While (" + boolExpr + ")");
                addASTNode(child, whle.Item2);
            }
            else if (c.IsSeq)
            {
                child = new CmdTreeNode("Sequence");
                BJKCore.Cmd.Seq seq = c as BJKCore.Cmd.Seq;
                addASTNode(child, seq.Item1);
                addASTNode(child, seq.Item2);
            }
            else if (c.IsSkip)
            {
                child = new CmdTreeNode("Skip");
            }
            else if (c.IsAsgn)
            {
                BJKCore.Cmd.Asgn asgn = c as BJKCore.Cmd.Asgn;
                string expr = exprString(asgn.Item2);
                int varIndex = asgn.Item1;
                string varName = getVarNameByVarIndex(varIndex);
                child = new CmdTreeNode(varName + " := " + expr);
            }
            else if (c.IsLoop)
            {
                BJKCore.Cmd.Loop loop = c as BJKCore.Cmd.Loop;
                child = new CmdTreeNode("Loop*");
                addASTNode(child, loop.Item);
            }
            else if (c.IsWidening)
            {
                child = new CmdTreeNode("Widening");
            }
            

            parent.Nodes.Add(child);
        }

        private string getVarNameByVarIndex(int varIndex)
        {
            string varStr = "X" + varIndex.ToString();
            if (m_showOriginalVarNamesInAST && BJKParse.parserLastVarToIndexMap().ContainsKey(varIndex))
            {
                varStr = BJKParse.parserLastVarToIndexMap()[varIndex];
            }
            return varStr; 
        }

        private string exprString(BJKCore.Expr expr)
        {
            string res = null;
            if (expr.IsFix)
            {
                BJKCore.Expr.Fix f = expr as BJKCore.Expr.Fix;
                res = f.Item.ToString();
            }
             else if (expr.IsVar)
            {
                BJKCore.Expr.Var v = expr as BJKCore.Expr.Var;
                res = getVarNameByVarIndex(v.Item);
            }
           else if (expr.IsPlus)
            {
                BJKCore.Expr.Plus plus = expr as BJKCore.Expr.Plus;
                res = exprString(plus.Item1) + " + " + exprString(plus.Item2);
            }
            else if (expr.IsMinus)
            {
                BJKCore.Expr.Minus minus = expr as BJKCore.Expr.Minus;
                res = exprString(minus.Item1) + " - " + exprString(minus.Item2);
            }
            else if (expr.IsTimes)
            {
                BJKCore.Expr.Times times = expr as BJKCore.Expr.Times;
                res = exprString(times.Item1) + " * " + exprString(times.Item2);
            }
            return res;
        }

        private string boolExprString(BJKCore.BoolExp boolExpr)
        {
            string res = null;
            if (boolExpr.IsTrue)
            {
                res = "TRUE";
            }
            else if (boolExpr.IsFalse)
            {
                res = "FALSE";
            }
            else if (boolExpr.IsEqual)
            {
                BJKCore.BoolExp.Equal b = boolExpr as BJKCore.BoolExp.Equal;
                res = exprString(b.Item1) + " == " + exprString(b.Item2);
            }
            else if (boolExpr.IsNotEqual)
            {
                BJKCore.BoolExp.NotEqual b = boolExpr as BJKCore.BoolExp.NotEqual;
                res = exprString(b.Item1) + " != " + exprString(b.Item2);
            }
            else if (boolExpr.IsGreat)
            {
                BJKCore.BoolExp.Great b = boolExpr as BJKCore.BoolExp.Great;
                res = exprString(b.Item1) + " > " + exprString(b.Item2);
            }
            else if (boolExpr.IsGreatEqual)
            {
                BJKCore.BoolExp.GreatEqual b = boolExpr as BJKCore.BoolExp.GreatEqual;
                res = exprString(b.Item1) + " >= " + exprString(b.Item2);
            }
            else if (boolExpr.IsLess)
            {
                BJKCore.BoolExp.Less b = boolExpr as BJKCore.BoolExp.Less;
                res = exprString(b.Item1) + " < " + exprString(b.Item2);
            }
            else if (boolExpr.IsLessEqual)
            {
                BJKCore.BoolExp.LessEqual b = boolExpr as BJKCore.BoolExp.LessEqual;
                res = exprString(b.Item1) + " <= " + exprString(b.Item2);
            }
            return res;
        }

        private string coBoolExprString(BJKCore.BoolExp boolExpr)
        {
            string res = null;
            if (boolExpr.IsTrue)
            {
                res = "FALSE";
            }
            else if (boolExpr.IsFalse)
            {
                res = "TRUE";
            }
            else if (boolExpr.IsEqual)
            {
                BJKCore.BoolExp.Equal b = boolExpr as BJKCore.BoolExp.Equal;
                res = exprString(b.Item1) + " != " + exprString(b.Item2);
            }
            else if (boolExpr.IsNotEqual)
            {
                BJKCore.BoolExp.NotEqual b = boolExpr as BJKCore.BoolExp.NotEqual;
                res = exprString(b.Item1) + " == " + exprString(b.Item2);
            }
            else if (boolExpr.IsGreat)
            {
                BJKCore.BoolExp.Great b = boolExpr as BJKCore.BoolExp.Great;
                res = exprString(b.Item1) + " <= " + exprString(b.Item2);
            }
            else if (boolExpr.IsGreatEqual)
            {
                BJKCore.BoolExp.GreatEqual b = boolExpr as BJKCore.BoolExp.GreatEqual;
                res = exprString(b.Item1) + " < " + exprString(b.Item2);
            }
            else if (boolExpr.IsLess)
            {
                BJKCore.BoolExp.Less b = boolExpr as BJKCore.BoolExp.Less;
                res = exprString(b.Item1) + " >= " + exprString(b.Item2);
            }
            else if (boolExpr.IsLessEqual)
            {
                BJKCore.BoolExp.LessEqual b = boolExpr as BJKCore.BoolExp.LessEqual;
                res = exprString(b.Item1) + " > " + exprString(b.Item2);
            }
            return res;
        }

        private void updateAST(BJKCore.Cmd ast)
        {
            ASTreeView.Nodes.Clear();
            ASTreeView.Nodes.Add(new CmdTreeNode("Program"));
            addASTNode(ASTreeView.TopNode as CmdTreeNode, ast);
            ASTreeView.ExpandAll();
        }
         
        private void Analyze()
        {
            try
            {
                m_lastAST = BJKParse.astBreakNestedExpressions(BJKParse.aString(CodeTextBox.Text));
                updateAST(m_lastAST);
                textBoxAnalysisResult.Clear();
                textBoxAnalysisResult.Text = "Analysing...\n";
                Analysis.AbstractSemantics.resultType result = Analysis.AbstractSemantics.analyse(m_lastAST);
                Console.WriteLine(result.toString());
                textBoxAnalysisResult.Multiline = true;
                textBoxAnalysisResult.Text = result.toString();
            }
            catch (Util.Fail f)
            {
                MessageBox.Show(f.Data0, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void buttonAnalyse_Click(object sender, EventArgs e)
        {          
            Analyze();
        }

        private void buttonAbout_Click(object sender, EventArgs e)
        {
            AboutForm frm = new AboutForm();
            frm.ShowDialog();
        }

        private void checkBoxShowOriginalVarNames_CheckedChanged(object sender, EventArgs e)
        {
            m_showOriginalVarNamesInAST = checkBoxShowOriginalVarNames.Checked;
            if (ASTreeView.Nodes.Count > 0 && m_lastAST != null)
            {
                updateAST(m_lastAST);
            }
        }      
    }
}
