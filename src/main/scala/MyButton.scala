

package main.scala

import javax.swing.JButton
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import java.awt.Font
import javax.swing.JPanel
import java.awt.Dimension

class MyButton(p: JPanel,width:Int)(text: String, f: () => Unit) extends JButton {
  init(text, null)
  setFont(new Font("メイリオ", java.awt.Font.BOLD, width/11))
  setPreferredSize(new Dimension(width,30))
  addActionListener(new ActionListener() {
    override def actionPerformed(e: ActionEvent) {
      f()
      p.repaint()
    }
  })
  setFocusable(false)
}