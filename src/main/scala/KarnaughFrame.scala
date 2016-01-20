package main.scala

import javax.swing.JPanel
import javax.swing.JFrame
import java.awt.Color
import java.awt.event.MouseEvent
import java.awt.event.MouseAdapter
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import javax.swing.BoxLayout
import javax.swing.WindowConstants
import javax.swing.JLabel
import java.awt.FlowLayout
import java.awt
import java.awt.Font

class KarnaughFrame(w: Int, h: Int, map: KarnaughMap) extends JFrame with OpUtil {
  val BACK_COLOR = new Color(0xCE, 0xEC, 0xF5)
  val kWidth: Int = w * (map.collist.length + 1)
  val kHeight: Int = h * (map.rawlist.length + 1)

  setTitle(map.outName + "のカルノー図")
  val fHeight = if (map.collist.length > 2) kHeight + 26 + 45 + 30 else kHeight + 26 + 45 + 30 + 84
  setSize(kWidth + 5, fHeight);
  setLayout(new BoxLayout(this.getContentPane(), BoxLayout.Y_AXIS))
  setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
  setResizable(false)

  // メッセージ用のラベル
  val fn = if (HWS.fnames.contains("メイリオ")) "メイリオ" else "ヒラギノ角ゴ"
  val message = new JLabel(" ") { setFont(new Font(fn, java.awt.Font.BOLD, 12)) }

  // カルノー図を表示するパネル
  val kp = new KarnaughPanel(kWidth, kHeight, map, message)
  // マウスイベントとキーイベントを登録
  addMouseListener(new MouseAdapter() {
    override def mousePressed(e: MouseEvent) {
      kp.clicked(e.getX, e.getY)
    }
  })
  addKeyListener(new KeyAdapter() {
    override def keyPressed(e: KeyEvent) {
      kp.keypressed(e.getKeyCode())
    }
  })
  add(kp)

  // ボタン用のパネル
  val buttonPanel = new JPanel {
    setBackground(BACK_COLOR)
    setLayout(new FlowLayout)
  }
  val bSize = if (map.collist.length > 2) kWidth / 4 - 10 else kWidth / 2 - 10
  val tb = new MyButton(kp, bSize)(_, _) // なんとなく部分適用を使ってみた
  buttonPanel.add(tb("囲む(Enter)", kp.makeCircle))
  buttonPanel.add(tb("リセット(del)", kp.clear))
  buttonPanel.add(tb("保存(S)", kp.save))
  buttonPanel.add(tb("おまかせ(F1)", kp.autoMakeCircle))
  add(buttonPanel)

  val messagePanel = new JPanel {
    setBackground(BACK_COLOR)
    setLayout(new FlowLayout)
  }
  messagePanel.add(message)
  add(messagePanel)

  setVisible(true)

}
