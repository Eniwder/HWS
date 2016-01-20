

package main.scala

import javax.swing.JPanel
import java.awt.Color
import java.awt.Graphics
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.Font
import java.awt.BasicStroke
import javax.swing.JLabel
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import javax.swing.JFileChooser
import java.io.PrintWriter
import scala.swing.Dialog.Result._
import scala.swing.Dialog

class KarnaughPanel(width: Int, height: Int, map: KarnaughMap, message: JLabel) extends JPanel with OpUtil {
  val W0, H0 = 4
  val SEP = " "
  val FONT_NAME = "Courier New"
  val rectW = width - 10
  val rectH = height - 10
  val oneW = rectW / (map.collist.length + 1)
  val oneH = rectH / (map.rawlist.length + 1)
  val dfsize = 36 * oneW / 96
  val g = getGraphics

  setPreferredSize(new Dimension(width, height))

  // 10=Enter , 83=S , F1=112 , 127=delete
  def keypressed(code: Int) {
    code match {
      case 10  => makeCircle()
      case 83  => save()
      case 112 => autoMakeCircle()
      case 127 => clear()
      case _   =>
    }
    repaint()
  }

  def msg(txt: String) { message.setText(txt) }

  def autoMakeCircle() {
    msg("自動でかこむよ！自分でも確かめてね！！")
    map.resetCircle()
    map.autoSurround()
    map.resetSelect()
  }

  def makeCircle() {
    if (!map.isPowOfTwo()) { msg("囲むセルの数がおかしいよ"); return }
    if (map.containsZero()) { msg("0を囲んでるよ"); return }
    if (!map.isAdjacent()) { msg("隣り合っていないセルを囲おうとしてるよ"); return }
    if (map.existCircle()) {
      map.deleteCircle()
      map.resetSelect()
      msg("選択した輪っかを消しました");
      return
    }
    msg("囲むよ")
    map.encircle()
    map.resetSelect()
  }

  def clear() {
    msg("リセットしたよ")
    map.resetSelect()
    map.resetCircle()
  }

  // 現在の状態を保存
  def save() {
    val fc = new JFileChooser() {
      // 同名ファイルがあった時に選択ダイアログを出す
      override def approveSelection() {
        val path = getCurrentDirectory() + "/" + getSelectedFile().getName()
        val mathf = new File(path + ".txt")
        val imgf = new File(path + ".png")
        if (HWS.isFOWCheck && (mathf.exists() || imgf.exists())) {
          Dialog.showConfirmation(title = "同名のファイルが存在しています", message = "ファイルを上書きしますか？") match {
            case Ok => super.approveSelection()
            case _  =>
          }
        } else {
          super.approveSelection()
        }
      }
    }
    fc.setSelectedFile(new File(HWS.lastSavePath, map.outName))
    fc.showSaveDialog(this) match {
      case JFileChooser.APPROVE_OPTION => { // 保存ボタンを押したら保存、それ以外はキャンセル
        val tn = fc.getSelectedFile().getName()
        val image = new BufferedImage(getWidth(), getHeight() - 2, BufferedImage.TYPE_INT_RGB);
        val g2 = image.createGraphics();
        this.paint(g2);
        g2.dispose();
        val dir = fc.getCurrentDirectory()
        HWS.lastSavePath = dir toString ()
        var ms = (if (HWS.isImgOut) " 画像" else "") + (if (HWS.isMathOut) " 数式" else "")
        msg(s"[ $tn ] $ms を出力します")
        if (HWS.isImgOut) writeImage(image, "png", new File(s"${dir}/${tn}.png"));
        if (HWS.isMathOut) writeMathFormula(dir + "/" + tn)
      }
      case _ => msg("出力をキャンセルしました")
    }
  }

  // 画像を出力
  def writeImage(img: BufferedImage, typ: String, f: File) {
    try ImageIO.write(img, typ, f)
    catch { case e: Exception => Dialog.showMessage(title = "例外発生", message = "画像の出力に失敗しました。") }
  }

  // 数式を作成してtxtファイルで出力
  def writeMathFormula(path: String) {
    val f = if (HWS.isWordMF) partWordSection _ else partTexSection _ // 設定により使用する関数を決める
    val res = map.getCircleSet().toList sortWith (_.length > _.length) map (f) mkString ("+")
    using(new PrintWriter(path + ".txt")) {
      _.print(s"${map.outName} = $res")
    }
  }

  // Word形式で項を作成
  def partWordSection(list: List[String]): String = {
    val sec = for (i <- 0 until list(0).length if (list :\ true)(_(i) == list(0)(i) && _)) yield {
      if (list(0)(i) == '0') "¯" + map.inNames(i) + SEP else map.inNames(i)
    }
    if (sec.isEmpty) "1" else sec.mkString(SEP)
  }

  // Tex形式で項を作成
  def partTexSection(list: List[String]): String = {
    val sec = for (i <- 0 until list(0).length if (list :\ true)(_(i) == list(0)(i) && _)) yield {
      if (list(0)(i) == '0') s"\\overline{${map.inNames(i)}}" else map.inNames(i)
    }
    if (sec.isEmpty) "1" else sec.mkString(SEP)
  }

  // クリックされたセルを求めて選択状態にする
  def clicked(x: Int, y: Int) {
    val xi = (x - W0 - 4) / oneW - 1
    val yi = (y - H0 - 24) / oneH - 1
    if (xi < 0 || map.collist.length - 1 < xi ||
      yi < 0 || map.rawlist.length - 1 < yi) {
      return
    }
    val selectCell = if (HWS.isVarDesc) map.rawlist(yi) + map.collist(xi) else map.collist(xi) + map.rawlist(yi)
    map.select(selectCell)
    repaint()
  }

  override def paint(g: Graphics) {
    val g2 = g.asInstanceOf[Graphics2D];
    // 色々設定
    g2.setColor(Color.white);
    g2.setBackground(Color.WHITE)
    g2.clearRect(0, 0, width, height)
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2.setColor(Color.BLACK);

    // カルノー図の格子を作成
    printLattice()
    // 入力名を表示
    printInputNames()
    // 入力ビットパターンを表示
    printInputBits()
    // それぞれの値
    printValues()
    // 囲んだ値を円でくくる
    printCircle()

    // 文字列のサイズを決める
    def strSize(g2: Graphics2D, str: String = "0") = {
      (g2.getFontMetrics().stringWidth(str), g2.getFontMetrics().getHeight() * 0.6 toInt)
    }

    // 輪たちを描く
    def printCircle() {
      val set = map.getCircleSet()
      var min = 0
      for (list <- set) {
        val xs = (for (p <- list) yield getPoint(p)._1) toSet;
        val ys = (for (p <- list) yield getPoint(p)._2) toSet;
        if (xs.size == 2 && (xs.max - xs.min) > 1 && ys.size == 2 && (ys.max - ys.min) > 1) { // 四隅が選択されている場合
          drawCircle(xs.min - 1, ys.min - 1, 2, 2, 0, -90)
          drawCircle(xs.max, ys.min - 1, 2, 2, 180, 90)
          drawCircle(xs.min - 1, ys.max, 2, 2, 0, 90)
          drawCircle(xs.max, ys.max, 2, 2, 180, -90)
        } else if (xs.size == 2 && (xs.max - xs.min) > 1) { // 横の端と端が選択されている場合
          drawCircle(xs.min - 1, ys.min, 2, ys.max - ys.min + 1, 90, -180)
          drawCircle(xs.max, ys.min, 2, ys.max - ys.min + 1, 90, 180)
        } else if (ys.size == 2 && (ys.max - ys.min) > 1) { // 縦の端と端が選択されている場合
          drawCircle(xs.min, ys.min - 1, xs.max - xs.min + 1, 2, 0, -180)
          drawCircle(xs.min, ys.max, xs.max - xs.min + 1, 2, 0, 180)
        } else if (xs.size == map.collist.size && ys.size == map.rawlist.size) { // 全部選択されている場合
          g2.drawRoundRect(oneW + W0 + 10, oneH + H0 + 10,
            oneW * xs.size - 20, oneH * ys.size - 20, 100, 100)
        } else {
          drawCircle(xs.min, ys.min, (xs.max - xs.min + 1), (ys.max - ys.min + 1))
        }

      }
    }

    // 円を描く
    def drawCircle(x: Int, y: Int, w: Int, h: Int, sd: Int = 0, ed: Int = 360) {
      g2.drawArc(x * oneW + 3 + W0, y * oneH + 3 + H0, w * oneW - 6, h * oneH - 6, sd, ed)
    }

    // 押された文字列から座標を求める
    def getPoint(s: String): (Int, Int) = {
      val (x, y) = if (HWS.isVarDesc) flipTuple(s.splitAt(map.raw)) else s.splitAt(map.col)
      (map.collist.indexOf(x) + 1, map.rawlist.indexOf(y) + 1)
    }

    // 入力名を描く
    def printInputNames() {
      val fsize = getFontSize(map.colnames) min (getFontSize(map.rawnames))
      g2.setFont(new Font(FONT_NAME, Font.PLAIN, fsize))
      val (w, h) = strSize(g2)
      var p = if (map.colnames.length == 1) 5 + w / 4 + w / 2 else 5
      for (str <- map.colnames) {
        g2.drawString(str, p + W0 + oneW / 3, H0 + h + h / 4)
        p += w * str.length() + w / 2
      }
      p = 0
      for (str <- map.rawnames) {
        g2.drawString(str, p + W0 + w / 3, H0 + oneH - h / 2)
        p += w * str.length + w / 2
      }
    }

    // フォントサイズを決める
    def getFontSize(names: List[String]) = {
      var length = 0.0
      for (name <- names) length += name.length
      if (length == 1) length = 1.5
      oneH.min(oneW) * 0.8 / length toInt
    }

    // 入力値を描く
    def printInputBits() {
      val div = if (log2(map.collist.length) <= 1) 1 else log2(map.collist.length) - 1
      val fsize = dfsize / div
      g2.setFont(new Font(FONT_NAME, Font.PLAIN, fsize))
      val (w, h) = strSize(g2, map.collist(0))
      printHorizon(map.collist, W0 + oneW + (oneW - w) / 2, H0 + (oneH + h) / 2, oneW)
      val (w2, h2) = strSize(g2, map.rawlist(0))
      printVertical(map.rawlist, W0 + (oneW - w2) / 2, H0 + oneH + (oneH + h2) / 2, oneH)
    }

    // 水平に文字を描く
    def printHorizon(list: List[String], sx: Int, sy: Int, vx: Int) {
      for (i <- 0 until list.length) g2.drawString(list(i), sx + vx * i, sy)
    }

    // 垂直に文字を描く
    def printVertical(list: List[String], sx: Int, sy: Int, vx: Int) {
      for (i <- 0 until list.length) g2.drawString(list(i), sx, sy + vx * i)
    }

    // 枠を描く
    def printLattice() {
      if (HWS.isSetFrame) {
        g2.setStroke(new BasicStroke(3))
        g2.drawRect(W0, H0, rectW, rectH)
        g2.setStroke(new BasicStroke(2))
        g2.drawLine(oneW + W0, H0, oneW + W0, H0 + rectH)
        g2.drawLine(W0, oneH + H0, W0 + rectW, oneH + H0)
        g2.setStroke(new BasicStroke())
        for (i <- 2 to map.collist.length)
          g2.drawLine(i * oneW + W0, H0, i * oneW + W0, H0 + rectH)
        for (i <- 2 to map.rawlist.length)
          g2.drawLine(W0, i * oneH + H0, W0 + rectW, i * oneH + H0)
        g2.drawLine(W0, H0, W0 + oneW, H0 + oneH)
      } else {
        g2.setStroke(new BasicStroke(2))
        g2.drawRect(W0 + oneW, H0 + oneH, rectW - oneW, rectH - oneH)
        g2.setStroke(new BasicStroke())
        for (i <- 2 to map.collist.length)
          g2.drawLine(i * oneW + W0, H0 + oneH, i * oneW + W0, H0 + rectH)
        for (i <- 2 to map.rawlist.length)
          g2.drawLine(W0 + oneW, i * oneH + H0, W0 + rectW, i * oneH + H0)
        g2.drawLine(W0, H0, W0 + oneW, H0 + oneH)
      }

    }

    // 値を描き込んでいく
    def printValues() {
      g2.setFont(new Font(FONT_NAME, Font.BOLD, dfsize))
      val (w, h) = strSize(g2)
      for (i <- 0 until map.collist.length; j <- 0 until map.rawlist.length) {
        val in = if (HWS.isVarDesc) map.rawlist(j) + map.collist(i) else map.collist(i) + map.rawlist(j)
        if (map.isSelected(in)) g2.setColor(Color.RED) else g2.setColor(Color.BLACK)
        g2.drawString(map.get(in), W0 + (i + 1) * oneW + (oneW - w) / 2, H0 + (j + 2) * oneH - (oneH - h) / 2)
      }
      g2.setColor(Color.BLACK)
    }

  }
}

