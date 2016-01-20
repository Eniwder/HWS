

package main.scala

import swing._
import scala.swing._
import javax.swing.JTable

class Spreadsheet(val height: Int, val width: Int, val in: Int, val out: Int) extends ScrollPane with OpUtil {

  val START_COL = 3
  val START_RAW = 2
  val ESC = "<@>"

  // Cellのこと
  val table = new Table(height, width) {
    rowHeight = 20 // セルの縦幅
    for (col <- 0 until width) peer.getColumnModel().getColumn(col).setPreferredWidth(20) // セルの横幅
    showGrid = true // 枠線を表示
    gridColor = new java.awt.Color(150, 150, 150)
    selectionBackground = java.awt.Color.lightGray
    peer.setAutoResizeMode(JTable.AUTO_RESIZE_OFF) // セルの自動サイズ調整をしない
    peer.getTableHeader().setReorderingAllowed(false) // ヘッダの入れ替えを禁止
    // セルの選択方法をExcelっぽく変更
    selection.intervalMode = Table.IntervalMode.SingleInterval //MultiInterval
    selection.elementMode = Table.ElementMode.Cell
    listenTo(this)
    reactions += {
      case event.TableUpdated(src, rows, col) => { // 出力値は1文字しか入力できない、後の文字を優先
        val v = src.model.getValueAt(rows.head, col).toString
        if (v.length() > 1 && rows.head > (START_RAW + 1) && col >= START_COL &&
          col < START_COL + in + out) src.model.setValueAt(v.reverse.head, rows.head, col)
      }
      case _ =>
    }
  }

  val tm = table.model
  val tp = table.peer

  // 表を作成
  genTruthTable()

  // 行名
  val rowHeader = new ListView((0 until height) map (_.toString)) {
    fixedCellWidth = 30
    fixedCellHeight = table.rowHeight
  }

  // スクロールの対応
  viewportView = table
  rowHeaderView = rowHeader

  // セルに入力欄を出力
  def genTruthTable() {
    for (c <- 0 until width; r <- 0 until height) {
      tm.setValueAt("", r, c)
      tp.getColumnModel().getColumn(c).setMinWidth(30)
    }
    tp.setDefaultRenderer(classOf[Any], new MyRenderer(START_COL, in + START_COL - 1,
      in + out + START_COL - 1, START_RAW, START_RAW + 1 + powOfTwo(in)))
    for (i <- 0 until in) {
      val line = makeTTcol(zeroAndOne(powOfTwo(in - i - 1)), powOfTwo(i))
      tm.setValueAt("入力", START_RAW, i + START_COL)
      tp.getColumnModel().getColumn(i + 3).setMinWidth(30)
      for (j <- 0 until line.length()) tm.setValueAt(line(j), j + START_RAW + 2, i + START_COL)
      for (y <- 0 until out) tm.setValueAt("出力", START_RAW, i + START_COL + 1 + y)
    }
  }

  // 仮
  //def genStateTable(in: Int, out: Int) {}

  // セルを削除
  //  def clearTable() {
  //    for (c <- 0 until width; r <- 0 until height) {
  //      if (r == 3) tm.setValueAt("", 3, c)
  //      if (r != 2 && 3 + in + c < width) tm.setValueAt("", r, 3 + in + c)
  //    }
  //  }

  // ファイル保存用にデータを渡す、以下にフォーマット
  // 1行目:入力数,出力数
  // 2行目:各セルの値が','区切りで並ぶ、元々','だったらESCに変換しとく
  def getDate(): String = {
    val data = for (j <- 0 until height; i <- 0 until width)
      yield tm.getValueAt(j, i).toString().replaceAll(",", ESC)
    in + "," + out + "\n" + data.mkString(",")
  }

  // 文字列からテーブルに値を読み込む
  def load(data: String) {
    val values = data.split(',')
    for (i <- 0 until height; j <- 0 until width)
      tm.setValueAt(values.getOrBlank(i * width + j).replaceAll(ESC, ","), i, j)
  }

  // カルノー図のモデル部分をいただく
  def getKarnaughs(): List[KarnaughMap] = {
    val innamelist = List.range(START_COL, START_COL + in, 1) map (tm.getValueAt(START_COL, _).toString())
    val outnamelist = List.range(START_COL + in, START_COL + in + out, 1) map
      (tm.getValueAt(START_COL, _).toString()) collect { case "" => "O" case x => x }
    // 入力のリスト
    val inlist = for (r <- START_RAW + 2 until START_RAW + 2 + powOfTwo(in)) yield {
      val str = for (c <- START_COL until START_COL + in) yield tm.getValueAt(r, c).toString()
      str.mkString
    }
    // カルノー図の作成に必要な情報
    val maps = for (c <- START_COL + in until START_COL + in + out) yield {
      (for (r <- START_RAW + 2 until START_RAW + 2 + powOfTwo(in)) yield {
        if (tm.getValueAt(r, c) == "") inlist(r - START_RAW - 2) -> "0"
        else inlist(r - START_RAW - 2) -> tm.getValueAt(r, c).toString()
      }) toMap
    }
    // カルノー図のモデルを生成
    for (i <- List.range(0, maps.length, 1))
      yield new KarnaughMap(maps(i), innamelist, outnamelist(i), in);
  }

}