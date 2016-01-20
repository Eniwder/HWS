package main.scala

import scala.swing._
import javax.swing.UIManager
import java.util.Properties
import java.io.PrintWriter
import javax.swing.JFileChooser
import java.io.File
import scala.swing.Dialog.Result._
import javax.swing.filechooser.FileNameExtensionFilter
import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap
import scala.swing.event.ButtonClicked
import main.scala._
import scala.io.Source

object HWS extends SimpleSwingApplication with OpUtil {

  // インストールされてる物理フォントの名前一覧
  val fnames = java.awt.GraphicsEnvironment
    .getLocalGraphicsEnvironment().getAllFonts().toList map (_.getName())
  // UIManagerでデフォルトのフォントを設定
  setFont()
  // ボタンとかの見た目を変更
  setLookAndFeel()

  // ウィンドウ関係
  val WIDTH = 500
  val SP_HEIGHT = powOfTwo(6) + 10
  val SP_WIDTH = 30
  val DEFAULT_TITLE = "HardwareSupport"

  // 設定関係の定数
  val PROP_PATH = "./conf.ini"
  val FILE_WRITE = "fileWrite"
  val MATH_FOMULA = "mathFomula"
  val FILE_OUT = "fileOut"
  val KARNAUGH_FRAME = "kframe"
  val KARNAUGH_VARIABLE = "kvariable"
  val SAVE_PATH = "savePath"

  // 設定関係
  val prop = getPropaties
  var lastSavePath = savePath
  var isTruthTable = true

  // ファイルから読み込んでいたらその名前を保持
  var nowFile = ""

  // メニューバーのアイテム群1
  val fowCheck = new RadioButton("確認") { override def toString() = "check" }
  val fowIgnore = new RadioButton("無視して上書き") { override def toString() = "ignore" }
  val fowGroup = new ButtonGroup(fowCheck, fowIgnore) { select(fileWriteMode) }

  // メニューバーのアイテム群2
  val wordMF = new RadioButton("Word形式") { override def toString() = "word" }
  val texMF = new RadioButton("Tex形式") { override def toString() = "tex" }
  val mfGroup = new ButtonGroup(wordMF, texMF) { select(mathFormulaMode) }

  // メニューバーのアイテム群3
  val mathFile = new RadioButton("数式のみ") { override def toString() = "math" }
  val imgFile = new RadioButton("画像のみ") { override def toString() = "img" }
  val allFile = new RadioButton("数式と画像") { override def toString() = "all" }
  val ofGroup = new ButtonGroup(mathFile, imgFile, allFile) { select(fileOutMode) }

  // メニューバーのアイテム群4
  val setFrameLine = new RadioButton("枠をつける") { override def toString() = "setFrame" }
  val clearFrameLine = new RadioButton("枠の一部を消去") { override def toString() = "clearFrame" }
  val setFrameGroup = new ButtonGroup(setFrameLine, clearFrameLine) { select(kFrameMode()) }

  // メニューバーのアイテム群5
  val variableDesc = new RadioButton("AB＼CD") { override def toString() = "desc" }
  val variableAsc = new RadioButton("CD＼AB") { override def toString() = "asc" }
  val karnaughVariableSortGroup = new ButtonGroup(variableDesc, variableAsc) { select(kVariableMode()) }

  // メニューバーで何が選択されているかを取得する関数群
  def isFOWCheck = fowGroup.selected.getOrElse(fowCheck) == fowCheck
  def isWordMF = mfGroup.selected.getOrElse(wordMF) == wordMF
  def isMathOut = ofGroup.selected.getOrElse(allFile) == mathFile || ofGroup.selected.getOrElse(allFile) == allFile
  def isImgOut = ofGroup.selected.getOrElse(allFile) == imgFile || ofGroup.selected.getOrElse(allFile) == allFile
  def isSetFrame = setFrameGroup.selected.getOrElse(setFrameLine) == setFrameLine
  def isVarDesc = karnaughVariableSortGroup.selected.getOrElse(variableDesc) == variableDesc

  // 真理値表を作成する関数のクロージャ
  val tableGen = truthTableGen()
  // タブペイン
  val tabPane = new TabbedPane { preferredSize = new Dimension(WIDTH, 500) }
  // タブと真理値表を関連付けるマップ
  val tabToTable: HashMap[TabbedPane.Page, Spreadsheet] = new HashMap()

  def top = main
  val main = new MainFrame {
    title = DEFAULT_TITLE
    override def closeOperation() = quit() // 閉じるボタンの処理

    // メニューバー
    menuBar = new MenuBar() {
      contents += new Menu("ファイル") {
        contents += new MenuItem(Action("ファイルから読み込み") { readFile(clear = true) })
        contents += new MenuItem(Action("ファイルから追加で読み込み") { readFile(clear = false) })
        contents += new MenuItem(Action("上書き保存") { overWriteSave() })
        contents += new MenuItem(Action("名前をつけて保存") { saveAs() })
      }
      contents += new Menu("設定") {
        contents += new Menu("カルノー図からファイルの上書き保存方法") { contents += (fowCheck, fowIgnore) }
        contents += new Menu("カルノー図から保存するファイルの種類") { contents += (mathFile, imgFile, allFile) }
        contents += new Menu("カルノー図に枠をつけるか") { contents += (setFrameLine, clearFrameLine) }
        contents += new Menu("カルノー図の変数を書く順番(真理値表の入力が左からABCDの時)") { contents += (variableDesc, variableAsc) }
        contents += new Menu("数式の出力形式") { contents += (wordMF, texMF) }
      }
    }

    // メインとなるパネル
    contents = new BoxPanel(Orientation.Vertical) {
      //    val truthTableButton = new RadioButton("真理値表")
      //     val stateTableButton = new RadioButton("拡大入力要求表")
      val inputLabel = new Label("入力数") //{ // 選択した表によって入力数ラベルの表示を変更する
      //        reactions += {
      //          case ButtonClicked(b) if b == truthTableButton =>
      //            text = "入力数"
      //            isTruthTable = true
      //          case ButtonClicked(b) if b == stateTableButton =>
      //            text = "状態数"
      //            isTruthTable = false
      //        }
      //      }
      //      inputLabel.listenTo(truthTableButton, stateTableButton) // ボタンのイベントをそれぞれ監視
      //
      //      // 表の種類を選択するパネル
      //      contents += new FlowPanel() {
      //        // ラジオボタン
      //        val group = new ButtonGroup(truthTableButton, stateTableButton) { select(truthTableButton) }
      //        //  contents += truthTableButton
      //        // contents += stateTableButton
      //        maximumSize = new Dimension(WIDTH, 100)
      //      }

      // 入力数と出力数を選択するパネル
      val InputNum = new ComboBox(List("2", "3", "4")) { selection.index = 2 }
      val OutputNum = new ComboBox(List("1", "2", "3", "4", "5", "6", "7", "8"))
      contents += new GridPanel(2, 2) {
        maximumSize = new Dimension(WIDTH, 100)
        contents += inputLabel
        contents += InputNum
        contents += new Label("出力数")
        contents += OutputNum
      }

      // 真理値表を作成するボタン
      val genButton = new Button("真理値表を作成") {
        reactions += { case e: ButtonClicked => tableGen(tabPane, InputNum.decode, OutputNum.decode) }
      }
      // 真理値表を削除するボタン
      val delButton = new Button("真理値表を削除") {
        reactions += { case e: ButtonClicked => deleteTable() }
      }
      // タブをリネームするボタン
      val renameButton = new Button("タブをリネーム") {
        reactions += { case e: ButtonClicked => renameTab() }
      }
      // 真理値表を操作するボタンのパネル
      contents += new FlowPanel() {
        maximumSize = new Dimension(WIDTH, 100)
        contents += (genButton, delButton, renameButton)
      }
      // カルノー図を作成するボタン
      val makeButton = new Button("カルノー図を作成") {
        reactions += { case e: ButtonClicked => makeKarnaugh(mapw.decode, maph.decode) }
      }
      val mapw = new ComboBox(List("60", "80", "100", "120", "140", "160", "180", "200")) { selection.index = 1 }
      val maph = new ComboBox(List("60", "80", "100", "120", "140", "160", "180", "200")) { selection.index = 1 }

      contents += tabPane

      // スプレッドシートを作成
      val spsheet = new Spreadsheet(SP_HEIGHT, SP_WIDTH, InputNum.decode, OutputNum.decode)
      tabPane.pages += new TabbedPane.Page("sheet0", spsheet)
      tabToTable += (tabPane.pages.head -> spsheet)

      // カルノー図を作成するボタンたち
      contents += new FlowPanel() {
        maximumSize = new Dimension(WIDTH, 100)
        contents += makeButton // カルノー図作成
        contents += new Label("横幅")
        contents += mapw // カルノー図の横幅
        contents += new Label("縦幅")
        contents += maph //カルノー図の縦幅
      }
    }
  }

  // 新たに真理値表を作成する、クロージャカッコいい
  def truthTableGen() = {
    var n = 0
    (p: TabbedPane, in: Int, out: Int) => {
      n += 1
      val sp = new Spreadsheet(SP_HEIGHT, SP_WIDTH, in, out)
      val p = new TabbedPane.Page("sheet" + n, sp)
      tabPane.pages += p
      tabToTable += (p -> sp)
    }
  }

  // 真理値表を削除する
  def deleteTable() = tabPane.selection.index match {
    case -1 =>
    case x => {
      tabToTable -= tabPane.pages(x)
      tabPane.pages.remove(x)
    }
  }

  // タブをリネーム
  def renameTab() = tabPane.selection.index match {
    case -1 =>
    case x => {
      Dialog.showInput(message = "新しい名前を入力してください", initial = tabPane.pages(x).title) match {
        case Some(n) => tabPane.pages(x).title = n
        case None    =>
      }
    }
  }

  // カルノー図を作成する
  def makeKarnaugh(w: Int, h: Int) = tabPane.selection.index match {
    case -1 =>
    case x => {
      tabToTable.get(tabPane.pages(x)) match {
        case Some(x) => for (kn <- x.getKarnaughs) new KarnaughFrame(w, h, kn)
        case None    =>
      }
    }
  }

  // プロパティファイルの情報をロード
  def getPropaties(): Properties = {
    val p = new Properties()
    try {
      p.load(new java.io.FileInputStream(PROP_PATH))
    } catch {
      case _: Throwable =>
        initConf()
        p.load(new java.io.FileInputStream(PROP_PATH))
    }
    p
  }

  // ファイルのデフォルト書き込み形式を取得
  def fileWriteMode(): RadioButton = {
    prop.get(FILE_WRITE) match {
      case "check"  => fowCheck
      case "ignore" => fowIgnore
      case _        => fowCheck
    }
  }

  // ファイルのデフォルト出力形式を取得
  def fileOutMode(): RadioButton = {
    prop.get(FILE_OUT) match {
      case "math" => mathFile
      case "img"  => imgFile
      case "all"  => allFile
      case _      => allFile
    }
  }

  // 数式のデフォルト出力形式を取得
  def mathFormulaMode(): RadioButton = {
    prop.get(MATH_FOMULA) match {
      case "word" => wordMF
      case "tex"  => texMF
      case _      => wordMF
    }
  }

  // 数式のデフォルト出力形式を取得
  def kFrameMode(): RadioButton = {
    prop.get(KARNAUGH_FRAME) match {
      case "setFrame"   => setFrameLine
      case "clearFrame" => clearFrameLine
      case _            => setFrameLine
    }
  }

  // 数式のデフォルト出力形式を取得
  def kVariableMode(): RadioButton = {
    prop.get(KARNAUGH_VARIABLE) match {
      case "desc" => variableDesc
      case "asc"  => variableAsc
      case _      => variableDesc
    }
  }

  // 最後にファイルを保存したパスを取得
  def savePath(): String = {
    prop.get(SAVE_PATH) match {
      case x: String if new File(x).exists() => x
      case _                                 => new File(".").getAbsoluteFile().getParent()
    }
  }

  // 設定ファイルを初期化
  def initConf() {
    using(new PrintWriter(PROP_PATH)) { x =>
      x.println(s"$FILE_WRITE = $fowCheck")
      x.println(s"$FILE_OUT = $allFile")
      x.println(s"$MATH_FOMULA  = $wordMF")
      x.println(s"$KARNAUGH_FRAME  = $setFrameLine")
      x.println(s"$KARNAUGH_VARIABLE  = $variableDesc")
      x.println(s"$SAVE_PATH = ${new File(".").getAbsoluteFile().getParent().replace('\\', '/')}")
    }
  }

  // 設定ファイルを保存
  override def quit() {
    using(new PrintWriter(PROP_PATH)) { x =>
      x.println(s"$FILE_WRITE = ${fowGroup.selected.getOrElse(fowCheck)}")
      x.println(s"$FILE_OUT  = ${ofGroup.selected.getOrElse(allFile)}")
      x.println(s"$MATH_FOMULA = ${mfGroup.selected.getOrElse(wordMF)}")
      x.println(s"$KARNAUGH_FRAME = ${setFrameGroup.selected.getOrElse(setFrameLine)}")
      x.println(s"$KARNAUGH_VARIABLE = ${karnaughVariableSortGroup.selected.getOrElse(variableDesc)}")
      x.println(s"$SAVE_PATH = ${lastSavePath.replace('\\', '/')}")
    }
    super.quit
  }

  // 名前をつけて保存
  def saveAs() {
    val fc = new JFileChooser() {
      // 同名ファイルがあった時に選択ダイアログを出す
      override def approveSelection() {
        val path = getCurrentDirectory() + "/" + getSelectedFile().getName()
        if (new File(s"${path}.hws").exists()) {
          Dialog.showConfirmation(title = "同名のファイルが存在しています", message = "ファイルを上書きしますか？") match {
            case Ok => super.approveSelection()
            case _  =>
          }
        } else {
          super.approveSelection()
        }
      }
    }
    fc.setAcceptAllFileFilterUsed(false)
    fc.addChoosableFileFilter(new FileNameExtensionFilter("hwsファイル", "hws"))
    fc.setSelectedFile(new File(lastSavePath + "/ "))
    fc.showSaveDialog(null) match {
      case JFileChooser.APPROVE_OPTION => { // 保存ボタンを押したら保存、それ以外はキャンセル
        lastSavePath = fc.getCurrentDirectory() toString ()
        val tn = fc.getSelectedFile().getName()
        val fn = if (tn.endsWith(".hws")) tn else tn + ".hws"
        save(lastSavePath + "/" + fn)
      }
      case _ =>
    }
  }

  // 上書き保存
  def overWriteSave() { if (nowFile == "") saveAs() else save(nowFile) }

  // 与えられたパスにファイルを保存する
  def save(name: String) {
    using(new PrintWriter(name)) { x =>
      for (i <- 0 until tabPane.pages.length) {
        x.println(tabPane.pages(i).title)
        x.println(tabToTable(tabPane.pages(i)).getDate)
      }
    }
    setPath(name)
  }

  // ファイルから読み込む
  def readFile(clear: Boolean) {
    val fc = new JFileChooser()
    fc.setAcceptAllFileFilterUsed(false)
    fc.addChoosableFileFilter(new FileNameExtensionFilter("hwsファイル", "hws"))
    fc.setSelectedFile(new File(lastSavePath + "/ "))
    fc.showOpenDialog(null) match {
      case JFileChooser.APPROVE_OPTION => { // 保存ボタンを押したら保存、それ以外はキャンセル
        if (clear) {
          tabPane.pages.clear
          tabToTable.clear
        }
        lastSavePath = fc.getCurrentDirectory() toString ()
        val tn = fc.getSelectedFile().getName()
        val fn = if (tn.endsWith(".hws")) tn else tn + ".hws"
        load(lastSavePath + "/" + fn)
      }
      case _ =>
    }
  }

  // 与えられたパスからファイルを読み込む
  def load(name: String) {
    var src = Source.fromFile(name)
    using(src) { x =>
      val sps = parse(x.getLines toList)
      for (sp <- sps) {
        val p = new TabbedPane.Page(sp._1, sp._2)
        tabToTable += (p -> sp._2)
        tabPane.pages += p
      }
    }
    def parse(xs: List[String]): List[(String, Spreadsheet)] = xs match {
      case (name :: x :: y :: xs) =>
        val (in, out) = x.splitChar(',')
        val sp = new Spreadsheet(SP_HEIGHT, SP_WIDTH, in.toInt, out.toInt)
        sp.load(y)
        (name, sp) :: parse(xs)
      case _ => Nil
    }
    setPath(name)
  }

  // 読み込み・保存した際にパスを保存してタイトルにもセットする
  def setPath(path: String) {
    nowFile = path
    main.title = DEFAULT_TITLE + " - " + nowFile.replace('\\', '/')
  }

  // 見た目(Look&Feel)を変更
  def setLookAndFeel() {
    try { UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName()) }
  }

  // フォントのセット
  def setFont() {
    if (fnames contains ("メイリオ"))
      for (entry <- UIManager.getDefaults().entrySet() if entry.getKey().toString().endsWith(".font"))
        UIManager.put(entry.getKey(), new Font("メイリオ", java.awt.Font.PLAIN, 12))
    else if (fnames contains ("ヒラギノ角ゴ"))
      for (entry <- UIManager.getDefaults().entrySet() if entry.getKey().toString().endsWith(".font"))
        UIManager.put(entry.getKey(), new Font("ヒラギノ角ゴ", java.awt.Font.PLAIN, 12))
  }

}
