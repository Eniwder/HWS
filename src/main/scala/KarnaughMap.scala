/*       in      out
 * MAP = 0101 ->  1
 */
package main.scala

import scala.collection.mutable.HashSet
import scala.collection.mutable.Buffer

class KarnaughMap(val kmap: Map[String, String], val inNames: List[String],
                  val outName: String, val inputN: Int) extends OpUtil {

  // 列数、行数
  val col = if (HWS.isVarDesc) inputN / 2 else inputN - inputN / 2
  val raw = if (HWS.isVarDesc) inputN - inputN / 2 else inputN / 2
  // 列名、行名
  val (colnames, rawnames) = if (HWS.isVarDesc) flipTuple(inNames.splitAt(raw)) else inNames.splitAt(col)
  // グレイコードを作成
  val collist = makeGray(col)
  val rawlist = makeGray(raw)
  // 選択されているかを管理するMap
  import scala.collection.mutable.{ Map => MutableMap }
  private val selectMap = MutableMap() ++ (kmap.keys map ((_, false)) toMap)
  // 囲まれている値を保持するするSet
  private val circleSet = new HashSet[List[String]]()

  // グレイコードを作成する
  def makeGray(elems: Int) = {

    // グレイコードを作成するための補助メソッド、末尾再帰じゃない
    def makeLine(line: String, n: Int): String = n match {
      case 0 => ""
      case _ => line + makeLine(line.reverse, n - 1)
    }

    val ret = Array.fill(powOfTwo(elems))("")
    val line = for (i <- 0 until elems)
      yield makeLine(zeroAndOne(powOfTwo(elems - i - 1)), powOfTwo(i))
    for (str <- line; i <- 0 until str.size) ret(i) += str(i).toString()
    ret.toList
  }

  // 入力から出力を取得
  def get(input: String) = kmap.getOrElse(input, "0")

  // 入力に対する出力の選択状態を変更
  def select(input: String) { selectMap += input -> !selectMap.getOrElse(input, false) }

  // 全ての選択状態を解除
  def resetSelect() { selectMap.keys.foreach(selectMap += _ -> false) }

  // 入力に対する出力が選択されていればtrue
  def isSelected(input: String) = selectMap.getOrElse(input, false)

  // 囲んだ集合を取得
  def getCircleSet(): HashSet[List[String]] = circleSet.clone

  // 囲んだ集合に追加
  def encircle() { circleSet += (selectMap.keys.filter(isSelected) toList) }

  // 囲んだ集合を初期化
  def resetCircle() { circleSet.clear() }

  // 選択されている数が2^nになっているかどうか
  def isPowOfTwo(n: Int = selectCells.size): Boolean = { ((0 to inputN) :\ false)(n == powOfTwo(_) || _) }

  // 選択されている値に0を含んでいるかどうか
  def containsZero() = ((kmap.filterKeys(isSelected)).values toList).contains("0")

  // 選択されているセルたち
  def selectCells = ((kmap.filterKeys(isSelected).keys) toList).sorted;

  // 選択されているセルを既に囲んでいるか
  def existCircle(): Boolean = (false /: circleSet)(_ || _.sorted == selectCells)

  // 選択したリストを囲んだ集合から消す
  def deleteCircle() { circleSet.remove((circleSet.filter(_.sorted == selectCells) toList)(0)) }

  // 囲むセルたちが隣り合っているかを調べる、数は考慮されない
  def isAdjacent(searchList: Iterable[String] = selectMap.keys.filter(isSelected)): Boolean = {
    val list = searchList toBuffer // 選択されているセルの集合
    val (c, r) = list.remove(0).splitAt(col) // 開始セルを適当に決定
    // あるセルと繋がってる行たちを取得
    def selectNTR(c: String, r: String): List[String] = {
      val ntr = nextToList(rawlist, r) // 自分の隣になっている行たちを取得
      val ntrs = ntr filter (r => list.contains(c + r)) // その行の中で選択されているものを取得
      for (r <- ntrs) list.remObj(c + r) // 選択されている行をリストから削除
      // 選択されている行が隣に無かったら自身のみを、そうでなければ隣の行も調べて取得
      List(r) ::: (if (ntrs.isEmpty) Nil else for (rr <- ntrs; r <- selectNTR(c, rr)) yield r)
    }

    val ntrs = selectNTR(c, r)

    // ある列と隣あっているセルを消していく、行も複数適用。
    // 全てを消しきれたらtrue、残りがあったらfalse
    def remNt(col: String, raws: List[String]): Boolean = {
      for (nt <- nextToList(collist, col)) { // 隣あっている列を全て調べる
        val rs = for (r <- raws if list.contains(nt + r)) yield list.remObj(nt + r) // リストに含まれている列をリストから消して取得
        if (!rs.isEmpty && (rs.size != raws.size || !remNt(nt, raws))) return false // 隣り合っているものがあるが、調べてる行の数と違ったらダメ、隣り合ってるものがあったらその列に対しても同じことを適用
      }
      true
    }
    remNt(c, ntrs) && list.isEmpty
  }

  // 自動で囲む
  def autoSurround() {
    val oneList = kmap collect { case s if s._2 == "1" => s._1 } toBuffer
    val tol = kmap collect { case s if s._2 == "1" => s._1 }
    val dcList = kmap collect { case s if (s._2 != "0" && s._2 != "1") => s._1 } toList
    val allList = tol ++ dcList toBuffer;
    if (oneList.isEmpty) return

    // 1を囲み終えるまで囲み続ける
    def making() {
      val grp = searchGroup(powOfTwo(inputN))
      grp foreach (x => if (oneList.contains(x)) oneList.remObj(x))
      circleSet += grp
      if (!oneList.isEmpty) making()
    }

    // 囲む塊を求める、ゴリ押し
    def searchGroup(n: Int): List[String] = n match {
      case 1                     => List(oneList.head)
      case n if allList.size < n => searchGroup(n / 2)
      case n =>
        val res = allList combinations (n) collect { case v if isAdjacent(v) => v } toList;
        if (res.isEmpty || !containOne(res)) searchGroup(n / 2);
        else res.maxBy(xs => xs.filter(oneList.contains).size) toList
    }

    // 1を１つでも含んでいるか(ドントケアだけは意味なし！)
    def containOne(xs: List[Buffer[String]]): Boolean = {
      for (ys <- xs if !(ys filter (oneList.contains) isEmpty)) return true
      false
    }

    making()
    // 無駄に1を囲んでいる円を消す(その円が無くても他の円で十分な場合)
    val duplSet = for (sx <- circleSet) yield {
      val nx = for (x <- sx; tx <- circleSet if (sx != tx && tx.contains(x))) yield x
      if (sx.sorted == nx.sorted) sx else Nil
    }
    duplSet foreach (x => if (circleSet.contains(x)) circleSet -= x)

  }

  // 隣あってるリストを取得
  def nextToList(list: List[String], in: String) = list.filter(diffCount(in, _) == 1)

  // ２つの文字列を比較して違う文字のある数をカウント
  def diffCount(s1: String, s2: String) = (s1 zip s2 filter (tp => tp._1 != tp._2)) size

}