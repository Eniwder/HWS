

package main.scala

import scala.swing.ComboBox
import scala.collection.mutable.Buffer

trait OpUtil {
  // 二のn乗を求める
  def powOfTwo(n: Int): Int = {
    if (n <= 0) 1
    else 2 * powOfTwo(n - 1)
  }

  // log2 n
  def log2(n: Int): Int = {
    if (n == 1) 0
    else 1 + log2(n / 2)
  }

  // タプルを入れ替える
  def flipTuple[A, B](ab: (A, B)): (B, A) = (ab._2, ab._1)

  // 真理値表のある列を求める
  def makeTTcol(line: String, count: Int): String = {
    if (count == 0) ""
    else line + makeTTcol(line, count - 1)
  }

  // n^2個の0と1の文字列を作成
  def zeroAndOne(n: Int) = { "0" * n + "1" * n }

  // ----- implicitクラスここから -----
  // ComboBoxの値をIntに変換する
  implicit class MyInt2[_](val cmb: ComboBox[_]) {
    def decode = cmb.peer.getSelectedItem().toString().toInt
  }

  // Bufferからオブジェクトを削除する
  implicit class MyBuf[T](val buf: Buffer[T]) {
    def remObj(obj: T) = buf.remove(buf.indexOf(obj))
  }

  // 配列から値を取得、取得できない場合(index<0 || xs.size<=index)は""を返す
  implicit class MyArray(val arr: Array[String]) {
    def getOrBlank(idx: Int): String = if (0 < idx && idx < arr.size) arr(idx) else ""
  }

  //指定文字で区切る、その文字は含まない
  implicit class MyString[T](val str: String) {
    def splitChar(c: Char): (String, String) = {
      val (in, to) = str.splitAt(str.indexOf(c))
      (in, to.tail)
    }
  }
  // ----- implicitクラスここまで -----
  
  // ローンパターン
  def using[A <% { def close(): Unit }](s: A)(f: A => Any) {
    try f(s)
    //  catch { case e: Exception => Dialog.showMessage(title = "例外発生", message = e + "\nファイルの読み込み・書き込みに失敗しました。") }
    finally s.close()
  }

}