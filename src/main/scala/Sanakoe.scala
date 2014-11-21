import java.io.File

import scala.collection.immutable.ListMap
import scala.io.Source._
import scala.util.Random

object Sanakoe {

  def main(parametrit: Array[String]) = {
    val sanastot = lataaSanastot("sanastot").reverse

    if (parametrit.isEmpty) {
      println("Valitse sanasto: ")
      for (sanasto <- sanastot) println(sanastot.indexOf(sanasto) + 1 + ") " + sanasto.nimi)
    } else {
      val sanasto = sanastot(parametrit(0).toInt - 1)
      val (oikein, yhteensa) = kyseleEnglanniksi(sanasto.sekoitaJaLyhennä(10))
      println("Sait " + oikein + "/" + yhteensa + " oikein!")
    }
  }

  def lataaSanastot(hakemisto: String): List[Sanasto] = {
    var sanastot = List[Sanasto]()
    for (tiedosto <- new File(hakemisto).listFiles) {
      sanastot = lataaSanasto(tiedosto) :: sanastot
    }
    sanastot
  }

  def kyseleEnglanniksi(sanasto: Sanasto) = {
    var oikein = 0

    for ((en, fi) <- sanasto.sanat) {
      println {
        kysyEnglanniksi(en, fi, vertaa) match
        {
          case Oikein() => {
            oikein = oikein + 1
            "Oikein!"
          }
          case Miinus() => "Melkein oikein."
          case _ => "Ei aivan! Oikea vastaus oli `" + en + "`."
        }
      }
    }

    (oikein, sanasto.sanat.size)
  }

  def vertaa(oikein: String, vastaus: String): Tulos =
    vastaus match {
      case `oikein` => Oikein()
      case _ => Väärin()
    }

  def kysyEnglanniksi(en: String, fi: String, vertaa: (String, String) => Tulos) = {
    println
    println("Mitä on englanniksi '" + fi + "'?")
    print("# ")
    vertaa(readLine.trim, en)
  }

  def lataaSanasto(tiedosto: File) =
    new Sanasto(tiedosto.getName, {
      val rivit = fromFile(tiedosto.getCanonicalPath).getLines
      var parit = ListMap[String, String]()
      for (rivi <- rivit) {
        val (en, fi) = rivi.splitAt(rivi.indexOf('='))
        parit += (en -> fi.tail)
      }
      parit
    })

}

class Sanasto(n: String, s: ListMap[String, String]) {
  require(n != null)
  require(s != null)

  val arpa = new Random

  val nimi = n
  val sanat = s

  def sekoitaJaLyhennä(sanoja: Int): Sanasto = {
    var uusi = ListMap[String, String]()
    var vanha = sanat.keys.toList
    while (!vanha.isEmpty && uusi.size < sanoja) {
      val indeksi = arpa.nextInt(vanha.size)
      val en = vanha(indeksi)
      val fi = sanat(en)
      uusi += (en -> fi)
      vanha = poista(en, vanha)
    }
    new Sanasto(nimi, uusi)
  }

  def poista(indeksi: String, lista: List[String]) = lista diff List(indeksi)
}

sealed abstract class Tulos

case class Oikein extends Tulos

case class Miinus extends Tulos

case class Väärin extends Tulos