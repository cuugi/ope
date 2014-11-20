import java.io.{FileInputStream, File}
import java.util.Properties
import scala.collection.JavaConverters._
import scala.io.Source._
import scala.util.Random
import scala.collection.immutable.ListMap

object Sanakoe {

  def main(parametrit: Array[String]) = {
    val sanastot = lataaSanastot("sanastot").reverse

    if (parametrit.isEmpty) {
      println("Valitse sanasto: ")
      for (sanasto <- sanastot) println(sanastot.indexOf(sanasto) + 1 + ") " + sanasto.nimi)
    } else {
      val sanasto = sanastot(parametrit(0).toInt - 1)
      val (oikein, yhteensa) = kysele(sanasto.sekoitaJaLyhennä(10))
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

  def kysele(sanasto: Sanasto) = {
    var oikein = 0

    for ((en, fi) <- sanasto.sanat) {
      if (kysy(en, fi)) {
        println("-> Oikein!")
        oikein = oikein + 1;
      } else {
        println("-> Väärin meni! Oikea vastaus oli '" + en + "'")
      }
    }

    (oikein, sanasto.sanat.size)
  }

  def kysy(en: String, fi: String) = {
    println
    println("Mitä on englanniksi '" + fi + "'?")
    print("# ")
    (readLine.trim == en)
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